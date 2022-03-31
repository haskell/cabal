{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Utilities to help commands with scripts
--
module Distribution.Client.ScriptUtils (
    getScriptCacheDirectoryRoot, getScriptHash, getScriptCacheDirectory, ensureScriptCacheDirectory,
    withContextAndSelectors, AcceptNoTargets(..), TargetContext(..),
    updateContextAndWriteProjectFile, updateContextAndWriteProjectFile',
    fakeProjectSourcePackage, lSrcpkgDescription
  ) where

import Prelude ()
import Distribution.Client.Compat.Prelude hiding (toList)

import Distribution.Compat.Lens
import qualified Distribution.Types.Lens as L

import Distribution.CabalSpecVersion
    ( CabalSpecVersion (..), cabalSpecLatest)
import Distribution.Client.ProjectOrchestration
import Distribution.Client.Config
    ( getCabalDir )
import Distribution.Client.DistDirLayout
    ( DistDirLayout(..) )
import Distribution.Client.HashValue
    ( hashValue, showHashValue )
import Distribution.Client.HttpUtils
         ( HttpTransport, configureTransport )
import Distribution.Client.NixStyleOptions
    ( NixStyleFlags (..) )
import Distribution.Client.ProjectConfig
    ( ProjectConfig(..), ProjectConfigShared(..)
    , reportParseResult, withProjectOrGlobalConfig
    , projectConfigHttpTransport )
import Distribution.Client.ProjectConfig.Legacy
    ( ProjectConfigSkeleton
    , parseProjectSkeleton, instantiateProjectConfigSkeleton )
import Distribution.Client.ProjectFlags
    ( flagIgnoreProject )
import Distribution.Client.RebuildMonad
    ( runRebuild )
import Distribution.Client.Setup
    ( ConfigFlags(..), GlobalFlags(..) )
import Distribution.Client.TargetSelector
    ( TargetSelectorProblem(..), TargetString(..) )
import Distribution.Client.Types
    ( PackageLocation(..), PackageSpecifier(..), UnresolvedSourcePackage )
import Distribution.FieldGrammar
    ( parseFieldGrammar, takeFields )
import Distribution.Fields
    ( ParseResult, parseFatalFailure, readFields )
import Distribution.PackageDescription
    ( ignoreConditions )
import Distribution.PackageDescription.FieldGrammar
    ( executableFieldGrammar )
import Distribution.PackageDescription.PrettyPrint
    ( showGenericPackageDescription )
import Distribution.Parsec
    ( Position(..) )
import Distribution.Simple.Flag
    ( fromFlagOrDefault, flagToMaybe )
import Distribution.Simple.PackageDescription
    ( parseString )
import Distribution.Simple.Setup
    ( Flag(..) )
import Distribution.Simple.Compiler
    ( compilerInfo )
import Distribution.Simple.Utils
    ( createDirectoryIfMissingVerbose, createTempDirectory, die', handleDoesNotExist, readUTF8File, warn, writeUTF8File )
import qualified Distribution.SPDX.License as SPDX
import Distribution.Solver.Types.SourcePackage as SP
    ( SourcePackage(..) )
import Distribution.System
    ( Platform(..) )
import Distribution.Types.BuildInfo
    ( BuildInfo(..) )
import Distribution.Types.CondTree
    ( CondTree(..) )
import Distribution.Types.Executable
    ( Executable(..) )
import Distribution.Types.GenericPackageDescription as GPD
    ( GenericPackageDescription(..), emptyGenericPackageDescription )
import Distribution.Types.PackageDescription
    ( PackageDescription(..), emptyPackageDescription )
import Distribution.Types.PackageName.Magic
    ( fakePackageCabalFileName, fakePackageId )
import Distribution.Utils.NubList
    ( fromNubList )
import Distribution.Client.ProjectPlanning
    ( configureCompiler )
import Distribution.Verbosity
    ( normal )
import Language.Haskell.Extension
    ( Language(..) )

import Control.Concurrent.MVar
    ( newEmptyMVar, putMVar, tryTakeMVar )
import Control.Exception
    ( bracket )
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Lazy ()
import qualified Data.Set as S
import System.Directory
    ( canonicalizePath, doesFileExist, getTemporaryDirectory, removeDirectoryRecursive )
import System.FilePath
    ( (</>), takeFileName )
import qualified Text.Parsec as P

-- A note on multi-module script support #6787:
-- Multi-module scripts are not supported and support is non-trivial.
-- What you want to do is pass the absolute path to the script's directory in hs-source-dirs,
-- but hs-source-dirs only accepts relative paths. This leaves you with several options none
-- of which are particularly appealing.
-- 1) Loosen the requirement that hs-source-dirs take relative paths
-- 2) Add a field to BuildInfo that acts like an hs-source-dir, but accepts an absolute path
-- 3) Use a path relative to the project root in hs-source-dirs, and pass extra flags to the
--    repl to deal with the fact that the repl is relative to the working directory and not
--    the project root.

-- | Get the directory where script builds are cached.
--
-- @CABAL_DIR\/script-builds\/@
getScriptCacheDirectoryRoot :: IO FilePath
getScriptCacheDirectoryRoot = do
  cabalDir <- getCabalDir
  return $ cabalDir </> "script-builds"

-- | Get the hash of a script's absolute path)
--
-- Two hashes will be the same as long as the absolute paths
-- are the same.
getScriptHash :: FilePath -> IO String
getScriptHash script = showHashValue . hashValue . fromString <$> canonicalizePath script

-- | Get the directory for caching a script build.
--
-- The only identity of a script is it's absolute path, so append the
-- hashed path to @CABAL_DIR\/script-builds\/@ to get the cache directory.
getScriptCacheDirectory :: FilePath -> IO FilePath
getScriptCacheDirectory script = (</>) <$> getScriptCacheDirectoryRoot <*> getScriptHash script

-- | Get the directory for caching a script build and ensure it exists.
--
-- The only identity of a script is it's absolute path, so append the
-- hashed path to @CABAL_DIR\/script-builds\/@ to get the cache directory.
ensureScriptCacheDirectory :: Verbosity -> FilePath -> IO FilePath
ensureScriptCacheDirectory verbosity script = do
  cacheDir <- getScriptCacheDirectory script
  createDirectoryIfMissingVerbose verbosity True cacheDir
  return cacheDir

-- | What your command should do when no targets are found.
data AcceptNoTargets
  = RejectNoTargets -- ^ die on 'TargetSelectorNoTargetsInProject'
  | AcceptNoTargets -- ^ return a default 'TargetSelector'
  deriving (Eq, Show)

-- | Information about the context in which we found the 'TargetSelector's.
data TargetContext
  = ProjectContext -- ^ The target selectors are part of a project.
  | GlobalContext  -- ^ The target selectors are from the global context.
  | ScriptContext FilePath Executable
  -- ^ The target selectors refer to a script. Contains the path to the script and
  -- the executable metadata parsed from the script
  deriving (Eq, Show)

-- | Determine whether the targets represent regular targets or a script
-- and return the proper context and target selectors.
-- Die with an error message if selectors are valid as neither regular targets or as a script.
--
-- In the case that the context refers to a temporary directory,
-- delete it after the action finishes.
withContextAndSelectors
  :: AcceptNoTargets     -- ^ What your command should do when no targets are found.
  -> Maybe ComponentKind -- ^ A target filter
  -> NixStyleFlags a     -- ^ Command line flags
  -> [String]            -- ^ Target strings or a script and args.
  -> GlobalFlags         -- ^ Global flags.
  -> (TargetContext -> ProjectBaseContext -> [TargetSelector] -> IO b)
  -- ^ The body of your command action.
  -> IO b
withContextAndSelectors noTargets kind flags@NixStyleFlags {..} targetStrings globalFlags act
  = withTemporaryTempDirectory $ \mkTmpDir -> do
    (tc, ctx) <- withProjectOrGlobalConfig verbosity ignoreProject globalConfigFlag with (without mkTmpDir)

    (tc', ctx', sels) <- case targetStrings of
      -- Only script targets may contain spaces and or end with ':'.
      -- Trying to readTargetSelectors such a target leads to a parse error.
      [target] | any (\c -> isSpace c) target || ":" `isSuffixOf` target -> do
          scriptOrError target [TargetSelectorNoScript $ TargetString1 target]
      _                                                   -> do
        -- In the case where a selector is both a valid target and script, assume it is a target,
        -- because you can disambiguate the script with "./script"
        readTargetSelectors (localPackages ctx) kind targetStrings >>= \case
          Left err@(TargetSelectorNoTargetsInProject:_)
            | [] <- targetStrings
            , AcceptNoTargets <- noTargets -> return (tc, ctx, defaultTarget)
            | (script:_) <- targetStrings  -> scriptOrError script err
          Left err@(TargetSelectorNoSuch t _:_)
            | TargetString1 script <- t    -> scriptOrError script err
          Left err@(TargetSelectorExpected t _ _:_)
            | TargetString1 script <- t    -> scriptOrError script err
          Left err@(MatchingInternalError _ _ _:_) -- Handle ':' in middle of script name.
            | [script] <- targetStrings    -> scriptOrError script err
          Left err                         -> reportTargetSelectorProblems verbosity err
          Right sels                       -> return (tc, ctx, sels)

    act tc' ctx' sels
  where
    verbosity = fromFlagOrDefault normal (configVerbosity configFlags)
    ignoreProject = flagIgnoreProject projectFlags
    cliConfig = commandLineFlagsToProjectConfig globalFlags flags mempty
    globalConfigFlag = projectConfigConfigFile (projectConfigShared cliConfig)
    defaultTarget = [TargetPackage TargetExplicitNamed [fakePackageId] Nothing]

    with = do
      ctx <- establishProjectBaseContext verbosity cliConfig OtherCommand
      return (ProjectContext, ctx)
    without mkDir globalConfig = do
      distDirLayout <- establishDummyDistDirLayout verbosity (globalConfig <> cliConfig) =<< mkDir
      ctx <- establishDummyProjectBaseContext verbosity (globalConfig <> cliConfig) distDirLayout [] OtherCommand
      return (GlobalContext, ctx)
    scriptOrError script err = do
      exists <- doesFileExist script
      if exists then do
        -- In the script case we always want a dummy context even when ignoreProject is False
        let mkCacheDir = ensureScriptCacheDirectory verbosity script
        (_, ctx) <- withProjectOrGlobalConfig verbosity (Flag True) globalConfigFlag with (without mkCacheDir)

        let projectRoot = distProjectRootDirectory $ distDirLayout ctx
        writeFile (projectRoot </> "scriptlocation") =<< canonicalizePath script

        scriptContents <- BS.readFile script
        executable     <- readExecutableBlockFromScript verbosity scriptContents


        httpTransport <- configureTransport verbosity
                     (fromNubList . projectConfigProgPathExtra $ projectConfigShared cliConfig)
                     (flagToMaybe . projectConfigHttpTransport $ projectConfigBuildOnly cliConfig)

        projectCfgSkeleton <- readProjectBlockFromScript verbosity httpTransport (distDirLayout ctx) (takeFileName script) scriptContents

        (compiler, Platform arch os, _) <- runRebuild (distProjectRootDirectory . distDirLayout $ ctx) $ configureCompiler verbosity (distDirLayout ctx) ((fst $ ignoreConditions projectCfgSkeleton) <> projectConfig ctx)

        let projectCfg = instantiateProjectConfigSkeleton os arch (compilerInfo compiler) mempty projectCfgSkeleton :: ProjectConfig

        let executable' = executable & L.buildInfo . L.defaultLanguage %~ maybe (Just Haskell2010) Just
            ctx'        = ctx & lProjectConfig %~ (<> projectCfg)
        return (ScriptContext script executable', ctx', defaultTarget)
      else reportTargetSelectorProblems verbosity err

withTemporaryTempDirectory :: (IO FilePath -> IO a) -> IO a
withTemporaryTempDirectory act = newEmptyMVar >>= \m -> bracket (getMkTmp m) (rmTmp m) act
  where
    -- We return an (IO Filepath) instead of a FilePath for two reasons:
    -- 1) To give the consumer the discretion to not create the tmpDir,
    --    but still grantee that it's deleted if they do create it
    -- 2) Because the path returned by createTempDirectory is not predicable
    getMkTmp m = return $ do
      tmpDir <- getTemporaryDirectory >>= flip createTempDirectory "cabal-repl."
      putMVar m tmpDir
      return tmpDir
    rmTmp m _ = tryTakeMVar m >>= maybe (return ()) (handleDoesNotExist () . removeDirectoryRecursive)

-- | Add the 'SourcePackage' to the context and use it to write a .cabal file.
updateContextAndWriteProjectFile' :: ProjectBaseContext -> SourcePackage (PackageLocation (Maybe FilePath)) -> IO ProjectBaseContext
updateContextAndWriteProjectFile' ctx srcPkg = do
  let projectRoot      = distProjectRootDirectory $ distDirLayout ctx
      packageFile      = projectRoot </> fakePackageCabalFileName
      contents         = showGenericPackageDescription (srcpkgDescription srcPkg)
      writePackageFile = writeUTF8File packageFile contents
  -- TODO This is here to prevent reconfiguration of cached repl packages.
  -- It's worth investigating why it's needed in the first place.
  packageFileExists <- doesFileExist packageFile
  if packageFileExists then do
    cached <- force <$> readUTF8File packageFile
    when (cached /= contents)
      writePackageFile
  else writePackageFile
  return (ctx & lLocalPackages %~ (++ [SpecificSourcePackage srcPkg]))

-- | Add add the executable metadata to the context and write a .cabal file.
updateContextAndWriteProjectFile :: ProjectBaseContext -> FilePath -> Executable -> IO ProjectBaseContext
updateContextAndWriteProjectFile ctx scriptPath scriptExecutable = do
  let projectRoot = distProjectRootDirectory $ distDirLayout ctx

  absScript <- canonicalizePath scriptPath
  let
    -- Replace characters which aren't allowed in the executable component name with '_'
    -- Prefix with "cabal-script-" to make it clear to end users that the name may be mangled
    scriptExeName = "cabal-script-" ++ map censor (takeFileName scriptPath)
    censor c | c `S.member` ccNamecore = c
             | otherwise               = '_'

    sourcePackage = fakeProjectSourcePackage projectRoot
      & lSrcpkgDescription . L.condExecutables
      .~ [(fromString scriptExeName, CondNode executable (targetBuildDepends $ buildInfo executable) [])]
    executable = scriptExecutable
      & L.modulePath .~ absScript

  updateContextAndWriteProjectFile' ctx sourcePackage

parseScriptBlock :: BS.ByteString -> ParseResult Executable
parseScriptBlock str =
    case readFields str of
        Right fs -> do
            let (fields, _) = takeFields fs
            parseFieldGrammar cabalSpecLatest fields (executableFieldGrammar "script")
        Left perr -> parseFatalFailure pos (show perr) where
            ppos = P.errorPos perr
            pos  = Position (P.sourceLine ppos) (P.sourceColumn ppos)

readScriptBlock :: Verbosity -> BS.ByteString -> IO Executable
readScriptBlock verbosity = parseString parseScriptBlock verbosity "script block"

-- | Extract the first encountered executable metadata block started and
-- terminated by the below tokens or die.
--
-- * @{- cabal:@
--
-- * @-}@
--
-- Return the metadata.
readExecutableBlockFromScript :: Verbosity -> BS.ByteString -> IO Executable
readExecutableBlockFromScript verbosity str = do
    str' <- case extractScriptBlock "cabal" str of
              Left e -> die' verbosity $ "Failed extracting script block: " ++ e
              Right x -> return x
    when (BS.all isSpace str') $ warn verbosity "Empty script block"
    readScriptBlock verbosity str'

-- | Extract the first encountered project metadata block started and
-- terminated by the below tokens.
--
-- * @{- project:@
--
-- * @-}@
--
-- Return the metadata.
readProjectBlockFromScript :: Verbosity -> HttpTransport -> DistDirLayout -> String -> BS.ByteString -> IO ProjectConfigSkeleton
readProjectBlockFromScript verbosity httpTransport DistDirLayout{distDownloadSrcDirectory} scriptName str = do
    case extractScriptBlock "project" str of
        Left  _ -> return mempty
        Right x ->    reportParseResult verbosity "script" scriptName
                  =<< parseProjectSkeleton distDownloadSrcDirectory httpTransport verbosity [] scriptName x

-- | Extract the first encountered script metadata block started end
-- terminated by the tokens
--
-- * @{- <header>:@
--
-- * @-}@
--
-- appearing alone on lines (while tolerating trailing whitespace).
-- These tokens are not part of the 'Right' result.
--
-- In case of missing or unterminated blocks a 'Left'-error is
-- returned.
extractScriptBlock :: BS.ByteString -> BS.ByteString -> Either String BS.ByteString
extractScriptBlock header str = goPre (BS.lines str)
  where
    isStartMarker = (== startMarker) . stripTrailSpace
    isEndMarker   = (== endMarker) . stripTrailSpace

    stripTrailSpace = fst . BS.spanEnd isSpace

    -- before start marker
    goPre ls = case dropWhile (not . isStartMarker) ls of
                 [] -> Left $ "`" ++ BS.unpack startMarker ++ "` start marker not found"
                 (_:ls') -> goBody [] ls'

    goBody _ [] = Left $ "`" ++ BS.unpack endMarker ++ "` end marker not found"
    goBody acc (l:ls)
      | isEndMarker l = Right $! BS.unlines $ reverse acc
      | otherwise     = goBody (l:acc) ls

    startMarker, endMarker :: BS.ByteString
    startMarker = "{- " <> header <> ":"
    endMarker   = "-}"

-- | The base for making a 'SourcePackage' for a fake project.
-- It needs a 'Distribution.Types.Library.Library' or 'Executable' depending on the command.
fakeProjectSourcePackage :: FilePath -> SourcePackage (PackageLocation loc)
fakeProjectSourcePackage projectRoot = sourcePackage
  where
    sourcePackage = SourcePackage
      { srcpkgPackageId     = fakePackageId
      , srcpkgDescription   = genericPackageDescription
      , srcpkgSource        = LocalUnpackedPackage projectRoot
      , srcpkgDescrOverride = Nothing
      }
    genericPackageDescription = emptyGenericPackageDescription
      { GPD.packageDescription = packageDescription }
    packageDescription = emptyPackageDescription
      { package = fakePackageId
      , specVersion = CabalSpecV2_2
      , licenseRaw = Left SPDX.NONE
      }

-- Lenses
-- | A lens for the 'srcpkgDescription' field of 'SourcePackage'
lSrcpkgDescription :: Lens' (SourcePackage loc) GenericPackageDescription
lSrcpkgDescription f s = fmap (\x -> s { srcpkgDescription = x }) (f (srcpkgDescription s))
{-# inline lSrcpkgDescription #-}

lLocalPackages :: Lens' ProjectBaseContext [PackageSpecifier UnresolvedSourcePackage]
lLocalPackages f s = fmap (\x -> s { localPackages = x }) (f (localPackages s))
{-# inline lLocalPackages #-}

lProjectConfig :: Lens' ProjectBaseContext ProjectConfig
lProjectConfig f s = fmap (\x -> s { projectConfig = x }) (f (projectConfig s))
{-# inline lProjectConfig #-}

-- Character classes
-- Transcribed from "templates/Lexer.x"
ccSpace, ccCtrlchar, ccPrintable, ccSymbol', ccParen, ccNamecore :: Set Char
ccSpace     = S.fromList " "
ccCtrlchar  = S.fromList $ [chr 0x0 .. chr 0x1f] ++ [chr 0x7f]
ccPrintable = S.fromList [chr 0x0 .. chr 0xff] S.\\ ccCtrlchar
ccSymbol'   = S.fromList ",=<>+*&|!$%^@#?/\\~"
ccParen     = S.fromList "()[]"
ccNamecore  = ccPrintable S.\\ S.unions [ccSpace, S.fromList ":\"{}", ccParen, ccSymbol']
