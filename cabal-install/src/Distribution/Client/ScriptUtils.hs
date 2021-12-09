{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Utilities to help commands with scripts
--
module Distribution.Client.ScriptUtils (
    getScriptCacheDirectoryRoot, getScriptHash, getScriptCacheDirectory, ensureScriptCacheDirectory,
    withContextAndSelectors, AcceptNoTargets(..), TargetContext(..),
    updateContextAndWriteProjectFile, updateContextAndWriteScriptProjectFiles,
    fakeProjectSourcePackage, lSrcpkgDescription
  ) where

import Prelude ()
import Distribution.Client.Compat.Prelude hiding (toList)

import Distribution.Compat.Lens
import qualified Distribution.Types.Lens as L

import Distribution.Client.ProjectOrchestration
import Distribution.Client.DistDirLayout
    ( DistDirLayout(..) )
import Distribution.Client.NixStyleOptions
    ( NixStyleFlags (..) )
import Distribution.Client.Setup
    ( GlobalFlags(..), ConfigFlags(..) )
import Distribution.Client.Config
    ( getCabalDir )
import Distribution.Simple.Flag
    ( fromFlagOrDefault )
import Distribution.Simple.Setup
    ( Flag(..) )
import Distribution.CabalSpecVersion
    (CabalSpecVersion (..), cabalSpecLatest)
import Distribution.Verbosity
    ( normal )
import Distribution.Simple.Utils
    ( warn, die', createDirectoryIfMissingVerbose
    , createTempDirectory, handleDoesNotExist )
import Distribution.Client.ProjectConfig
    ( ProjectConfig(..), ProjectConfigShared(..)
    , withProjectOrGlobalConfig )
import Distribution.Client.ProjectFlags
    ( flagIgnoreProject )
import Distribution.Client.TargetSelector
    ( TargetSelectorProblem(..), TargetString(..) )
import Distribution.Client.Types
    ( PackageLocation(..), PackageSpecifier(..), UnresolvedSourcePackage )
import Distribution.FieldGrammar
    ( takeFields, parseFieldGrammar )
import Distribution.PackageDescription.FieldGrammar
    ( executableFieldGrammar )
import Distribution.PackageDescription.PrettyPrint
    ( writeGenericPackageDescription, showGenericPackageDescription )
import Distribution.Parsec
    ( Position(..) )
import Distribution.Fields
    ( ParseResult, parseString, parseFatalFailure, readFields )
import qualified Distribution.SPDX.License as SPDX
import Distribution.Solver.Types.SourcePackage as SP
    ( SourcePackage(..) )
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
    ( fakePackageId )
import Language.Haskell.Extension
    ( Language(..) )
import Distribution.Client.HashValue
    ( hashValue, showHashValue )
import Distribution.Simple.Utils
    ( readUTF8File )

import Control.Concurrent.MVar
    ( newEmptyMVar, putMVar, tryTakeMVar )
import Control.Exception
    ( bracket )
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Lazy ()
import qualified Text.Parsec as P
import System.Directory
    ( getTemporaryDirectory, removeDirectoryRecursive, doesFileExist, canonicalizePath )
import System.FilePath
    ( (</>), takeExtension )


-- | Get the directory where script builds are cached.
--
-- @CABAL_DIR\/script-builds\/@
getScriptCacheDirectoryRoot :: IO FilePath
getScriptCacheDirectoryRoot = do
  cabalDir <- getCabalDir
  return $ cabalDir </> "script-builds"

-- | Get the hash of (@prefix@ ++ the script's absolute path)
--
-- Two hashes for the same @prefix@ will be the same whenever
-- the absolute path is the same. Two hashes with different
-- @prefix@ will always differ.
--
-- @prefix@ must not contain path separator characters because
-- it could cause unwanted collisions.
getScriptHash :: String -> FilePath -> IO String
getScriptHash prefix script
  | '/' `notElem` prefix && '\\' `notElem` prefix = showHashValue . hashValue . fromString . (prefix ++) <$> canonicalizePath script
  | otherwise                                     = error "getScriptHash: prefix must not contain '/' or '\\'"

-- | Get the directory for caching a script build.
--
-- The only identity of a script is it's absolute path, so append the
-- hashed path to @CABAL_DIR\/script-builds\/@ to get the cache directory.
--
-- @prefix@ must not contain path separator characters.
getScriptCacheDirectory :: String -> FilePath -> IO FilePath
getScriptCacheDirectory prefix script = (</>) <$> getScriptCacheDirectoryRoot <*> getScriptHash prefix script

-- | Get the directory for caching a script build and ensure it exists.
--
-- The only identity of a script is it's absolute path, so append the
-- hashed path to @CABAL_DIR\/script-builds\/@ to get the cache directory.
--
-- @prefix@ must not contain path separator characters.
ensureScriptCacheDirectory :: Verbosity -> String -> FilePath -> IO FilePath
ensureScriptCacheDirectory verbosity prefix script = do
  cacheDir <- getScriptCacheDirectory prefix script
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
  | ScriptContext { scriptPath :: FilePath, scriptExecutable :: Executable, scriptContents :: BS.ByteString }
  -- ^ The target selectors refer to a script.
  deriving (Eq, Show)

-- | Determine whether the targets represent regular targets or a script
-- and return the proper context and target selectors.
-- Die with an error message if selectors are valid as neither regular targets or as a script.
--
-- In the case that the context refers to a temporary directory,
-- delete it after the action finishes.
withContextAndSelectors
  :: AcceptNoTargets     -- ^ What your command should do when no targets are found.
  -> String              -- ^ A prefix to add to the path before hashing, if you don't want to use the default cache dir.
  -> Maybe ComponentKind -- ^ A target filter
  -> NixStyleFlags a     -- ^ Command line flags
  -> [String]            -- ^ Target strings or a script and args.
  -> GlobalFlags         -- ^ Global flags.
  -> (TargetContext -> ProjectBaseContext -> [TargetSelector] -> IO b)
  -- ^ The body of your command action.
  -> IO b
withContextAndSelectors noTargets cachePrefix kind flags@NixStyleFlags {..} targetStrings globalFlags act
  = withTemporaryTempDirectory $ \mkTmpDir -> do
    (tc, ctx) <- withProjectOrGlobalConfig verbosity ignoreProject globalConfigFlag with (without mkTmpDir)

    -- In the case where a selector is both a valid target and script, assume it is a target,
    -- because you can disambiguate the script with "./script"
    let truncateExe = if kind == Just ExeKind then take 1 else id
    (tc', ctx', sels) <- readTargetSelectors (localPackages ctx) kind (truncateExe targetStrings) >>= \case
      Left err@(TargetSelectorNoTargetsInProject:_)
        | [] <- targetStrings
        , AcceptNoTargets <- noTargets -> return (tc, ctx, defaultTarget)
        | (script:_) <- targetStrings  -> scriptOrError script err
      Left err@(TargetSelectorNoSuch t _:_)
        | TargetString1 script <- t    -> scriptOrError script err
      Left err@(TargetSelectorExpected t _ _:_)
        | TargetString1 script <- t    -> scriptOrError script err
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
        let mkCacheDir = ensureScriptCacheDirectory verbosity cachePrefix script
        (_, ctx) <- withProjectOrGlobalConfig verbosity (Flag True) globalConfigFlag with (without mkCacheDir)

        let projectRoot = distProjectRootDirectory $ distDirLayout ctx
        writeFile (projectRoot </> "scriptlocation") =<< canonicalizePath script

        (executable, contents) <- readScriptBlockFromScript verbosity =<< BS.readFile script

        let executable' = executable & L.buildInfo . L.defaultLanguage %~ maybe (Just Haskell2010) Just
        return (ScriptContext script executable' contents, ctx, defaultTarget)
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

-- | Add the 'SourcePackage' to the context and use it to write a fake-package.cabal file.
updateContextAndWriteProjectFile :: ProjectBaseContext -> SourcePackage (PackageLocation (Maybe FilePath)) -> IO ProjectBaseContext
updateContextAndWriteProjectFile ctx srcPkg = do
  let projectRoot = distProjectRootDirectory $ distDirLayout ctx
      projectFile = projectRoot </> "fake-package.cabal"
      writeProjectFile = writeGenericPackageDescription (projectRoot </> "fake-package.cabal") (srcpkgDescription srcPkg)
  projectFileExists <- doesFileExist projectFile
  -- TODO This is here to prevent reconfiguration of cached repl packages.
  -- It's worth investigating why it's needed in the first place.
  if projectFileExists then do
    contents <- readUTF8File projectFile
    when (contents /= showGenericPackageDescription (srcpkgDescription srcPkg))
      writeProjectFile
  else writeProjectFile
  return (ctx & lLocalPackages %~ (++ [SpecificSourcePackage srcPkg]))

-- | In a project or global context do nothing and return the base context unchanged.
-- In a script context, write a fake-package.cabal file and the script source file (Main.hs or Main.lhs)
-- and add a 'SourcePackage' to the base context.
updateContextAndWriteScriptProjectFiles :: ProjectBaseContext -> TargetContext -> IO ProjectBaseContext
updateContextAndWriteScriptProjectFiles ctx = \case
  ProjectContext     -> return ctx
  GlobalContext      -> return ctx
  ScriptContext {..} -> do
    let projectRoot = distProjectRootDirectory $ distDirLayout ctx
        mainName = if takeExtension scriptPath == ".lhs" then "Main.lhs" else "Main.hs"

        sourcePackage = fakeProjectSourcePackage projectRoot
          & lSrcpkgDescription . L.condExecutables
          .~ [("script", CondNode executable (targetBuildDepends $ buildInfo executable) [])]
        executable = scriptExecutable & L.modulePath .~ mainName

    BS.writeFile (projectRoot </> mainName) scriptContents
    updateContextAndWriteProjectFile ctx sourcePackage

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

-- | Extract the first encountered script metadata block started end
-- terminated by the bellow tokens or die.
--
-- * @{- cabal:@
--
-- * @-}@
--
-- Return the metadata and the contents of the file without the #! line.
readScriptBlockFromScript :: Verbosity -> BS.ByteString -> IO (Executable, BS.ByteString)
readScriptBlockFromScript verbosity str = do
    str' <- case extractScriptBlock str of
              Left e -> die' verbosity $ "Failed extracting script block: " ++ e
              Right x -> return x
    when (BS.all isSpace str') $ warn verbosity "Empty script block"
    (\x -> (x, noShebang)) <$> readScriptBlock verbosity str'
  where
    noShebang = BS.unlines . filter (not . BS.isPrefixOf "#!") . BS.lines $ str

-- | Extract the first encountered script metadata block started end
-- terminated by the tokens
--
-- * @{- cabal:@
--
-- * @-}@
--
-- appearing alone on lines (while tolerating trailing whitespace).
-- These tokens are not part of the 'Right' result.
--
-- In case of missing or unterminated blocks a 'Left'-error is
-- returned.
extractScriptBlock :: BS.ByteString -> Either String BS.ByteString
extractScriptBlock str = goPre (BS.lines str)
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
    startMarker = fromString "{- cabal:"
    endMarker   = fromString "-}"

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
