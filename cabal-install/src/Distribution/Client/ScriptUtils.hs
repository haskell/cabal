{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Utilities to help commands with scripts
module Distribution.Client.ScriptUtils
  ( getScriptHash
  , getScriptCacheDirectory
  , ensureScriptCacheDirectory
  , withContextAndSelectors
  , AcceptNoTargets (..)
  , TargetContext (..)
  , updateContextAndWriteProjectFile
  , updateContextAndWriteProjectFile'
  , fakeProjectSourcePackage
  , lSrcpkgDescription
  , movedExePath
  ) where

import Distribution.Client.Compat.Prelude hiding (toList)
import Prelude ()

import Distribution.Compat.Lens
import qualified Distribution.Types.Lens as L

import Distribution.CabalSpecVersion
  ( CabalSpecVersion (..)
  , cabalSpecLatest
  )
import Distribution.Client.Config
  ( defaultScriptBuildsDir
  )
import Distribution.Client.DistDirLayout
  ( DistDirLayout (..)
  , DistDirParams (..)
  )
import Distribution.Client.HashValue
  ( hashValue
  , showHashValue
  , truncateHash
  )
import Distribution.Client.HttpUtils
  ( HttpTransport
  , configureTransport
  )
import Distribution.Client.NixStyleOptions
  ( NixStyleFlags (..)
  )
import Distribution.Client.ProjectConfig
  ( PackageConfig (..)
  , ProjectConfig (..)
  , ProjectConfigShared (..)
  , projectConfigHttpTransport
  , reportParseResult
  , withGlobalConfig
  , withProjectOrGlobalConfig
  )
import Distribution.Client.ProjectConfig.Legacy
  ( ProjectConfigSkeleton
  , instantiateProjectConfigSkeletonFetchingCompiler
  , parseProject
  )
import Distribution.Client.ProjectConfig.Types (ProjectConfigToParse (..))
import Distribution.Client.ProjectFlags
  ( flagIgnoreProject
  )
import Distribution.Client.ProjectOrchestration
import Distribution.Client.ProjectPlanning
  ( ElaboratedConfiguredPackage (..)
  , ElaboratedSharedConfig (..)
  , configureCompiler
  )
import Distribution.Client.RebuildMonad
  ( runRebuild
  )
import Distribution.Client.Setup
  ( CommonSetupFlags (..)
  , ConfigFlags (..)
  , GlobalFlags (..)
  )
import Distribution.Client.TargetSelector
  ( TargetSelectorProblem (..)
  , TargetString (..)
  )
import Distribution.Client.Types
  ( PackageLocation (..)
  , PackageSpecifier (..)
  , UnresolvedSourcePackage
  )
import Distribution.Compiler
  ( CompilerId (..)
  , perCompilerFlavorToList
  )
import Distribution.FieldGrammar
  ( parseFieldGrammar
  , takeFields
  )
import Distribution.Fields
  ( ParseResult
  , parseFatalFailure
  , readFields
  )
import Distribution.PackageDescription
  ( ignoreConditions
  )
import Distribution.PackageDescription.FieldGrammar
  ( executableFieldGrammar
  )
import Distribution.PackageDescription.PrettyPrint
  ( showGenericPackageDescription
  )
import Distribution.Parsec
  ( Position (..)
  )
import qualified Distribution.SPDX.License as SPDX
import Distribution.Simple.Compiler
  ( Compiler (..)
  , OptimisationLevel (..)
  , compilerInfo
  )
import Distribution.Simple.Flag
  ( flagToMaybe
  , fromFlagOrDefault
  )
import Distribution.Simple.PackageDescription
  ( parseString
  )
import Distribution.Simple.Setup
  ( Flag (..)
  )
import Distribution.Simple.Utils
  ( createDirectoryIfMissingVerbose
  , createTempDirectory
  , dieWithException
  , handleDoesNotExist
  , readUTF8File
  , warn
  , writeUTF8File
  )
import Distribution.Solver.Types.SourcePackage as SP
  ( SourcePackage (..)
  )
import Distribution.System
  ( Platform (..)
  )
import Distribution.Types.BuildInfo
  ( BuildInfo (..)
  )
import Distribution.Types.ComponentId
  ( mkComponentId
  )
import Distribution.Types.CondTree
  ( CondTree (..)
  )
import Distribution.Types.Executable
  ( Executable (..)
  )
import Distribution.Types.GenericPackageDescription as GPD
  ( GenericPackageDescription (..)
  , emptyGenericPackageDescription
  )
import Distribution.Types.PackageDescription
  ( PackageDescription (..)
  , emptyPackageDescription
  )
import Distribution.Types.PackageName.Magic
  ( fakePackageCabalFileName
  , fakePackageId
  )
import Distribution.Types.UnitId
  ( newSimpleUnitId
  )
import Distribution.Types.UnqualComponentName
  ( UnqualComponentName
  )
import Distribution.Utils.NubList
  ( fromNubList
  )
import Distribution.Verbosity
  ( normal
  )
import Language.Haskell.Extension
  ( Language (..)
  )

import Control.Concurrent.MVar
  ( newEmptyMVar
  , putMVar
  , tryTakeMVar
  )
import Control.Exception
  ( bracket
  )
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Lazy ()
import qualified Data.Set as S
import Distribution.Client.Errors
import Distribution.Utils.Path
  ( unsafeMakeSymbolicPath
  )
import System.Directory
  ( canonicalizePath
  , doesFileExist
  , getTemporaryDirectory
  , removeDirectoryRecursive
  )
import System.FilePath
  ( makeRelative
  , normalise
  , takeDirectory
  , takeFileName
  , (</>)
  )
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

-- | Get the hash of a script's absolute path.
--
-- Two hashes will be the same as long as the absolute paths
-- are the same.
getScriptHash :: FilePath -> IO String
getScriptHash script =
  -- Truncation here tries to help with long path issues on Windows.
  showHashValue
    . truncateHash 26
    . hashValue
    . fromString
    <$> canonicalizePath script

-- | Get the directory for caching a script build.
--
-- The only identity of a script is it's absolute path, so append the
-- hashed path to the @script-builds@ dir to get the cache directory.
getScriptCacheDirectory :: FilePath -> IO FilePath
getScriptCacheDirectory script = (</>) <$> defaultScriptBuildsDir <*> getScriptHash script

-- | Get the directory for caching a script build and ensure it exists.
--
-- The only identity of a script is it's absolute path, so append the
-- hashed path to the @script-builds@ dir to get the cache directory.
ensureScriptCacheDirectory :: Verbosity -> FilePath -> IO FilePath
ensureScriptCacheDirectory verbosity script = do
  cacheDir <- getScriptCacheDirectory script
  createDirectoryIfMissingVerbose verbosity True cacheDir
  return cacheDir

-- | What your command should do when no targets are found.
data AcceptNoTargets
  = -- | die on 'TargetSelectorNoTargetsInProject'
    RejectNoTargets
  | -- | return a default 'TargetSelector'
    AcceptNoTargets
  deriving (Eq, Show)

-- | Information about the context in which we found the 'TargetSelector's.
data TargetContext
  = -- | The target selectors are part of a project.
    ProjectContext
  | -- | The target selectors are from the global context.
    GlobalContext
  | -- | The target selectors refer to a script. Contains the path to the script and
    -- the executable metadata parsed from the script
    ScriptContext FilePath Executable
  deriving (Eq, Show)

-- | Determine whether the targets represent regular targets or a script
-- and return the proper context and target selectors.
-- Die with an error message if selectors are valid as neither regular targets or as a script.
--
-- In the case that the context refers to a temporary directory,
-- delete it after the action finishes.
withContextAndSelectors
  :: AcceptNoTargets
  -- ^ What your command should do when no targets are found.
  -> Maybe ComponentKind
  -- ^ A target filter
  -> NixStyleFlags a
  -- ^ Command line flags
  -> [String]
  -- ^ Target strings or a script and args.
  -> GlobalFlags
  -- ^ Global flags.
  -> CurrentCommand
  -- ^ Current Command (usually for error reporting).
  -> (TargetContext -> ProjectBaseContext -> [TargetSelector] -> IO b)
  -- ^ The body of your command action.
  -> IO b
withContextAndSelectors noTargets kind flags@NixStyleFlags{..} targetStrings globalFlags cmd act =
  withTemporaryTempDirectory $ \mkTmpDir -> do
    (tc, ctx) <-
      withProjectOrGlobalConfig
        ignoreProject
        withProject
        (withGlobalConfig verbosity globalConfigFlag $ withoutProject mkTmpDir)

    (tc', ctx', sels) <- case targetStrings of
      -- Only script targets may end with ':'.
      -- Trying to readTargetSelectors such a target leads to a parse error.
      [target] | ":" `isSuffixOf` target -> do
        scriptOrError target [TargetSelectorNoScript $ TargetString1 target]
      _ -> do
        -- In the case where a selector is both a valid target and script, assume it is a target,
        -- because you can disambiguate the script with "./script"
        readTargetSelectors (localPackages ctx) kind targetStrings >>= \case
          -- If there are no target selectors and no targets are fine, return
          -- the context
          Left (TargetSelectorNoTargetsInCwd{} : _)
            | [] <- targetStrings
            , AcceptNoTargets <- noTargets ->
                return (tc, ctx, defaultTarget)
          Left err@(TargetSelectorNoTargetsInProject : _)
            -- If there are no target selectors and no targets are fine, return
            -- the context
            | [] <- targetStrings
            , AcceptNoTargets <- noTargets ->
                return (tc, ctx, defaultTarget)
            | (script : _) <- targetStrings -> scriptOrError script err
          Left err@(TargetSelectorNoSuch t _ : _)
            | TargetString1 script <- t -> scriptOrError script err
          Left err@(TargetSelectorExpected t _ _ : _)
            | TargetString1 script <- t -> scriptOrError script err
          Left err@(MatchingInternalError _ _ _ : _) -- Handle ':' in middle of script name.
            | [script] <- targetStrings -> scriptOrError script err
          Left err -> reportTargetSelectorProblems verbosity err
          Right sels -> return (tc, ctx, sels)

    act tc' ctx' sels
  where
    verbosity = fromFlagOrDefault normal (setupVerbosity $ configCommonFlags configFlags)
    ignoreProject = flagIgnoreProject projectFlags
    cliConfig = commandLineFlagsToProjectConfig globalFlags flags mempty
    globalConfigFlag = projectConfigConfigFile (projectConfigShared cliConfig)
    defaultTarget = [TargetPackage TargetExplicitNamed [fakePackageId] Nothing]

    withProject = do
      ctx <- establishProjectBaseContext verbosity cliConfig cmd
      return (ProjectContext, ctx)
    withoutProject mkTmpDir globalConfig = do
      distDirLayout <- establishDummyDistDirLayout verbosity (globalConfig <> cliConfig) =<< mkTmpDir
      ctx <- establishDummyProjectBaseContext verbosity (globalConfig <> cliConfig) distDirLayout [] cmd
      return (GlobalContext, ctx)

    scriptBaseCtx script globalConfig = do
      let noDistDir = mempty{projectConfigShared = mempty{projectConfigDistDir = Flag ""}}
      let cfg = noDistDir <> globalConfig <> cliConfig
      rootDir <- ensureScriptCacheDirectory verbosity script
      distDirLayout <- establishDummyDistDirLayout verbosity cfg rootDir
      establishDummyProjectBaseContext verbosity cfg distDirLayout [] cmd

    scriptOrError script err = do
      exists <- doesFileExist script
      if exists
        then do
          ctx <- withGlobalConfig verbosity globalConfigFlag (scriptBaseCtx script)

          let projectRoot = distProjectRootDirectory $ distDirLayout ctx
          writeFile (projectRoot </> "scriptlocation") =<< canonicalizePath script

          scriptContents <- BS.readFile script
          executable <- readExecutableBlockFromScript verbosity scriptContents

          httpTransport <-
            configureTransport
              verbosity
              (fromNubList . projectConfigProgPathExtra $ projectConfigShared cliConfig)
              (flagToMaybe . projectConfigHttpTransport $ projectConfigBuildOnly cliConfig)

          projectCfgSkeleton <- readProjectBlockFromScript verbosity httpTransport (distDirLayout ctx) (takeFileName script) scriptContents

          createDirectoryIfMissingVerbose verbosity True (distProjectCacheDirectory $ distDirLayout ctx)
          (compiler, platform@(Platform arch os), _) <- runRebuild projectRoot $ configureCompiler verbosity (distDirLayout ctx) (fst (ignoreConditions projectCfgSkeleton) <> projectConfig ctx)

          projectCfg <- instantiateProjectConfigSkeletonFetchingCompiler (pure (os, arch, compilerInfo compiler)) mempty projectCfgSkeleton

          let ctx' = ctx & lProjectConfig %~ (<> projectCfg)

              build_dir = distBuildDirectory (distDirLayout ctx') $ (scriptDistDirParams script) ctx' compiler platform
              exePath = build_dir </> "bin" </> scriptExeFileName script
              exePathRel = makeRelative (normalise projectRoot) exePath

              executable' =
                executable
                  & L.buildInfo . L.defaultLanguage %~ maybe (Just Haskell2010) Just
                  & L.buildInfo . L.options %~ fmap (setExePath exePathRel)

          createDirectoryIfMissingVerbose verbosity True (takeDirectory exePath)

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
      tmpBaseDir <- getTemporaryDirectory
      tmpRelDir <- createTempDirectory tmpBaseDir "cabal-repl."
      let tmpDir = tmpBaseDir </> tmpRelDir
      putMVar m tmpDir
      return tmpDir
    rmTmp m _ = tryTakeMVar m >>= maybe (return ()) (handleDoesNotExist () . removeDirectoryRecursive)

scriptComponenetName :: IsString s => FilePath -> s
scriptComponenetName scriptPath = fromString cname
  where
    cname = "script-" ++ map censor (takeFileName scriptPath)
    censor c
      | c `S.member` ccNamecore = c
      | otherwise = '_'

scriptExeFileName :: FilePath -> FilePath
scriptExeFileName scriptPath = "cabal-script-" ++ takeFileName scriptPath

scriptDistDirParams :: FilePath -> ProjectBaseContext -> Compiler -> Platform -> DistDirParams
scriptDistDirParams scriptPath ctx compiler platform =
  DistDirParams
    { distParamUnitId = newSimpleUnitId cid
    , distParamPackageId = fakePackageId
    , distParamComponentId = cid
    , distParamComponentName = Just $ CExeName cn
    , distParamCompilerId = compilerId compiler
    , distParamPlatform = platform
    , distParamOptimization = fromFlagOrDefault NormalOptimisation optimization
    }
  where
    cn = scriptComponenetName scriptPath
    cid = mkComponentId $ prettyShow fakePackageId <> "-inplace-" <> prettyShow cn
    optimization = (packageConfigOptimization . projectConfigLocalPackages . projectConfig) ctx

setExePath :: FilePath -> [String] -> [String]
setExePath exePath options
  | "-o" `notElem` options = "-o" : exePath : options
  | otherwise = options

-- | Add the 'SourcePackage' to the context and use it to write a .cabal file.
updateContextAndWriteProjectFile' :: ProjectBaseContext -> SourcePackage (PackageLocation (Maybe FilePath)) -> IO ProjectBaseContext
updateContextAndWriteProjectFile' ctx srcPkg = do
  let projectRoot = distProjectRootDirectory $ distDirLayout ctx
      packageFile = projectRoot </> fakePackageCabalFileName
      contents = showGenericPackageDescription (srcpkgDescription srcPkg)
      writePackageFile = writeUTF8File packageFile contents
  -- TODO This is here to prevent reconfiguration of cached repl packages.
  -- It's worth investigating why it's needed in the first place.
  packageFileExists <- doesFileExist packageFile
  if packageFileExists
    then do
      cached <- force <$> readUTF8File packageFile
      when
        (cached /= contents)
        writePackageFile
    else writePackageFile
  return (ctx & lLocalPackages %~ (++ [SpecificSourcePackage srcPkg]))

-- | Add the executable metadata to the context and write a .cabal file.
updateContextAndWriteProjectFile :: ProjectBaseContext -> FilePath -> Executable -> IO ProjectBaseContext
updateContextAndWriteProjectFile ctx scriptPath scriptExecutable = do
  let projectRoot = distProjectRootDirectory $ distDirLayout ctx

  absScript <- unsafeMakeSymbolicPath . makeRelative (normalise projectRoot) <$> canonicalizePath scriptPath
  let
    sourcePackage =
      fakeProjectSourcePackage projectRoot
        & lSrcpkgDescription . L.condExecutables
          .~ [(scriptComponenetName scriptPath, CondNode executable (targetBuildDepends $ buildInfo executable) [])]
    executable =
      scriptExecutable
        & L.modulePath .~ absScript

  updateContextAndWriteProjectFile' ctx sourcePackage

parseScriptBlock :: BS.ByteString -> ParseResult Executable
parseScriptBlock str =
  case readFields str of
    Right fs -> do
      let (fields, _) = takeFields fs
      parseFieldGrammar cabalSpecLatest fields (executableFieldGrammar "script")
    Left perr -> parseFatalFailure pos (show perr)
      where
        ppos = P.errorPos perr
        pos = Position (P.sourceLine ppos) (P.sourceColumn ppos)

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
    Left e -> dieWithException verbosity $ FailedExtractingScriptBlock e
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
    Left _ -> return mempty
    Right x ->
      reportParseResult verbosity "script" scriptName
        =<< parseProject scriptName distDownloadSrcDirectory httpTransport verbosity (ProjectConfigToParse x)

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
    isEndMarker = (== endMarker) . stripTrailSpace

    stripTrailSpace = fst . BS.spanEnd isSpace

    -- before start marker
    goPre ls = case dropWhile (not . isStartMarker) ls of
      [] -> Left $ "`" ++ BS.unpack startMarker ++ "` start marker not found"
      (_ : ls') -> goBody [] ls'

    goBody _ [] = Left $ "`" ++ BS.unpack endMarker ++ "` end marker not found"
    goBody acc (l : ls)
      | isEndMarker l = Right $! BS.unlines $ reverse acc
      | otherwise = goBody (l : acc) ls

    startMarker, endMarker :: BS.ByteString
    startMarker = "{- " <> header <> ":"
    endMarker = "-}"

-- | The base for making a 'SourcePackage' for a fake project.
-- It needs a 'Distribution.Types.Library.Library' or 'Executable' depending on the command.
fakeProjectSourcePackage :: FilePath -> SourcePackage (PackageLocation loc)
fakeProjectSourcePackage projectRoot = sourcePackage
  where
    sourcePackage =
      SourcePackage
        { srcpkgPackageId = fakePackageId
        , srcpkgDescription = genericPackageDescription
        , srcpkgSource = LocalUnpackedPackage projectRoot
        , srcpkgDescrOverride = Nothing
        }
    genericPackageDescription =
      emptyGenericPackageDescription
        { GPD.packageDescription = packageDescription
        }
    packageDescription =
      emptyPackageDescription
        { package = fakePackageId
        , specVersion = CabalSpecV2_2
        , licenseRaw = Left SPDX.NONE
        }

-- | Find the path of an exe that has been relocated with a "-o" option
movedExePath :: UnqualComponentName -> DistDirLayout -> ElaboratedSharedConfig -> ElaboratedConfiguredPackage -> Maybe FilePath
movedExePath selectedComponent distDirLayout elabShared elabConfigured = do
  exe <- find ((== selectedComponent) . exeName) . executables $ elabPkgDescription elabConfigured
  let CompilerId flavor _ = (compilerId . pkgConfigCompiler) elabShared
  opts <- lookup flavor (perCompilerFlavorToList . options $ buildInfo exe)
  let projectRoot = distProjectRootDirectory distDirLayout
  fmap (projectRoot </>) . lookup "-o" $ reverse (zip opts (drop 1 opts))

-- Lenses

-- | A lens for the 'srcpkgDescription' field of 'SourcePackage'
lSrcpkgDescription :: Lens' (SourcePackage loc) GenericPackageDescription
lSrcpkgDescription f s = fmap (\x -> s{srcpkgDescription = x}) (f (srcpkgDescription s))
{-# INLINE lSrcpkgDescription #-}

lLocalPackages :: Lens' ProjectBaseContext [PackageSpecifier UnresolvedSourcePackage]
lLocalPackages f s = fmap (\x -> s{localPackages = x}) (f (localPackages s))
{-# INLINE lLocalPackages #-}

lProjectConfig :: Lens' ProjectBaseContext ProjectConfig
lProjectConfig f s = fmap (\x -> s{projectConfig = x}) (f (projectConfig s))
{-# INLINE lProjectConfig #-}

-- Character classes
-- Transcribed from "templates/Lexer.x"
ccSpace, ccCtrlchar, ccPrintable, ccSymbol', ccParen, ccNamecore :: Set Char
ccSpace = S.fromList " "
ccCtrlchar = S.fromList $ [chr 0x0 .. chr 0x1f] ++ [chr 0x7f]
ccPrintable = S.fromList [chr 0x0 .. chr 0xff] S.\\ ccCtrlchar
ccSymbol' = S.fromList ",=<>+*&|!$%^@#?/\\~"
ccParen = S.fromList "()[]"
ccNamecore = ccPrintable S.\\ S.unions [ccSpace, S.fromList ":\"{}", ccParen, ccSymbol']
