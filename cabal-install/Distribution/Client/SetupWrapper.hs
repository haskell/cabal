-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.SetupWrapper
-- Copyright   :  (c) The University of Glasgow 2006,
--                    Duncan Coutts 2008
--
-- Maintainer  :  cabal-devel@haskell.org
-- Stability   :  alpha
-- Portability :  portable
--
-- An interface to building and installing Cabal packages.
-- If the @Built-Type@ field is specified as something other than
-- 'Custom', and the current version of Cabal is acceptable, this performs
-- setup actions directly.  Otherwise it builds the setup script and
-- runs it with the given arguments.

module Distribution.Client.SetupWrapper (
    setupWrapper,
    SetupScriptOptions(..),
    defaultSetupScriptOptions,
  ) where

import qualified Distribution.Make as Make
import qualified Distribution.Simple as Simple
import Distribution.Version
         ( Version(..), VersionRange, anyVersion
         , intersectVersionRanges, orLaterVersion
         , withinRange )
import Distribution.InstalledPackageInfo (installedPackageId, sourcePackageId)
import Distribution.Package
         ( InstalledPackageId(..), PackageIdentifier(..),
           PackageName(..), Package(..), packageName
         , packageVersion, Dependency(..) )
import Distribution.PackageDescription
         ( GenericPackageDescription(packageDescription)
         , PackageDescription(..), specVersion
         , BuildType(..), knownBuildTypes )
import Distribution.PackageDescription.Parse
         ( readPackageDescription )
import Distribution.Simple.Configure
         ( configCompiler )
import Distribution.Compiler ( buildCompilerId )
import Distribution.Simple.Compiler
         ( CompilerFlavor(GHC), Compiler(compilerId)
         , compilerVersion
         , PackageDB(..), PackageDBStack )
import Distribution.Simple.Program
         ( ProgramConfiguration, emptyProgramConfiguration
         , getDbProgramOutput, runDbProgram, ghcProgram )
import Distribution.Simple.BuildPaths
         ( defaultDistPref, exeExtension )
import Distribution.Simple.Command
         ( CommandUI(..), commandShowOptions )
import Distribution.Simple.Program.GHC
         ( GhcMode(..), GhcOptions(..), renderGhcOptions )
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.Simple.PackageIndex (PackageIndex)
import Distribution.Client.Config
         ( defaultCabalDir )
import Distribution.Client.IndexUtils
         ( getInstalledPackages )
import Distribution.Client.JobControl
         ( Lock, criticalSection )
import Distribution.Simple.Setup
         ( Flag(..) )
import Distribution.Simple.Utils
         ( die, debug, info, cabalVersion, findPackageDesc, comparing
         , createDirectoryIfMissingVerbose, installExecutableFile
         , rewriteFile, intercalate )
import Distribution.Client.Utils
         ( moreRecentFile, inDir, tryCanonicalizePath )
import Distribution.System ( Platform(..), buildPlatform )
import Distribution.Text
         ( display )
import Distribution.Verbosity
         ( Verbosity )
import Distribution.Compat.Exception
         ( catchIO )

import System.Directory  ( doesFileExist )
import System.FilePath   ( (</>), (<.>) )
import System.IO         ( Handle, hPutStr )
import System.Exit       ( ExitCode(..), exitWith )
import System.Process    ( runProcess, waitForProcess )
import Control.Monad     ( when, unless )
import Data.List         ( maximumBy )
import Data.Maybe        ( fromMaybe, isJust )
import Data.Monoid       ( mempty )
import Data.Char         ( isSpace )

data SetupScriptOptions = SetupScriptOptions {
    useCabalVersion          :: VersionRange,
    useCompiler              :: Maybe Compiler,
    usePlatform              :: Maybe Platform,
    usePackageDB             :: PackageDBStack,
    usePackageIndex          :: Maybe PackageIndex,
    useProgramConfig         :: ProgramConfiguration,
    useDistPref              :: FilePath,
    useLoggingHandle         :: Maybe Handle,
    useWorkingDir            :: Maybe FilePath,
    forceExternalSetupMethod :: Bool,

    -- Used only when calling setupWrapper from parallel code to serialise
    -- access to the setup cache; should be Nothing otherwise.
    --
    -- Note: setup exe cache
    ------------------------
    -- When we are installing in parallel, we always use the external setup
    -- method. Since compiling the setup script each time adds noticeable
    -- overhead, we use a shared setup script cache
    -- ('~/.cabal/setup-exe-cache'). For each (compiler, platform, Cabal
    -- version) combination the cache holds a compiled setup script
    -- executable. This only affects the Simple build type; for the Custom,
    -- Configure and Make build types we always compile the setup script anew.
    setupCacheLock           :: Maybe Lock
  }

defaultSetupScriptOptions :: SetupScriptOptions
defaultSetupScriptOptions = SetupScriptOptions {
    useCabalVersion          = anyVersion,
    useCompiler              = Nothing,
    usePlatform              = Nothing,
    usePackageDB             = [GlobalPackageDB, UserPackageDB],
    usePackageIndex          = Nothing,
    useProgramConfig         = emptyProgramConfiguration,
    useDistPref              = defaultDistPref,
    useLoggingHandle         = Nothing,
    useWorkingDir            = Nothing,
    forceExternalSetupMethod = False,
    setupCacheLock           = Nothing
  }

setupWrapper :: Verbosity
             -> SetupScriptOptions
             -> Maybe PackageDescription
             -> CommandUI flags
             -> (Version -> flags)
             -> [String]
             -> IO ()
setupWrapper verbosity options mpkg cmd flags extraArgs = do
  pkg <- maybe getPkg return mpkg
  let setupMethod = determineSetupMethod options' buildType'
      options'    = options {
                      useCabalVersion = intersectVersionRanges
                                          (useCabalVersion options)
                                          (orLaterVersion (specVersion pkg))
                    }
      buildType'  = fromMaybe Custom (buildType pkg)
      mkArgs cabalLibVersion = commandName cmd
                             : commandShowOptions cmd (flags cabalLibVersion)
                            ++ extraArgs
  checkBuildType buildType'
  setupMethod verbosity options' (packageId pkg) buildType' mkArgs
  where
    getPkg = findPackageDesc (fromMaybe "." (useWorkingDir options))
         >>= readPackageDescription verbosity
         >>= return . packageDescription

    checkBuildType (UnknownBuildType name) =
      die $ "The build-type '" ++ name ++ "' is not known. Use one of: "
         ++ intercalate ", " (map display knownBuildTypes) ++ "."
    checkBuildType _ = return ()

-- | Decide if we're going to be able to do a direct internal call to the
-- entry point in the Cabal library or if we're going to have to compile
-- and execute an external Setup.hs script.
--
determineSetupMethod :: SetupScriptOptions -> BuildType -> SetupMethod
determineSetupMethod options buildType'
  | forceExternalSetupMethod options = externalSetupMethod
  | isJust (useLoggingHandle options)
 || buildType' == Custom             = externalSetupMethod
  | cabalVersion `withinRange`
      useCabalVersion options        = internalSetupMethod
  | otherwise                        = externalSetupMethod

type SetupMethod = Verbosity
                -> SetupScriptOptions
                -> PackageIdentifier
                -> BuildType
                -> (Version -> [String]) -> IO ()

-- ------------------------------------------------------------
-- * Internal SetupMethod
-- ------------------------------------------------------------

internalSetupMethod :: SetupMethod
internalSetupMethod verbosity options _ bt mkargs = do
  let args = mkargs cabalVersion
  debug verbosity $ "Using internal setup method with build-type " ++ show bt
                 ++ " and args:\n  " ++ show args
  inDir (useWorkingDir options) $
    buildTypeAction bt args

buildTypeAction :: BuildType -> ([String] -> IO ())
buildTypeAction Simple    = Simple.defaultMainArgs
buildTypeAction Configure = Simple.defaultMainWithHooksArgs
                              Simple.autoconfUserHooks
buildTypeAction Make      = Make.defaultMainArgs
buildTypeAction Custom               = error "buildTypeAction Custom"
buildTypeAction (UnknownBuildType _) = error "buildTypeAction UnknownBuildType"

-- ------------------------------------------------------------
-- * External SetupMethod
-- ------------------------------------------------------------

externalSetupMethod :: SetupMethod
externalSetupMethod verbosity options pkg bt mkargs = do
  debug verbosity $ "Using external setup method with build-type " ++ show bt
  createDirectoryIfMissingVerbose verbosity True setupDir
  (cabalLibVersion, options') <- cabalLibVersionToUse
  debug verbosity $ "Using Cabal library version " ++ display cabalLibVersion
  setupHs <- updateSetupScript cabalLibVersion bt
  debug verbosity $ "Using " ++ setupHs ++ " as setup script."
  path <- case bt of
    -- TODO: Should we also cache the setup exe for the Make and Configure build
    -- types?
    Simple -> getCachedSetupExecutable options' cabalLibVersion setupHs
    _      -> compileSetupExecutable options' cabalLibVersion setupHs False
  invokeSetupScript path (mkargs cabalLibVersion)

  where
  workingDir       = case fromMaybe "" (useWorkingDir options) of
                       []  -> "."
                       dir -> dir
  setupDir         = workingDir </> useDistPref options </> "setup"
  setupVersionFile = setupDir </> "setup" <.> "version"

  maybeGetInstalledPackages :: SetupScriptOptions -> Compiler
                               -> ProgramConfiguration -> IO PackageIndex
  maybeGetInstalledPackages options' comp conf =
    case usePackageIndex options' of
      Just index -> return index
      Nothing    -> getInstalledPackages verbosity
                    comp (usePackageDB options') conf

  cabalLibVersionToUse :: IO (Version, SetupScriptOptions)
  cabalLibVersionToUse = do
    savedVersion <- savedCabalVersion
    let versionRangeToUse = useCabalVersion options
    case savedVersion of
      Just version | version `withinRange` versionRangeToUse
                  -- If the Cabal lib version that cabal-install was compiled
                  -- with can be used instead of the cached version,
                  -- double-check that we really want to use the cached one.
                  && (version == cabalVersion
                      || not (cabalVersion `withinRange` versionRangeToUse))
        -> return (version, options)
      _ -> do (comp, conf, options') <- configureCompiler options
              version <- installedCabalVersion options' comp conf
              rewriteFile setupVersionFile (show version ++ "\n")
              return (version, options')

  savedCabalVersion = do
    versionString <- readFile setupVersionFile `catchIO` \_ -> return ""
    case reads versionString of
      [(version,s)] | all isSpace s -> return (Just version)
      _                             -> return Nothing

  installedCabalVersion :: SetupScriptOptions -> Compiler
                        -> ProgramConfiguration -> IO Version
  installedCabalVersion _ _ _ | packageName pkg == PackageName "Cabal" =
    return (packageVersion pkg)
  installedCabalVersion options' comp conf = do
    index <- maybeGetInstalledPackages options' comp conf
    let cabalDep = Dependency (PackageName "Cabal") (useCabalVersion options')
    case PackageIndex.lookupDependency index cabalDep of
      []   -> die $ "The package '" ++ display (packageName pkg)
                 ++ "' requires Cabal library version "
                 ++ display (useCabalVersion options)
                 ++ " but no suitable version is installed."
      pkgs -> return $ bestVersion id (map fst pkgs)

  bestVersion :: (a -> Version) -> [a] -> a
  bestVersion f = maximumBy (comparing (preference . f))
    where
      preference version   = (sameVersion, sameMajorVersion
                             ,stableVersion, latestVersion)
        where
          sameVersion      = version == cabalVersion
          sameMajorVersion = majorVersion version == majorVersion cabalVersion
          majorVersion     = take 2 . versionBranch
          stableVersion    = case versionBranch version of
                               (_:x:_) -> even x
                               _       -> False
          latestVersion    = version

  -- TODO: This function looks a lot like @installedCabalVersion@ - can the
  -- duplication be removed?
  installedCabalPkgId :: SetupScriptOptions -> Compiler -> ProgramConfiguration
                         -> Version -> IO (Maybe InstalledPackageId)
  installedCabalPkgId _ _ _ _ | packageName pkg == PackageName "Cabal" =
    return Nothing
  installedCabalPkgId options' compiler conf cabalLibVersion = do
    index <- maybeGetInstalledPackages options' compiler conf
    let cabalPkgid = PackageIdentifier (PackageName "Cabal") cabalLibVersion
    case PackageIndex.lookupSourcePackageId index cabalPkgid of
      []           -> die $ "The package '" ++ display (packageName pkg)
                      ++ "' requires Cabal library version "
                      ++ display (cabalLibVersion)
                      ++ " but no suitable version is installed."
      iPkgInfos   -> return . Just . installedPackageId
                     . bestVersion (pkgVersion . sourcePackageId) $ iPkgInfos

  configureCompiler :: SetupScriptOptions
                    -> IO (Compiler, ProgramConfiguration, SetupScriptOptions)
  configureCompiler options' = do
    (comp, conf) <- case useCompiler options' of
      Just comp -> return (comp, useProgramConfig options')
      Nothing   -> do (comp, _, conf) <-
                        configCompiler (Just GHC) Nothing Nothing
                        (useProgramConfig options') verbosity
                      return (comp, conf)
    -- Whenever we need to call configureCompiler, we also need to access the
    -- package index, so let's cache it here.
    index <- maybeGetInstalledPackages options' comp conf
    return (comp, conf, options' { useCompiler      = Just comp,
                                   usePackageIndex  = Just index,
                                   useProgramConfig = conf })

  -- | Decide which Setup.hs script to use, creating it if necessary.
  --
  updateSetupScript :: Version -> BuildType -> IO FilePath
  updateSetupScript _ Custom = do
    useHs  <- doesFileExist setupHs
    useLhs <- doesFileExist setupLhs
    unless (useHs || useLhs) $ die
      "Using 'build-type: Custom' but there is no Setup.hs or Setup.lhs script."
    return (if useHs then setupHs else setupLhs)
    where
      setupHs  = workingDir </> "Setup.hs"
      setupLhs = workingDir </> "Setup.lhs"

  updateSetupScript cabalLibVersion _ = do
    rewriteFile setupHs (buildTypeScript cabalLibVersion)
    return setupHs
    where
      setupHs  = setupDir </> "setup.hs"

  buildTypeScript :: Version -> String
  buildTypeScript cabalLibVersion = case bt of
    Simple    -> "import Distribution.Simple; main = defaultMain\n"
    Configure -> "import Distribution.Simple; main = defaultMainWithHooks "
              ++ if cabalLibVersion >= Version [1,3,10] []
                   then "autoconfUserHooks\n"
                   else "defaultUserHooks\n"
    Make      -> "import Distribution.Make; main = defaultMain\n"
    Custom             -> error "buildTypeScript Custom"
    UnknownBuildType _ -> error "buildTypeScript UnknownBuildType"

  -- | Look up the setup executable in the cache; update the cache if the setup
  -- executable is not found.
  getCachedSetupExecutable :: SetupScriptOptions -> Version -> FilePath
                           -> IO FilePath
  getCachedSetupExecutable options' cabalLibVersion setupHsFile = do
    cabalDir <- defaultCabalDir
    let setupCacheDir = cabalDir </> "setup-exe-cache"
    let setupProgFile = setupCacheDir
                        </> ("setup-" ++ cabalVersionString ++ "-"
                             ++ platformString ++ "-"
                             ++ compilerVersionString)
                        <.> exeExtension
    setupProgFileExists <- doesFileExist setupProgFile
    if setupProgFileExists
      then debug verbosity $
           "Found cached setup executable: " ++ setupProgFile
      else criticalSection' $ do
        -- The cache may have been populated while we were waiting.
        setupProgFileExists' <- doesFileExist setupProgFile
        if setupProgFileExists'
          then debug verbosity $
               "Found cached setup executable: " ++ setupProgFile
          else do
          debug verbosity $ "Setup executable not found in the cache."
          src <- compileSetupExecutable options' cabalLibVersion setupHsFile True
          createDirectoryIfMissingVerbose verbosity True setupCacheDir
          installExecutableFile verbosity src setupProgFile
    return setupProgFile
      where
        cabalVersionString    = "Cabal-" ++ (display cabalLibVersion)
        compilerVersionString = display $
                                fromMaybe buildCompilerId
                                (fmap compilerId . useCompiler $ options')
        platformString        = display $
                                fromMaybe buildPlatform (usePlatform options')
        criticalSection'      = fromMaybe id
                                (fmap criticalSection $ setupCacheLock options')

  -- | If the Setup.hs is out of date wrt the executable then recompile it.
  -- Currently this is GHC only. It should really be generalised.
  --
  compileSetupExecutable :: SetupScriptOptions -> Version -> FilePath -> Bool
                         -> IO FilePath
  compileSetupExecutable options' cabalLibVersion setupHsFile forceCompile = do
    setupHsNewer      <- setupHsFile      `moreRecentFile` setupProgFile
    cabalVersionNewer <- setupVersionFile `moreRecentFile` setupProgFile
    let outOfDate = setupHsNewer || cabalVersionNewer
    when (outOfDate || forceCompile) $ do
      debug verbosity "Setup executable needs to be updated, compiling..."
      (compiler, conf, options'') <- configureCompiler options'
      let cabalPkgid = PackageIdentifier (PackageName "Cabal") cabalLibVersion
      maybeCabalInstalledPkgId <- installedCabalPkgId options'' compiler conf
                                  cabalLibVersion
      let ghcOptions = mempty {
              ghcOptVerbosity       = Flag verbosity
            , ghcOptMode            = Flag GhcModeMake
            , ghcOptInputFiles      = [setupHsFile]
            , ghcOptOutputFile      = Flag setupProgFile
            , ghcOptObjDir          = Flag setupDir
            , ghcOptHiDir           = Flag setupDir
            , ghcOptSourcePathClear = Flag True
            , ghcOptSourcePath      = [workingDir]
            , ghcOptPackageDBs      = usePackageDB options''
            , ghcOptPackages        =
              maybe []
              (\cabalInstalledPkgId -> [(cabalInstalledPkgId, cabalPkgid)])
              maybeCabalInstalledPkgId
            }
      let ghcCmdLine = renderGhcOptions (compilerVersion compiler) ghcOptions
      case useLoggingHandle options of
        Nothing          -> runDbProgram verbosity ghcProgram conf ghcCmdLine

        -- If build logging is enabled, redirect compiler output to the log file.
        (Just logHandle) -> do output <- getDbProgramOutput verbosity ghcProgram
                                         conf ghcCmdLine
                               hPutStr logHandle output
    return setupProgFile
    where
      setupProgFile = setupDir </> "setup" <.> exeExtension

  invokeSetupScript :: FilePath -> [String] -> IO ()
  invokeSetupScript path args = do
    info verbosity $ unwords (path : args)
    case useLoggingHandle options of
      Nothing        -> return ()
      Just logHandle -> info verbosity $ "Redirecting build log to "
                                      ++ show logHandle

    -- Since useWorkingDir can change the relative path, the path argument must
    -- be turned into an absolute path. On some systems, runProcess will take
    -- path as relative to the new working directory instead of the current
    -- working directory.
    path' <- tryCanonicalizePath path

    process <- runProcess path' args
                 (useWorkingDir options) Nothing
                 Nothing (useLoggingHandle options) (useLoggingHandle options)
    exitCode <- waitForProcess process
    unless (exitCode == ExitSuccess) $ exitWith exitCode
