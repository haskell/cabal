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
import Distribution.Package
         ( PackageIdentifier(..), PackageName(..), Package(..), packageName
         , packageVersion, Dependency(..) )
import Distribution.PackageDescription
         ( GenericPackageDescription(packageDescription)
         , PackageDescription(..), specVersion
         , BuildType(..), knownBuildTypes )
import Distribution.PackageDescription.Parse
         ( readPackageDescription )
import Distribution.Simple.Configure
         ( configCompiler )
import Distribution.Simple.Compiler
         ( CompilerFlavor(GHC), Compiler, compilerVersion, showCompilerId
         , PackageDB(..), PackageDBStack )
import Distribution.Simple.Program
         ( ProgramConfiguration, emptyProgramConfiguration
         , getDbProgramOutput, runDbProgram, ghcProgram )
import Distribution.Simple.BuildPaths
         ( defaultDistPref, exeExtension )
import Distribution.Simple.Command
         ( CommandUI(..), commandShowOptions )
import Distribution.Simple.GHC
         ( ghcVerbosityOptions )
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.Simple.PackageIndex (PackageIndex)
import Distribution.Client.Config
         ( defaultCabalDir )
import Distribution.Client.IndexUtils
         ( getInstalledPackages )
import Distribution.Client.JobControl
         ( Lock, criticalSection )
import Distribution.Simple.Utils
         ( die, debug, info, cabalVersion, findPackageDesc, comparing
         , createDirectoryIfMissingVerbose, installExecutableFile
         , rewriteFile, intercalate )
import Distribution.Client.Utils
         ( moreRecentFile, inDir )
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
import Data.Char         ( isSpace )

data SetupScriptOptions = SetupScriptOptions {
    useCabalVersion          :: VersionRange,
    useCompiler              :: Maybe Compiler,
    usePackageDB             :: PackageDBStack,
    usePackageIndex          :: Maybe PackageIndex,
    useProgramConfig         :: ProgramConfiguration,
    useDistPref              :: FilePath,
    useLoggingHandle         :: Maybe Handle,
    useWorkingDir            :: Maybe FilePath,
    forceExternalSetupMethod :: Bool,

    -- Used only when calling setupWrapper from parallel code to serialise
    -- access to the setup cache; should be Nothing otherwise.
    setupCacheLock           :: Maybe Lock
  }

defaultSetupScriptOptions :: SetupScriptOptions
defaultSetupScriptOptions = SetupScriptOptions {
    useCabalVersion          = anyVersion,
    useCompiler              = Nothing,
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
    Simple -> getCachedSetupExecutable options' cabalLibVersion setupHs
    _      -> compileSetupExecutable options' cabalLibVersion setupHs
  invokeSetupScript path (mkargs cabalLibVersion)

  where
  workingDir       = case fromMaybe "" (useWorkingDir options) of
                       []  -> "."
                       dir -> dir
  setupDir         = workingDir </> useDistPref options </> "setup"
  setupVersionFile = setupDir </> "setup" <.> "version"

  cabalLibVersionToUse :: IO (Version, SetupScriptOptions)
  cabalLibVersionToUse = do
    savedVersion <- savedCabalVersion
    case savedVersion of
      Just version | version `withinRange` useCabalVersion options
        -> return (version, options)
      _ -> do (comp, conf, options') <- configureCompiler options
              version <- installedCabalVersion options comp conf
              writeFile setupVersionFile (show version ++ "\n")
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
    index <- case usePackageIndex options' of
      Just index -> return index
      Nothing    -> getInstalledPackages verbosity
                      comp (usePackageDB options') conf

    let cabalDep = Dependency (PackageName "Cabal") (useCabalVersion options)
    case PackageIndex.lookupDependency index cabalDep of
      []   -> die $ "The package requires Cabal library version "
                 ++ display (useCabalVersion options)
                 ++ " but no suitable version is installed."
      pkgs -> return $ bestVersion (map fst pkgs)
    where
      bestVersion          = maximumBy (comparing preference)
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

  configureCompiler :: SetupScriptOptions
                    -> IO (Compiler, ProgramConfiguration, SetupScriptOptions)
  configureCompiler options' = do
    (comp, conf) <- case useCompiler options' of
      Just comp -> return (comp, useProgramConfig options')
      Nothing   -> configCompiler (Just GHC) Nothing Nothing
                     (useProgramConfig options') verbosity
    return (comp, conf, options' { useCompiler = Just comp,
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
          src <- compileSetupExecutable options' cabalLibVersion setupHsFile
          createDirectoryIfMissingVerbose verbosity True setupCacheDir
          installExecutableFile verbosity src setupProgFile
    return setupProgFile
      where
        cabalVersionString    = "Cabal-" ++ (display cabalLibVersion)
        compilerVersionString = fromMaybe "nonexisting-compiler"
                                (showCompilerId `fmap` useCompiler options')
        criticalSection'      = fromMaybe id
                                (fmap criticalSection $ setupCacheLock options')

  -- | If the Setup.hs is out of date wrt the executable then recompile it.
  -- Currently this is GHC only. It should really be generalised.
  --
  compileSetupExecutable :: SetupScriptOptions -> Version -> FilePath
                         -> IO FilePath
  compileSetupExecutable options' cabalLibVersion setupHsFile = do
    setupHsNewer      <- setupHsFile      `moreRecentFile` setupProgFile
    cabalVersionNewer <- setupVersionFile `moreRecentFile` setupProgFile
    let outOfDate = setupHsNewer || cabalVersionNewer
    when outOfDate $ do
      debug verbosity "Setup script is out of date, compiling..."
      (compiler, conf, _) <- configureCompiler options'
      --TODO: get Cabal's GHC module to export a GhcOptions type and render func
      let ghcCmdLine =
            ghcVerbosityOptions verbosity
            ++ ["--make", setupHsFile, "-o", setupProgFile
               ,"-odir", setupDir, "-hidir", setupDir
               ,"-i", "-i" ++ workingDir ]
            ++ ghcPackageDbOptions compiler (usePackageDB options')
            ++ if packageName pkg == PackageName "Cabal"
               then []
               else ["-package", display cabalPkgid]
      case useLoggingHandle options of
        Nothing          -> runDbProgram verbosity ghcProgram conf ghcCmdLine

        -- If build logging is enabled, redirect compiler output to the log file.
        (Just logHandle) -> do output <- getDbProgramOutput verbosity ghcProgram
                                         conf ghcCmdLine
                               hPutStr logHandle output
    return setupProgFile
    where
      setupProgFile = setupDir </> "setup" <.> exeExtension
      cabalPkgid    = PackageIdentifier (PackageName "Cabal") cabalLibVersion

      ghcPackageDbOptions :: Compiler -> PackageDBStack -> [String]
      ghcPackageDbOptions compiler dbstack = case dbstack of
        (GlobalPackageDB:UserPackageDB:dbs) -> concatMap specific dbs
        (GlobalPackageDB:dbs)               -> ("-no-user-" ++ packageDbFlag)
                                             : concatMap specific dbs
        _                                   -> ierror
        where
          specific (SpecificPackageDB db) = [ '-':packageDbFlag, db ]
          specific _ = ierror
          ierror     = error "internal error: unexpected package db stack"

          packageDbFlag
            | compilerVersion compiler < Version [7,5] []
            = "package-conf"
            | otherwise
            = "package-db"

  invokeSetupScript :: FilePath -> [String] -> IO ()
  invokeSetupScript path args = do
    info verbosity $ unwords (path : args)
    case useLoggingHandle options of
      Nothing        -> return ()
      Just logHandle -> info verbosity $ "Redirecting build log to "
                                      ++ show logHandle
    process <- runProcess path args
                 (useWorkingDir options) Nothing
                 Nothing (useLoggingHandle options) (useLoggingHandle options)
    exitCode <- waitForProcess process
    unless (exitCode == ExitSuccess) $ exitWith exitCode
