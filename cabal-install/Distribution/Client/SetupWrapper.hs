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
import Distribution.InstalledPackageInfo (installedPackageId)
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
         ( configCompilerEx )
import Distribution.Compiler ( buildCompilerId )
import Distribution.Simple.Compiler
         ( CompilerFlavor(GHC), Compiler(compilerId)
         , PackageDB(..), PackageDBStack )
import Distribution.Simple.PreProcess
         ( runSimplePreProcessor, ppUnlit )
import Distribution.Simple.Program
         ( ProgramConfiguration, emptyProgramConfiguration
         , getProgramSearchPath, getDbProgramOutput, runDbProgram, ghcProgram )
import Distribution.Simple.Program.Find
         ( programSearchPathAsPATHVar )
import Distribution.Simple.Program.Run
         ( getEffectiveEnvironment )
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
         , copyFileVerbose, rewriteFile, intercalate )
import Distribution.Client.Utils
         ( inDir, tryCanonicalizePath
         , existsAndIsMoreRecentThan, moreRecentFile )
import Distribution.System ( Platform(..), buildPlatform )
import Distribution.Text
         ( display )
import Distribution.Verbosity
         ( Verbosity )
import Distribution.Compat.Exception
         ( catchIO )

import System.Directory    ( doesFileExist )
import System.FilePath     ( (</>), (<.>) )
import System.IO           ( Handle, hPutStr )
import System.Exit         ( ExitCode(..), exitWith )
import System.Process      ( runProcess, waitForProcess )
import Control.Applicative ( (<$>), (<*>) )
import Control.Monad       ( when, unless )
import Data.List           ( foldl1' )
import Data.Maybe          ( fromMaybe, isJust )
import Data.Monoid         ( mempty )
import Data.Char           ( isSpace )

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
  (cabalLibVersion, mCabalLibInstalledPkgId, options') <- cabalLibVersionToUse
  debug verbosity $ "Using Cabal library version " ++ display cabalLibVersion
  path <- if useCachedSetupExecutable
          then getCachedSetupExecutable options'
               cabalLibVersion mCabalLibInstalledPkgId
          else compileSetupExecutable   options'
               cabalLibVersion mCabalLibInstalledPkgId False
  invokeSetupScript options' path (mkargs cabalLibVersion)

  where
  workingDir       = case fromMaybe "" (useWorkingDir options) of
                       []  -> "."
                       dir -> dir
  setupDir         = workingDir </> useDistPref options </> "setup"
  setupVersionFile = setupDir   </> "setup" <.> "version"
  setupHs          = setupDir   </> "setup" <.> "hs"
  setupProgFile    = setupDir   </> "setup" <.> exeExtension

  useCachedSetupExecutable = (bt == Simple || bt == Configure || bt == Make)

  maybeGetInstalledPackages :: SetupScriptOptions -> Compiler
                               -> ProgramConfiguration -> IO PackageIndex
  maybeGetInstalledPackages options' comp conf =
    case usePackageIndex options' of
      Just index -> return index
      Nothing    -> getInstalledPackages verbosity
                    comp (usePackageDB options') conf

  cabalLibVersionToUse :: IO (Version, (Maybe InstalledPackageId)
                             ,SetupScriptOptions)
  cabalLibVersionToUse = do
    savedVer <- savedVersion
    case savedVer of
      Just version | version `withinRange` useCabalVersion options
        -> do updateSetupScript version bt
              -- Does the previously compiled setup executable still exist and
              -- is it up-to date?
              useExisting <- canUseExistingSetup version
              if useExisting
                then return (version, Nothing, options)
                else installedVersion
      _ -> installedVersion
    where
      -- This check duplicates the checks in 'getCachedSetupExecutable' /
      -- 'compileSetupExecutable'. Unfortunately, we have to perform it twice
      -- because the selected Cabal version may change as a result of this
      -- check.
      canUseExistingSetup :: Version -> IO Bool
      canUseExistingSetup version =
        if useCachedSetupExecutable
        then do
          (_, cachedSetupProgFile) <- cachedSetupDirAndProg options version
          doesFileExist cachedSetupProgFile
        else
          (&&) <$> setupProgFile `existsAndIsMoreRecentThan` setupHs
               <*> setupProgFile `existsAndIsMoreRecentThan` setupVersionFile

      installedVersion :: IO (Version, Maybe InstalledPackageId
                             ,SetupScriptOptions)
      installedVersion = do
        (comp,    conf,    options')  <- configureCompiler options
        (version, mipkgid, options'') <- installedCabalVersion options' comp conf
        updateSetupScript version bt
        writeFile setupVersionFile (show version ++ "\n")
        return (version, mipkgid, options'')

      savedVersion :: IO (Maybe Version)
      savedVersion = do
        versionString <- readFile setupVersionFile `catchIO` \_ -> return ""
        case reads versionString of
          [(version,s)] | all isSpace s -> return (Just version)
          _                             -> return Nothing

  -- | Update a Setup.hs script, creating it if necessary.
  updateSetupScript :: Version -> BuildType -> IO ()
  updateSetupScript _ Custom = do
    useHs  <- doesFileExist customSetupHs
    useLhs <- doesFileExist customSetupLhs
    unless (useHs || useLhs) $ die
      "Using 'build-type: Custom' but there is no Setup.hs or Setup.lhs script."
    let src = (if useHs then customSetupHs else customSetupLhs)
    srcNewer <- src `moreRecentFile` setupHs
    when srcNewer $ if useHs
                    then copyFileVerbose verbosity src setupHs
                    else runSimplePreProcessor ppUnlit src setupHs verbosity
    where
      customSetupHs   = workingDir </> "Setup.hs"
      customSetupLhs  = workingDir </> "Setup.lhs"

  updateSetupScript cabalLibVersion _ =
    rewriteFile setupHs (buildTypeScript cabalLibVersion)

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

  installedCabalVersion :: SetupScriptOptions -> Compiler -> ProgramConfiguration
                        -> IO (Version, Maybe InstalledPackageId
                              ,SetupScriptOptions)
  installedCabalVersion options' _ _ | packageName pkg == PackageName "Cabal" =
    return (packageVersion pkg, Nothing, options')
  installedCabalVersion options' compiler conf = do
    index <- maybeGetInstalledPackages options' compiler conf
    let cabalDep   = Dependency (PackageName "Cabal") (useCabalVersion options')
        options''  = options' { usePackageIndex = Just index }
    case PackageIndex.lookupDependency index cabalDep of
      []   -> die $ "The package '" ++ display (packageName pkg)
                 ++ "' requires Cabal library version "
                 ++ display (useCabalVersion options)
                 ++ " but no suitable version is installed."
      pkgs -> let ipkginfo = head . snd . bestVersion fst $ pkgs
              in return (packageVersion ipkginfo
                        ,Just . installedPackageId $ ipkginfo, options'')

  bestVersion :: (a -> Version) -> [a] -> a
  bestVersion f = firstMaximumBy (comparing (preference . f))
    where
      -- Like maximumBy, but picks the first maximum element instead of the
      -- last. In general, we expect the preferred version to go first in the
      -- list. For the default case, this has the effect of choosing the version
      -- installed in the user package DB instead of the global one. See #1463.
      --
      -- Note: firstMaximumBy could be written as just
      -- `maximumBy cmp . reverse`, but the problem is that the behaviour of
      -- maximumBy is not fully specified in the case when there is not a single
      -- greatest element.
      firstMaximumBy :: (a -> a -> Ordering) -> [a] -> a
      firstMaximumBy _ []   =
        error "Distribution.Client.firstMaximumBy: empty list"
      firstMaximumBy cmp xs =  foldl1' maxBy xs
        where
          maxBy x y = case cmp x y of { GT -> x; EQ -> x; LT -> y; }

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
      Nothing   -> do (comp, _, conf) <-
                        configCompilerEx (Just GHC) Nothing Nothing
                        (useProgramConfig options') verbosity
                      return (comp, conf)
    -- Whenever we need to call configureCompiler, we also need to access the
    -- package index, so let's cache it here.
    index <- maybeGetInstalledPackages options' comp conf
    return (comp, conf, options' { useCompiler      = Just comp,
                                   usePackageIndex  = Just index,
                                   useProgramConfig = conf })

  -- | Path to the setup exe cache directory and path to the cached setup
  -- executable.
  cachedSetupDirAndProg :: SetupScriptOptions -> Version
                        -> IO (FilePath, FilePath)
  cachedSetupDirAndProg options' cabalLibVersion = do
    cabalDir <- defaultCabalDir
    let setupCacheDir       = cabalDir </> "setup-exe-cache"
        cachedSetupProgFile = setupCacheDir
                              </> ("setup-" ++ buildTypeString ++ "-"
                                   ++ cabalVersionString ++ "-"
                                   ++ platformString ++ "-"
                                   ++ compilerVersionString)
                              <.> exeExtension
    return (setupCacheDir, cachedSetupProgFile)
      where
        buildTypeString       = show bt
        cabalVersionString    = "Cabal-" ++ (display cabalLibVersion)
        compilerVersionString = display $
                                fromMaybe buildCompilerId
                                (fmap compilerId . useCompiler $ options')
        platformString        = display $
                                fromMaybe buildPlatform (usePlatform options')

  -- | Look up the setup executable in the cache; update the cache if the setup
  -- executable is not found.
  getCachedSetupExecutable :: SetupScriptOptions
                           -> Version -> Maybe InstalledPackageId
                           -> IO FilePath
  getCachedSetupExecutable options' cabalLibVersion
                           maybeCabalLibInstalledPkgId = do
    (setupCacheDir, cachedSetupProgFile) <-
      cachedSetupDirAndProg options' cabalLibVersion
    cachedSetupExists <- doesFileExist cachedSetupProgFile
    if cachedSetupExists
      then debug verbosity $
           "Found cached setup executable: " ++ cachedSetupProgFile
      else criticalSection' $ do
        -- The cache may have been populated while we were waiting.
        cachedSetupExists' <- doesFileExist cachedSetupProgFile
        if cachedSetupExists'
          then debug verbosity $
               "Found cached setup executable: " ++ cachedSetupProgFile
          else do
          debug verbosity $ "Setup executable not found in the cache."
          src <- compileSetupExecutable options'
                 cabalLibVersion maybeCabalLibInstalledPkgId True
          createDirectoryIfMissingVerbose verbosity True setupCacheDir
          installExecutableFile verbosity src cachedSetupProgFile
    return cachedSetupProgFile
      where
        criticalSection'      = fromMaybe id
                                (fmap criticalSection $ setupCacheLock options')

  -- | If the Setup.hs is out of date wrt the executable then recompile it.
  -- Currently this is GHC only. It should really be generalised.
  --
  compileSetupExecutable :: SetupScriptOptions
                         -> Version -> Maybe InstalledPackageId -> Bool
                         -> IO FilePath
  compileSetupExecutable options' cabalLibVersion maybeCabalLibInstalledPkgId
                         forceCompile = do
    setupHsNewer      <- setupHs          `moreRecentFile` setupProgFile
    cabalVersionNewer <- setupVersionFile `moreRecentFile` setupProgFile
    let outOfDate = setupHsNewer || cabalVersionNewer
    when (outOfDate || forceCompile) $ do
      debug verbosity "Setup executable needs to be updated, compiling..."
      (compiler, conf, options'') <- configureCompiler options'
      let cabalPkgid = PackageIdentifier (PackageName "Cabal") cabalLibVersion
      let ghcOptions = mempty {
              ghcOptVerbosity       = Flag verbosity
            , ghcOptMode            = Flag GhcModeMake
            , ghcOptInputFiles      = [setupHs]
            , ghcOptOutputFile      = Flag setupProgFile
            , ghcOptObjDir          = Flag setupDir
            , ghcOptHiDir           = Flag setupDir
            , ghcOptSourcePathClear = Flag True
            , ghcOptSourcePath      = [workingDir]
            , ghcOptPackageDBs      = usePackageDB options''
            , ghcOptPackages        = maybe []
                                      (\ipkgid -> [(ipkgid, cabalPkgid)])
                                      maybeCabalLibInstalledPkgId
            }
      let ghcCmdLine = renderGhcOptions compiler ghcOptions
      case useLoggingHandle options of
        Nothing          -> runDbProgram verbosity ghcProgram conf ghcCmdLine

        -- If build logging is enabled, redirect compiler output to the log file.
        (Just logHandle) -> do output <- getDbProgramOutput verbosity ghcProgram
                                         conf ghcCmdLine
                               hPutStr logHandle output
    return setupProgFile

  invokeSetupScript :: SetupScriptOptions -> FilePath -> [String] -> IO ()
  invokeSetupScript options' path args = do
    info verbosity $ unwords (path : args)
    case useLoggingHandle options' of
      Nothing        -> return ()
      Just logHandle -> info verbosity $ "Redirecting build log to "
                                      ++ show logHandle

    -- Since useWorkingDir can change the relative path, the path argument must
    -- be turned into an absolute path. On some systems, runProcess will take
    -- path as relative to the new working directory instead of the current
    -- working directory.
    path' <- tryCanonicalizePath path

    searchpath <- programSearchPathAsPATHVar
                    (getProgramSearchPath (useProgramConfig options'))
    env        <- getEffectiveEnvironment [("PATH", Just searchpath)]

    process <- runProcess path' args
                 (useWorkingDir options') env
                 Nothing (useLoggingHandle options') (useLoggingHandle options')
    exitCode <- waitForProcess process
    unless (exitCode == ExitSuccess) $ exitWith exitCode
