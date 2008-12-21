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
         ( Version(..), VersionRange(..), withinRange )
import Distribution.Package
         ( PackageIdentifier(..), PackageName(..), Package(..), packageName
         , packageVersion, Dependency(..) )
import Distribution.PackageDescription
         ( GenericPackageDescription(packageDescription)
         , PackageDescription(..), BuildType(..) )
import Distribution.PackageDescription.Parse
         ( readPackageDescription )
import Distribution.InstalledPackageInfo
         ( InstalledPackageInfo )
import Distribution.Simple.Configure
         ( configCompiler, getInstalledPackages )
import Distribution.Simple.Compiler
         ( CompilerFlavor(GHC), Compiler, PackageDB(..) )
import Distribution.Simple.Program
         ( ProgramConfiguration, emptyProgramConfiguration
         , rawSystemProgramConf, ghcProgram )
import Distribution.Simple.BuildPaths
         ( defaultDistPref, exeExtension )
import Distribution.Simple.Command
         ( CommandUI(..), commandShowOptions )
import Distribution.Simple.GHC
         ( ghcVerbosityOptions )
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.Simple.PackageIndex (PackageIndex)
import Distribution.Simple.Utils
         ( die, debug, info, cabalVersion, findPackageDesc, comparing
         , createDirectoryIfMissingVerbose )
import Distribution.Client.Utils
         ( moreRecentFile, rewriteFile, inDir )
import Distribution.Text
         ( display )
import Distribution.Verbosity
         ( Verbosity )

import System.Directory  ( doesFileExist, getCurrentDirectory )
import System.FilePath   ( (</>), (<.>) )
import System.IO         ( Handle )
import System.Exit       ( ExitCode(..), exitWith )
import System.Process    ( runProcess, waitForProcess )
import Control.Monad     ( when, unless )
import Data.List         ( maximumBy )
import Data.Maybe        ( fromMaybe, isJust )
import Data.Monoid       ( Monoid(mempty) )
import Data.Char         ( isSpace )

data SetupScriptOptions = SetupScriptOptions {
    useCabalVersion  :: VersionRange,
    useCompiler      :: Maybe Compiler,
    usePackageDB     :: PackageDB,
    usePackageIndex  :: Maybe (PackageIndex InstalledPackageInfo),
    useProgramConfig :: ProgramConfiguration,
    useDistPref      :: FilePath,
    useLoggingHandle :: Maybe Handle,
    useWorkingDir    :: Maybe FilePath
  }

defaultSetupScriptOptions :: SetupScriptOptions
defaultSetupScriptOptions = SetupScriptOptions {
    useCabalVersion  = AnyVersion,
    useCompiler      = Nothing,
    usePackageDB     = UserPackageDB,
    usePackageIndex  = Nothing,
    useProgramConfig = emptyProgramConfiguration,
    useDistPref      = defaultDistPref,
    useLoggingHandle = Nothing,
    useWorkingDir    = Nothing
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
                      useCabalVersion = IntersectVersionRanges
                                          (useCabalVersion options)
                                          (descCabalVersion pkg)
                    }
      buildType'  = fromMaybe Custom (buildType pkg)
      mkArgs cabalLibVersion = commandName cmd
                             : commandShowOptions cmd (flags cabalLibVersion)
                            ++ extraArgs
  setupMethod verbosity options' (packageId pkg) buildType' mkArgs
  where
    getPkg = findPackageDesc (fromMaybe "." (useWorkingDir options))
         >>= readPackageDescription verbosity
         >>= return . packageDescription

-- | Decide if we're going to be able to do a direct internal call to the
-- entry point in the Cabal library or if we're going to have to compile
-- and execute an external Setup.hs script.
--
determineSetupMethod :: SetupScriptOptions -> BuildType -> SetupMethod
determineSetupMethod options buildType'
  | isJust (useLoggingHandle options)
 || buildType' == Custom      = externalSetupMethod
  | cabalVersion `withinRange`
      useCabalVersion options = internalSetupMethod
  | otherwise                 = externalSetupMethod

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
  compileSetupExecutable options' cabalLibVersion setupHs
  invokeSetupScript (mkargs cabalLibVersion)

  where
  workingDir       = case fromMaybe "" (useWorkingDir options) of
                       []  -> "."
                       dir -> dir
  setupDir         = workingDir </> useDistPref options </> "setup"
  setupVersionFile = setupDir </> "setup" <.> "version"
  setupProgFile    = setupDir </> "setup" <.> exeExtension

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
    versionString <- readFile setupVersionFile `catch` \_ -> return ""
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
      Nothing    -> fromMaybe mempty
             `fmap` getInstalledPackages verbosity
                      comp (usePackageDB options') conf

    let cabalDep = Dependency (PackageName "Cabal") (useCabalVersion options)
    case PackageIndex.lookupDependency index cabalDep of
      []   -> die $ "The package requires Cabal library version "
                 ++ display (useCabalVersion options)
                 ++ " but no suitable version is installed."
      pkgs -> return $ bestVersion (map packageVersion pkgs)
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

  -- | If the Setup.hs is out of date wrt the executable then recompile it.
  -- Currently this is GHC only. It should really be generalised.
  --
  compileSetupExecutable :: SetupScriptOptions -> Version -> FilePath -> IO ()
  compileSetupExecutable options' cabalLibVersion setupHsFile = do
    setupHsNewer      <- setupHsFile      `moreRecentFile` setupProgFile
    cabalVersionNewer <- setupVersionFile `moreRecentFile` setupProgFile
    let outOfDate = setupHsNewer || cabalVersionNewer
    when outOfDate $ do
      debug verbosity "Setup script is out of date, compiling..."
      (_, conf, _) <- configureCompiler options'
      rawSystemProgramConf verbosity ghcProgram conf $
          ghcVerbosityOptions verbosity
       ++ ["--make", setupHsFile, "-o", setupProgFile
          ,"-odir", setupDir, "-hidir", setupDir
          ,"-i", "-i" ++ workingDir ]
       ++ (case usePackageDB options' of
             GlobalPackageDB      -> ["-no-user-package-conf"]
             UserPackageDB        -> []
             SpecificPackageDB db -> ["-no-user-package-conf"
                                     ,"-package-conf", db])
       ++ if packageName pkg == PackageName "Cabal"
            then []
            else ["-package", display cabalPkgid]
    where cabalPkgid = PackageIdentifier (PackageName "Cabal") cabalLibVersion

  invokeSetupScript :: [String] -> IO ()
  invokeSetupScript args = do
    info verbosity $ unwords (setupProgFile : args)
    case useLoggingHandle options of
      Nothing        -> return ()
      Just logHandle -> info verbosity $ "Redirecting build log to "
                                      ++ show logHandle
    currentDir <- getCurrentDirectory
    process <- runProcess (currentDir </> setupProgFile) args
                 (useWorkingDir options) Nothing
                 Nothing (useLoggingHandle options) (useLoggingHandle options)
    exitCode <- waitForProcess process
    unless (exitCode == ExitSuccess) $ exitWith exitCode
