-----------------------------------------------------------------------------
-- |
-- Module      :  Hackage.SetupWrapper
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

module Hackage.SetupWrapper (
    setupWrapper,
    SetupScriptOptions(..),
    defaultSetupScriptOptions,
  ) where

import qualified Distribution.Make as Make
import qualified Distribution.Simple as Simple
import Distribution.Version
         ( VersionRange(..), withinRange )
import Distribution.Package
         ( PackageIdentifier, packageVersion, packageId, Dependency(..) )
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
         ( distPref, exeExtension )
import Distribution.Simple.Command
         ( CommandUI(..), commandShowOptions )
import Distribution.Simple.GHC
         ( ghcVerbosityOptions )
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.Simple.PackageIndex (PackageIndex)
import Distribution.Simple.Utils
         ( die, debug, comparing, cabalVersion, defaultPackageDesc
         , rawSystemExit, createDirectoryIfMissingVerbose )
import Distribution.Text
         ( display )
import Distribution.Verbosity
         ( Verbosity )

import System.Directory  ( doesFileExist, getModificationTime )
import System.FilePath   ( (</>), (<.>) )
import System.IO.Error   ( isDoesNotExistError )
import Control.Monad     ( when, unless )
import Control.Exception ( evaluate )
import Data.Maybe        ( fromMaybe )
import Data.Monoid       ( Monoid(mempty) )
import Data.List         ( maximumBy )

data SetupScriptOptions = SetupScriptOptions {
    useCabalVersion  :: VersionRange,
    useCompiler      :: Maybe Compiler,
    usePackageIndex  :: Maybe (PackageIndex InstalledPackageInfo),
    useProgramConfig :: ProgramConfiguration
  }

defaultSetupScriptOptions :: SetupScriptOptions
defaultSetupScriptOptions = SetupScriptOptions {
    useCabalVersion  = AnyVersion,
    useCompiler      = Nothing,
    usePackageIndex  = Nothing,
    useProgramConfig = emptyProgramConfiguration
  }

setupWrapper :: Verbosity
             -> SetupScriptOptions
             -> Maybe PackageDescription
             -> CommandUI flags
             -> flags
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
      args = commandName cmd
           : commandShowOptions cmd flags
          ++ extraArgs
  setupMethod verbosity buildType' args
  where
    getPkg = defaultPackageDesc verbosity
         >>= readPackageDescription verbosity
         >>= return . packageDescription

-- | Decide if we're going to be able to do a direct internal call to the
-- entry point in the Cabal library or if we're going to have to compile
-- and execute an external Setup.hs script.
--
determineSetupMethod :: SetupScriptOptions -> BuildType -> SetupMethod
determineSetupMethod options buildType'
  | buildType' == Custom      = externalSetupMethod options
  | cabalVersion `withinRange`
      useCabalVersion options = internalSetupMethod
  | otherwise                 = externalSetupMethod options

type SetupMethod = Verbosity -> BuildType -> [String] -> IO ()

-- ------------------------------------------------------------
-- * Internal SetupMethod
-- ------------------------------------------------------------

internalSetupMethod :: SetupMethod
internalSetupMethod verbosity bt args = do
  debug verbosity $ "Using internal setup method with build-type " ++ show bt
                 ++ " and args:\n  " ++ show args
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

externalSetupMethod :: SetupScriptOptions -> SetupMethod
externalSetupMethod options verbosity bt args = do
  debug verbosity $ "Using external setup method with build-type " ++ show bt
                 ++ " and args:\n  " ++ show args
  setupHs <- updateSetupScript verbosity bt
  debug verbosity $ "Using " ++ setupHs ++ " as setup script."
  compileSetupExecutable verbosity options setupHs
  invokeSetupScript verbosity args

-- | Decide which Setup.hs script to use, creating it if necessary.
--
updateSetupScript :: Verbosity -> BuildType -> IO FilePath
updateSetupScript _ Custom = do
  useHs  <- doesFileExist "Setup.hs"
  useLhs <- doesFileExist "Setup.lhs"
  unless (useHs || useLhs) $ die
    "Using 'build-type: Custom' but there is no Setup.hs or Setup.lhs script."
  return (if useHs then "Setup.hs" else "Setup.lhs")

updateSetupScript verbosity bt = do
  createDirectoryIfMissingVerbose verbosity True setupDir
  rewriteFile setupHs (buildTypeScript bt)
  return setupHs
  where
    setupDir = distPref </> "setup"
    setupHs  = setupDir </> "setup" <.> "hs"

buildTypeScript :: BuildType -> String
buildTypeScript Simple    = "import Distribution.Simple; main = defaultMain"
buildTypeScript Configure = "import Distribution.Simple; "
                         ++ "main = defaultMainWithHooks autoconfUserHooks"
buildTypeScript Make      = "import Distribution.Make; main = defaultMain"
buildTypeScript Custom               = error "buildTypeScript Custom"
buildTypeScript (UnknownBuildType _) = error "buildTypeScript UnknownBuildType"

-- | If the Setup.hs is out of date wrt the executable then recompile it.
-- Currently this is GHC only. It should really be generalised.
--
compileSetupExecutable :: Verbosity -> SetupScriptOptions -> FilePath -> IO ()
compileSetupExecutable verbosity options setupHs = do
  outOfDate <- setupHs `moreRecentFile` setupProg
  when outOfDate $ do
    debug verbosity "Setup script is out of date, compiling..."
    (comp, conf) <- case useCompiler options of
      Just comp -> return (comp, useProgramConfig options)
      Nothing -> configCompiler (Just GHC) Nothing Nothing
                   (useProgramConfig options) verbosity
    cabalPkgId <- installedCabalLibVer verbosity options comp conf
    createDirectoryIfMissingVerbose verbosity True setupDir
    rawSystemProgramConf verbosity ghcProgram conf $
        ghcVerbosityOptions verbosity
     ++ ["--make", setupHs, "-o", setupProg
        ,"-package", display cabalPkgId
        ,"-odir", setupDir, "-hidir", setupDir]

  where
    setupDir  = distPref </> "setup"
    setupProg = setupDir </> "setup" <.> exeExtension

installedCabalLibVer :: Verbosity -> SetupScriptOptions
                     -> Compiler -> ProgramConfiguration
                     -> IO PackageIdentifier
installedCabalLibVer verbosity options comp conf = do
  index <- case usePackageIndex options of
    Just index -> return index
    Nothing    -> fromMaybe mempty
           `fmap` getInstalledPackages verbosity comp UserPackageDB conf
                  -- user packages are *allowed* here, no portability problem

  let cabalDep = Dependency "Cabal" (useCabalVersion options)
  case PackageIndex.lookupDependency index cabalDep of
    []   -> die $ "The package requires Cabal library version "
               ++ display (useCabalVersion options)
               ++ " but no suitable version is installed."
    pkgs -> return $ maximumBy (comparing packageVersion) (map packageId pkgs)

invokeSetupScript :: Verbosity -> [String] -> IO ()
invokeSetupScript verbosity args = rawSystemExit verbosity setupProg args
  where
    setupProg = distPref </> "setup" </> "setup" <.> exeExtension

-- ------------------------------------------------------------
-- * Utils
-- ------------------------------------------------------------

-- | Compare the modification times of two files to see if the first is newer
-- than the second. The first file must exist but the second need not.
-- The expected use case is when the second file is generated using the first.
-- In this use case, if the result is True then the second file is out of date.
--
moreRecentFile :: FilePath -> FilePath -> IO Bool
moreRecentFile a b = do
  exists <- doesFileExist b
  if not exists
    then return True
    else do tb <- getModificationTime b
            ta <- getModificationTime a
            return (ta > tb)

-- | Write a file but only if it would have new content. If we would be writing
-- the same as the existing content then leave the file as is so that we do not
-- update the file's modification time.
--
rewriteFile :: FilePath -> String -> IO ()
rewriteFile path newContent =
  flip catch mightNotExist $ do
    existingContent <- readFile path
    evaluate (length existingContent)
    unless (existingContent == newContent) $
      writeFile path newContent
  where
    mightNotExist e | isDoesNotExistError e = writeFile path newContent
                    | otherwise             = ioError e
