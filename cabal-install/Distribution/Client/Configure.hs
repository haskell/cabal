-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Configure
-- Copyright   :  (c) David Himmelstrup 2005,
--                    Duncan Coutts 2005
-- License     :  BSD-like
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- High level interface to configuring a package.
-----------------------------------------------------------------------------
module Distribution.Client.Configure (
    configure,
  ) where

import Data.Monoid
         ( Monoid(mempty) )
import qualified Data.Map as Map

import Distribution.Client.Dependency
         ( resolveDependenciesWithProgress
         , PackageConstraint(..)
         , PackagesPreference(..), PackagesPreferenceDefault(..)
         , PackagePreference(..)
         , Progress(..), foldProgress, )
import qualified Distribution.Client.InstallPlan as InstallPlan
import Distribution.Client.InstallPlan (InstallPlan)
import Distribution.Client.IndexUtils as IndexUtils
         ( getAvailablePackages, getInstalledPackages )
import Distribution.Client.Setup
         ( ConfigExFlags(..), configureCommand, filterConfigureFlags )
import Distribution.Client.Types as Available
         ( AvailablePackage(..), AvailablePackageSource(..), Repo(..)
         , AvailablePackageDb(..), ConfiguredPackage(..), InstalledPackage )
import Distribution.Client.SetupWrapper
         ( setupWrapper, SetupScriptOptions(..), defaultSetupScriptOptions )

import Distribution.Simple.Compiler
         ( CompilerId(..), Compiler(compilerId)
         , PackageDB(..), PackageDBStack )
import Distribution.Simple.Program (ProgramConfiguration )
import Distribution.Simple.Setup
         ( ConfigFlags(..), toFlag, flagToMaybe, fromFlagOrDefault )
import qualified Distribution.Client.PackageIndex as PackageIndex
import Distribution.Client.PackageIndex (PackageIndex)
import Distribution.Simple.Utils
         ( defaultPackageDesc )
import Distribution.Package
         ( PackageName, packageName, packageVersion
         , Package(..), Dependency(..), thisPackageVersion )
import Distribution.PackageDescription.Parse
         ( readPackageDescription )
import Distribution.PackageDescription.Configuration
         ( finalizePackageDescription )
import Distribution.Version
         ( VersionRange, anyVersion, thisVersion )
import Distribution.Simple.Utils as Utils
         ( notice, info, die )
import Distribution.System
         ( Platform, buildPlatform )
import Distribution.Verbosity as Verbosity
         ( Verbosity )

-- | Configure the package found in the local directory
configure :: Verbosity
          -> PackageDBStack
          -> [Repo]
          -> Compiler
          -> ProgramConfiguration
          -> ConfigFlags
          -> ConfigExFlags
          -> [String]
          -> IO ()
configure verbosity packageDBs repos comp conf
  configFlags configExFlags extraArgs = do

  installed <- getInstalledPackages verbosity comp packageDBs conf
  available <- getAvailablePackages verbosity repos

  progress <- planLocalPackage verbosity comp configFlags configExFlags
                               installed available

  notice verbosity "Resolving dependencies..."
  maybePlan <- foldProgress (\message rest -> info verbosity message >> rest)
                            (return . Left) (return . Right) progress
  case maybePlan of
    Left message -> do
      info verbosity message
      setupWrapper verbosity (setupScriptOptions installed) Nothing
        configureCommand (const configFlags) extraArgs

    Right installPlan -> case InstallPlan.ready installPlan of
      [pkg@(ConfiguredPackage (AvailablePackage _ _ (LocalUnpackedPackage _)) _ _)] ->
        configurePackage verbosity
          (InstallPlan.planPlatform installPlan)
          (InstallPlan.planCompiler installPlan)
          (setupScriptOptions installed)
          configFlags pkg extraArgs

      _ -> die $ "internal error: configure install plan should have exactly "
              ++ "one local ready package."

  where
    setupScriptOptions index = SetupScriptOptions {
      useCabalVersion  = maybe anyVersion thisVersion
                         (flagToMaybe (configCabalVersion configExFlags)),
      useCompiler      = Just comp,
      -- Hack: we typically want to allow the UserPackageDB for finding the
      -- Cabal lib when compiling any Setup.hs even if we're doing a global
      -- install. However we also allow looking in a specific package db.
      usePackageDB     = if UserPackageDB `elem` packageDBs
                           then packageDBs
                           else packageDBs ++ [UserPackageDB],
      usePackageIndex  = if UserPackageDB `elem` packageDBs
                           then Just index
                           else Nothing,
      useProgramConfig = conf,
      useDistPref      = fromFlagOrDefault
                           (useDistPref defaultSetupScriptOptions)
                           (configDistPref configFlags),
      useLoggingHandle = Nothing,
      useWorkingDir    = Nothing
    }

-- | Make an 'InstallPlan' for the unpacked package in the current directory,
-- and all its dependencies.
--
planLocalPackage :: Verbosity -> Compiler
                 -> ConfigFlags -> ConfigExFlags
                 -> PackageIndex InstalledPackage
                 -> AvailablePackageDb
                 -> IO (Progress String String InstallPlan)
planLocalPackage verbosity comp configFlags configExFlags installed
  (AvailablePackageDb _ availablePrefs) = do
  pkg <- readPackageDescription verbosity =<< defaultPackageDesc verbosity
  let -- The trick is, we add the local package to the available index and
      -- remove it from the installed index. Then we ask to resolve a
      -- dependency on exactly that package. So the resolver ends up having
      -- to pick the local package.
      available' = PackageIndex.insert localPkg mempty
      installed' = PackageIndex.deletePackageId (packageId localPkg) installed
      localPkg = AvailablePackage {
        packageInfoId                = packageId pkg,
        Available.packageDescription = pkg,
        packageSource                = LocalUnpackedPackage Nothing
      }
      targets     = [packageName pkg]
      constraints = [PackageVersionConstraint (packageName pkg)
                       (thisVersion (packageVersion pkg))
                    ,PackageFlagsConstraint   (packageName pkg)
                       (configConfigurationsFlags configFlags)]
                 ++ [ PackageVersionConstraint name ver
                    | Dependency name ver <- configConstraints configFlags ]
      preferences = mergePackagePrefs PreferLatestForSelected
                                      availablePrefs configExFlags

  return $ resolveDependenciesWithProgress buildPlatform (compilerId comp)
             installed' available' preferences constraints targets


mergePackagePrefs :: PackagesPreferenceDefault
                  -> Map.Map PackageName VersionRange
                  -> ConfigExFlags
                  -> PackagesPreference
mergePackagePrefs defaultPref availablePrefs configExFlags =
  PackagesPreference defaultPref $
       -- The preferences that come from the hackage index
       [ PackageVersionPreference name ver
       | (name, ver) <- Map.toList availablePrefs ]
       -- additional preferences from the config file or command line
    ++ [ PackageVersionPreference name ver
       | Dependency name ver <- configPreferences configExFlags ]

-- | Call an installer for an 'AvailablePackage' but override the configure
-- flags with the ones given by the 'ConfiguredPackage'. In particular the
-- 'ConfiguredPackage' specifies an exact 'FlagAssignment' and exactly
-- versioned package dependencies. So we ignore any previous partial flag
-- assignment or dependency constraints and use the new ones.
--
configurePackage :: Verbosity
                 -> Platform -> CompilerId
                 -> SetupScriptOptions
                 -> ConfigFlags
                 -> ConfiguredPackage
                 -> [String]
                 -> IO ()
configurePackage verbosity platform comp scriptOptions configFlags
  (ConfiguredPackage (AvailablePackage _ gpkg _) flags deps) extraArgs =

  setupWrapper verbosity
    scriptOptions (Just pkg) configureCommand configureFlags extraArgs

  where
    configureFlags   = filterConfigureFlags configFlags {
      configConfigurationsFlags = flags,
      configConstraints         = map thisPackageVersion deps,
      configVerbosity           = toFlag verbosity
    }

    pkg = case finalizePackageDescription flags
           (const True)
           platform comp [] gpkg of
      Left _ -> error "finalizePackageDescription ConfiguredPackage failed"
      Right (desc, _) -> desc
