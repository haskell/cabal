-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Outdated
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Implementation of the 'outdated' command. Checks for outdated
-- dependencies in the package description file or freeze file.
-----------------------------------------------------------------------------

module Distribution.Client.Outdated ( outdated ) where

import Prelude ()
import Distribution.Client.Config
import Distribution.Client.IndexUtils as IndexUtils
import Distribution.Client.Compat.Prelude
import Distribution.Client.ProjectConfig
import Distribution.Client.RebuildMonad
import Distribution.Client.Setup
import Distribution.Client.Targets
import Distribution.Client.Types
import Distribution.Solver.Types.PackageConstraint
import Distribution.Solver.Types.PackageIndex
import Distribution.Client.Sandbox.PackageEnvironment

import Distribution.Package                          (PackageName, packageVersion)
import Distribution.PackageDescription               (buildDepends)
import Distribution.PackageDescription.Configuration (finalizePD)
import Distribution.PackageDescription.Parse
       (readPackageDescription)
import Distribution.Simple.Compiler                  (Compiler, compilerInfo)
import Distribution.Simple.Setup                     (fromFlagOrDefault)
import Distribution.Simple.Utils
       (die, notice, debug, tryFindPackageDesc)
import Distribution.System                           (Platform)
import Distribution.Text                             (display)
import Distribution.Types.ComponentRequestedSpec
       (defaultComponentRequestedSpec)
import Distribution.Types.Dependency
       (Dependency(..), depPkgName, simplifyDependency)
import Distribution.Verbosity                        (Verbosity)
import Distribution.Version
       (Version, UpperBound(..), asVersionIntervals, majorBoundVersion)

import qualified Data.Set as S
import System.Directory                              (getCurrentDirectory)
import System.Exit                                   (exitFailure)

-- | Entry point for the 'outdated' command.
outdated :: Verbosity -> OutdatedFlags -> RepoContext
         -> Compiler -> Platform
         -> IO ()
outdated verbosity outdatedFlags repoContext comp platform = do
  sourcePkgDb <- IndexUtils.getSourcePackages verbosity repoContext
  let freezeFile    = fromFlagOrDefault False (outdatedFreezeFile outdatedFlags)
      newFreezeFile = fromFlagOrDefault False (outdatedNewFreezeFile outdatedFlags)
      exitCode      = fromFlagOrDefault False (outdatedExitCode outdatedFlags)
      ignore        = S.fromList (outdatedIgnore outdatedFlags)
      minor         = S.fromList (outdatedMinor outdatedFlags)
      pkgIndex      = packageIndex sourcePkgDb
  deps <- if freezeFile
          then depsFromFreezeFile verbosity
          else if newFreezeFile
               then depsFromNewFreezeFile verbosity
               else depsFromPkgDesc       verbosity comp platform
  let outdatedDeps = listOutdated deps pkgIndex ignore minor
  notice verbosity ("Outdated dependencies: "
                     ++ intercalate ", "
                        (map (\(d, v) -> display d
                               ++ " (latest: " ++ display v ++ ")") outdatedDeps))
  if (exitCode && (not . null $ outdatedDeps))
    then exitFailure
    else return ()

-- | Convert a list of 'UserConstraint's to a 'Dependency' list.
userConstraintsToDependencies :: [UserConstraint] -> [Dependency]
userConstraintsToDependencies ucnstrs =
  mapMaybe (packageConstraintToDependency . userToPackageConstraint) ucnstrs

-- | Read the list of dependencies from the freeze file.
depsFromFreezeFile :: Verbosity -> IO [Dependency]
depsFromFreezeFile verbosity = do
  cwd        <- getCurrentDirectory
  userConfig <- loadUserConfig verbosity cwd Nothing
  let ucnstrs = map fst . configExConstraints . savedConfigureExFlags $ userConfig
      deps    = userConstraintsToDependencies ucnstrs
  debug verbosity "Reading the list of dependencies from the freeze file"
  return deps

-- | Read the list of dependencies from the new-style freeze file.
depsFromNewFreezeFile :: Verbosity -> IO [Dependency]
depsFromNewFreezeFile verbosity = do
  projectRootDir <- findProjectRoot {- TODO: Support '--project-file' -} mempty
  projectConfig <- runRebuild projectRootDir $
                   readProjectLocalFreezeConfig verbosity mempty projectRootDir
  let ucnstrs = map fst . projectConfigConstraints . projectConfigShared
                $ projectConfig
      deps    = userConstraintsToDependencies ucnstrs
  debug verbosity "Reading the list of dependencies from the new-style freeze file"
  return deps

-- | Read the list of dependencies from the package description.
depsFromPkgDesc :: Verbosity -> Compiler  -> Platform -> IO [Dependency]
depsFromPkgDesc verbosity comp platform = do
  cwd  <- getCurrentDirectory
  path <- tryFindPackageDesc cwd
  gpd  <- readPackageDescription verbosity path
  let cinfo = compilerInfo comp
      epd = finalizePD [] defaultComponentRequestedSpec
            (const True) platform cinfo [] gpd
  case epd of
    Left _        -> die "finalizePD failed"
    Right (pd, _) -> do
      let bd = buildDepends pd
      debug verbosity
        "Reading the list of dependencies from the package description"
      return bd

-- | Find all outdated dependencies.
listOutdated :: [Dependency] -> PackageIndex UnresolvedSourcePackage
              -> S.Set PackageName -> S.Set PackageName
              -> [(Dependency, Version)]
listOutdated deps pkgIndex ignore minor =
  mapMaybe isOutdated $ map simplifyDependency deps
  where
    isOutdated :: Dependency -> Maybe (Dependency, Version)
    isOutdated dep
      | depPkgName dep `S.member` ignore = Nothing
      | otherwise                      =
          let this   = map packageVersion $ lookupDependency pkgIndex dep
              latest = lookupLatest dep
          in (\v -> (dep, v)) `fmap` isOutdated' this latest

    isOutdated' :: [Version] -> [Version] -> Maybe Version
    isOutdated' [] _  = Nothing
    isOutdated' _  [] = Nothing
    isOutdated' this latest = let this'   = last this
                                  latest' = last latest
                              in if this' < latest' then Just latest' else Nothing

    lookupLatest :: Dependency -> [Version]
    lookupLatest dep
      | depPkgName dep `S.member` minor =
        map packageVersion $ lookupDependency pkgIndex  (relaxMinor dep)
      | otherwise                     =
        map packageVersion $ lookupPackageName pkgIndex (depPkgName dep)

    relaxMinor :: Dependency -> Dependency
    relaxMinor (Dependency pn vr) = (Dependency pn vr')
      where
        vr' = let vis = asVersionIntervals vr
                  (_,upper) = last vis
              in case upper of
                   NoUpperBound     -> vr
                   UpperBound ver _ -> majorBoundVersion ver
