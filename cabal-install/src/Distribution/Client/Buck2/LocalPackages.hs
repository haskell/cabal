-- | Which packages of a project @cabal buck2@ builds from source with buck2
-- (rather than leaving to cabal), and facts about them that come from the
-- elaborated install plan rather than from their @.cabal@ files.
module Distribution.Client.Buck2.LocalPackages
  ( isBuiltLocally
  , packageSourceDir
  , componentNamesFor
  , builtLocalPackages
  , wantedBuildTools
  , projectTestOptions
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import qualified Data.Map as Map

import Distribution.Client.DistDirLayout (DistDirLayout (distUnpackedSrcDirectory))
import qualified Distribution.Client.InstallPlan as InstallPlan
import Distribution.Client.ProjectPlanning
  ( ElaboratedConfiguredPackage (..)
  , ElaboratedInstallPlan
  )
import Distribution.Client.ProjectPlanning.Types
  ( BuildStyle (BuildAndInstall)
  , ElaboratedPackageOrComponent (ElabComponent, ElabPackage)
  , elabComponentName
  )
import Distribution.Client.Types.PackageLocation (PackageLocation (..))

import Distribution.Package (PackageName, packageId, packageName)
import Distribution.PackageDescription (PackageDescription)
import qualified Distribution.PackageDescription as PD
import Distribution.Simple.InstallDirs (PathTemplate)
import Distribution.Simple.Utils (dieWithException, ordNub)
import Distribution.Types.Component (componentBuildInfo, componentName)
import Distribution.Types.ComponentName (ComponentName (CTestName))
import Distribution.Types.ExeDependency (ExeDependency (..))
import Distribution.Types.UnqualComponentName (UnqualComponentName)

import Distribution.Client.Errors (CabalInstallException (Buck2NonLocalPackageLocation))

-- | Does buck2 (rather than cabal) build this package from source? True
-- for every genuinely local package, *plus* every non-local one whose own
-- build was forced 'inplace' by depending on one - when a non-local
-- package is forced inplace it must be built by buck2 from source too,
-- otherwise the build would contain multiple incompatible versions of the
-- local dependency. A real-world example is hackage-security in the cabal
-- project, which is not a local package but depends on the local
-- Cabal-syntax.
isBuiltLocally :: ElaboratedConfiguredPackage -> Bool
isBuiltLocally elab = elabLocalToProject elab || elabBuildStyle elab /= BuildAndInstall

-- | Real on-disk source directory for any package buck2 builds from
-- source - a genuinely local one (always 'LocalUnpackedPackage') or an
-- inplace non-local one, resolved the same way
-- 'Distribution.Client.ProjectPlanning.Types.dataDirEnvVarForPackage'
-- does for the same 'BuildInplaceOnly' case: a plain source checkout
-- uses its own path directly, anything fetched as a tarball\/repo was
-- already unpacked to 'distUnpackedSrcDirectory' to be built inplace in
-- the first place.
packageSourceDir :: Verbosity -> DistDirLayout -> ElaboratedConfiguredPackage -> IO FilePath
packageSourceDir verbosity distDirLayout elab = case elabPkgSourceLocation elab of
  LocalUnpackedPackage dir -> return dir
  _ | elabLocalToProject elab -> unsupported
  LocalTarballPackage{} -> return unpackedPath
  RemoteTarballPackage{} -> return unpackedPath
  RepoTarballPackage{} -> return unpackedPath
  RemoteSourceRepoPackage _ (Just localCheckout) -> return localCheckout
  RemoteSourceRepoPackage{} -> unsupported
  where
    unpackedPath = distUnpackedSrcDirectory distDirLayout (elabPkgSourceId elab)
    unsupported = dieWithException verbosity (Buck2NonLocalPackageLocation (prettyShow (packageId elab)))

-- | The buildable component names for one elaborated node - either the
-- single component 'elabComponentName' itself names (per-component
-- elaboration, @ElabComponent@), or *every* buildable component of the
-- whole package it configured (whole-package elaboration,
-- @ElabPackage@ - see 'elabComponentName's own haddock, "there could be
-- more, but default this": one @configureFinal@ call in that mode
-- genuinely produces a 'ComponentLocalBuildInfo' for every component of
-- the package internally, regardless of which single one
-- 'elabComponentName' defaults to).
componentNamesFor :: ElaboratedConfiguredPackage -> PackageDescription -> [ComponentName]
componentNamesFor elab pkgDesc = case elabPkgOrComp elab of
  ElabComponent _ -> maybeToList (elabComponentName elab)
  ElabPackage _ -> [componentName comp | comp <- PD.pkgBuildableComponents pkgDesc]

-- | The source directory and (whole-package) 'PackageDescription' of every
-- package buck2 builds from source. Per-component elaboration gives each
-- such package one 'ElaboratedConfiguredPackage' per component, all
-- sharing the same directory and 'PackageDescription', so there is one
-- entry per directory here.
builtLocalPackages :: Verbosity -> DistDirLayout -> ElaboratedInstallPlan -> IO [(FilePath, PackageDescription)]
builtLocalPackages verbosity distDirLayout plan =
  fmap (nubBy ((==) `on` fst)) . sequenceA $
    [ do
      dir <- packageSourceDir verbosity distDirLayout elab
      return (dir, elabPkgDescription elab)
    | InstallPlan.Configured elab <- InstallPlan.toList plan
    , isBuiltLocally elab
    ]

-- | Every @pkg:exe@ named in any component's @build-tool-depends:@ across
-- the given packages.
wantedBuildTools :: [(FilePath, PackageDescription)] -> [(PackageName, UnqualComponentName)]
wantedBuildTools pkgs =
  ordNub
    [ (pn, exeName)
    | (_dir, pkgDesc) <- pkgs
    , comp <- PD.pkgBuildableComponents pkgDesc
    , ExeDependency pn exeName _ <- PD.buildToolDepends (componentBuildInfo comp)
    ]

-- | The project's @test-options:@ for each test-suite that has any - only
-- the elaborated package knows them (they aren't part of the @.cabal@ file
-- or the 'LocalBuildInfo').
projectTestOptions :: ElaboratedInstallPlan -> Map (PackageName, ComponentName) [PathTemplate]
projectTestOptions plan =
  Map.fromList
    [ ((packageName elab, cname), elabTestTestOptions elab)
    | InstallPlan.Configured elab <- InstallPlan.toList plan
    , isBuiltLocally elab
    , not (null (elabTestTestOptions elab))
    , cname@CTestName{} <- componentNamesFor elab (elabPkgDescription elab)
    ]
