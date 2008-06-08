-----------------------------------------------------------------------------
-- |
-- Module      :  Hackage.Reporting
-- Copyright   :  (c) David Waern 2008
-- License     :  BSD-like
--
-- Maintainer  :  david.waern@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Report data structure
--
-----------------------------------------------------------------------------
module Hackage.Reporting (
    BuildReport(..),
    InstallOutcome(..),
    DocsOutcome(..),
    TestsOutcome(..),

    -- * Constructing and writing reports
    buildReport,
    writeBuildReports,

    -- * 'InstallPlan' variants
    planPackageBuildReport,
    installPlanBuildReports,
    writeInstallPlanBuildReports
  ) where

import Hackage.Types
         ( ConfiguredPackage(..), BuildResult )
import qualified Hackage.Types as BR
         ( BuildResult(..) )
import qualified Hackage.InstallPlan as InstallPlan
import Hackage.InstallPlan
         ( InstallPlan, PlanPackage(..) )
import Hackage.Config
         ( defaultBuildReportFile )

import Distribution.Package
         ( PackageIdentifier, Package(packageId) )
import Distribution.PackageDescription
         ( FlagAssignment )
--import Distribution.Version
--         ( Version )
import Distribution.System
         ( OS, Arch )
import Distribution.Compiler
         ( CompilerId )

import Data.Maybe
         ( catMaybes )


data BuildReport
   = BuildReport {
    -- | The package this build report is about
    package         :: PackageIdentifier,

    -- | Which hackage server this package is from and thus which server this
    -- report should be sent to.
--    server          :: String,

    -- | The OS and Arch the package was built on
    os              :: OS,
    arch            :: Arch,

    -- | The Haskell compiler (and hopefully version) used
    compiler        :: CompilerId,

    -- | Which configurations flags we used
    flagAssignment  :: FlagAssignment,

    -- | Which dependent packages we were using exactly
    dependencies    :: [PackageIdentifier],

    -- | Did installing work ok?
    installOutcome  :: InstallOutcome,

    -- | Which version of the Cabal library was used to compile the Setup.hs
--    cabalVersion    :: Version,

    -- | Which build tools we were using (with versions)
--    tools      :: [PackageIdentifier],

    -- | Configure outcome, did configure work ok?
    docsOutcome     :: DocsOutcome,

    -- | Configure outcome, did configure work ok?
    testsOutcome    :: TestsOutcome
  }
  deriving (Show, Read)

data InstallOutcome
   = DependencyFailed PackageIdentifier
   | DownloadFailed
   | UnpackFailed
   | SetupFailed
   | ConfigureFailed
   | BuildFailed
   | InstallFailed
   | InstallOk
   deriving (Show, Read)

data DocsOutcome
   = DocsNotTried
   | DocsFailed
   | DocsOk
   deriving (Show, Read)

data TestsOutcome
   = TestsNotTried
   | TestsFailed
   | TestsOk
   deriving (Show, Read)

writeBuildReports :: [BuildReport] -> IO ()
writeBuildReports reports = do
  file <- defaultBuildReportFile
  appendFile file (unlines (map show reports))

buildReport :: OS -> Arch -> CompilerId -- -> Version
            -> ConfiguredPackage -> BR.BuildResult
            -> BuildReport
buildReport os' arch' comp (ConfiguredPackage pkg flags deps) result =
  BuildReport {
    package               = packageId pkg,
    os                    = os',
--    server                = undefined,
    arch                  = arch',
    compiler              = comp,
    flagAssignment        = flags,
    dependencies          = deps,
    installOutcome        = case result of
    BR.DependentFailed p -> DependencyFailed p
    BR.UnpackFailed _    -> UnpackFailed
    BR.ConfigureFailed _ -> ConfigureFailed
    BR.BuildFailed _     -> BuildFailed
    BR.InstallFailed _   -> InstallFailed
    BR.BuildOk           -> InstallOk,
--    cabalVersion          = undefined
    docsOutcome           = DocsNotTried,
    testsOutcome          = TestsNotTried
  }

-- ------------------------------------------------------------
-- * InstallPlan support
-- ------------------------------------------------------------

writeInstallPlanBuildReports :: InstallPlan BuildResult -> IO ()
writeInstallPlanBuildReports = writeBuildReports . installPlanBuildReports

installPlanBuildReports :: InstallPlan BuildResult -> [BuildReport]
installPlanBuildReports plan = catMaybes
                             . map (planPackageBuildReport os' arch' comp)
                             . InstallPlan.toList
                             $ plan
  where os'   = InstallPlan.planOS plan
        arch' = InstallPlan.planArch plan
        comp  = InstallPlan.planCompiler plan

planPackageBuildReport :: OS -> Arch -> CompilerId
                       -> PlanPackage BuildResult -> Maybe BuildReport
planPackageBuildReport os' arch' comp planPackage = case planPackage of
  PreExisting _     -> Nothing
  Configured  _     -> Nothing
  Installed pkg     -> Just $ buildReport os' arch' comp pkg BR.BuildOk
  Failed pkg result -> Just $ buildReport os' arch' comp pkg result
