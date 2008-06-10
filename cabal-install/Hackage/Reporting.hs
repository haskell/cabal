{-# OPTIONS_GHC -fno-warn-orphans #-}
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
    Outcome(..),

    -- * Constructing and writing reports
    buildReport,
    writeBuildReports,

    -- * parsing and pretty printing
    parseBuildReport,
    parseBuildReports,
    showBuildReport,

    -- * 'InstallPlan' variants
    planPackageBuildReport,
    installPlanBuildReports,
    writeInstallPlanBuildReports
  ) where

import Hackage.Types
         ( ConfiguredPackage(..), AvailablePackage(..)
         , AvailablePackageSource(..), repoURI, BuildResult )
import qualified Hackage.Types as BR
         ( BuildResult(..) )
import qualified Hackage.InstallPlan as InstallPlan
import Hackage.InstallPlan
         ( InstallPlan, PlanPackage )
import Hackage.Config
         ( defaultBuildReportFile )
import Hackage.ParseUtils
         ( showFields, parseBasicStanza )

import Distribution.Package
         ( PackageIdentifier, Package(packageId) )
import Distribution.PackageDescription
         ( FlagName(..), FlagAssignment )
--import Distribution.Version
--         ( Version )
import Distribution.System
         ( OS, Arch )
import Distribution.Compiler
         ( CompilerId )
import Distribution.Text
         ( Text(disp, parse) )
import Distribution.ParseUtils
         ( FieldDescr(..), ParseResult(..), simpleField, listField )
import qualified Distribution.Compat.ReadP as Parse
         ( ReadP, pfail, munch1, char, option, skipSpaces )
import Text.PrettyPrint.HughesPJ as Disp
         ( Doc, char, text, (<+>), (<>) )

import Data.List
         ( unfoldr )
import Data.Maybe
         ( catMaybes )
import Data.Char as Char
         ( isAlpha, isAlphaNum )
import Network.URI
         ( URI, uriToString, parseAbsoluteURI )

data BuildReport
   = BuildReport {
    -- | The package this build report is about
    package         :: PackageIdentifier,

    -- | Which hackage server this package is from and thus which server this
    -- report should be sent to.
    server          :: URI,

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
    docsOutcome     :: Outcome,

    -- | Configure outcome, did configure work ok?
    testsOutcome    :: Outcome
  }

data InstallOutcome
   = DependencyFailed PackageIdentifier
   | DownloadFailed
   | UnpackFailed
   | SetupFailed
   | ConfigureFailed
   | BuildFailed
   | InstallFailed
   | InstallOk

data Outcome = NotTried | Failed | Ok

writeBuildReports :: [BuildReport] -> IO ()
writeBuildReports reports = do
  file <- defaultBuildReportFile
  appendFile file (concatMap (("\n\n"++) . showBuildReport) reports)

buildReport :: OS -> Arch -> CompilerId -- -> Version
            -> URI -> ConfiguredPackage -> BR.BuildResult
            -> BuildReport
buildReport os' arch' comp uri (ConfiguredPackage pkg flags deps) result =
  BuildReport {
    package               = packageId pkg,
    os                    = os',
    server                = uri,
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
    docsOutcome           = NotTried,
    testsOutcome          = NotTried
  }

-- ------------------------------------------------------------
-- * External format
-- ------------------------------------------------------------

initialBuildReport :: BuildReport
initialBuildReport = BuildReport {
    package         = requiredField "package",
    server          = requiredField "server",
    os              = requiredField "os",
    arch            = requiredField "arch",
    compiler        = requiredField "compiler",
    flagAssignment  = [],
    dependencies    = [],
    installOutcome  = requiredField "install-outcome",
--    cabalVersion  = Nothing,
--    tools         = [],
    docsOutcome     = NotTried,
    testsOutcome    = NotTried
  }
  where
    requiredField fname = error ("required field: " ++ fname)

-- -----------------------------------------------------------------------------
-- Parsing

parseBuildReport :: String -> ParseResult BuildReport
parseBuildReport = parseBasicStanza fieldDescrs initialBuildReport

parseBuildReports :: String -> [BuildReport]
parseBuildReports str =
  [ report | ParseOk [] report <- map parseBuildReport (split str) ]

  where
    split :: String -> [String]
    split = filter (not . null) . unfoldr chunk . lines
    chunk [] = Nothing
    chunk ls = case break null ls of
                 (r, rs) -> Just (unlines r, dropWhile null rs)

-- -----------------------------------------------------------------------------
-- Pretty-printing

showBuildReport :: BuildReport -> String
showBuildReport = showFields fieldDescrs

-- -----------------------------------------------------------------------------
-- Description of the fields, for parsing/printing

fieldDescrs :: [FieldDescr BuildReport]
fieldDescrs =
 [ simpleField "package"         disp           parse
                                 package        (\v r -> r { package = v })
 , simpleField "server"          disp           parse
                                 server         (\v r -> r { server = v })
 , simpleField "os"              disp           parse
                                 os             (\v r -> r { os = v })
 , simpleField "arch"            disp           parse
                                 arch           (\v r -> r { arch = v })
 , simpleField "compiler"        disp           parse
                                 compiler       (\v r -> r { compiler = v })
 , listField   "flags"           dispFlag       parseFlag
                                 flagAssignment (\v r -> r { flagAssignment = v })
 , listField   "dependencies"    disp           parse
                                 dependencies   (\v r -> r { dependencies = v })
 , simpleField "install-outcome" disp           parse
                                 installOutcome (\v r -> r { installOutcome = v })
 , simpleField "docs-outcome"    disp           parse
                                 docsOutcome    (\v r -> r { docsOutcome = v })
 , simpleField "tests-outcome"   disp           parse
                                 testsOutcome   (\v r -> r { testsOutcome = v })
 ]

dispFlag :: (FlagName, Bool) -> Disp.Doc
dispFlag (FlagName name, True)  = Disp.char '-' <> Disp.text name
dispFlag (FlagName name, False) =                  Disp.text name

parseFlag :: Parse.ReadP r (FlagName, Bool)
parseFlag = do
  value <- Parse.option True (Parse.char '-' >> return False)
  name  <- Parse.munch1 (\c -> Char.isAlphaNum c || c == '_' || c == '-')
  return (FlagName name, value)

instance Text InstallOutcome where
  disp (DependencyFailed pkgid) = Disp.text "DependencyFailed" <+> disp pkgid
  disp DownloadFailed  = Disp.text "DownloadFailed"
  disp UnpackFailed    = Disp.text "UnpackFailed"
  disp SetupFailed     = Disp.text "SetupFailed"
  disp ConfigureFailed = Disp.text "ConfigureFailed"
  disp BuildFailed     = Disp.text "BuildFailed"
  disp InstallFailed   = Disp.text "InstallFailed"
  disp InstallOk       = Disp.text "InstallOk"

  parse = do
    name <- Parse.munch1 Char.isAlphaNum
    case name of
      "DependencyFailed" -> do Parse.skipSpaces
                               pkgid <- parse
                               return (DependencyFailed pkgid)
      "DownloadFailed"   -> return DownloadFailed
      "UnpackFailed"     -> return UnpackFailed
      "SetupFailed"      -> return SetupFailed
      "ConfigureFailed"  -> return ConfigureFailed
      "BuildFailed"      -> return BuildFailed
      "InstallFailed"    -> return InstallFailed
      "InstallOk"        -> return InstallOk
      _                  -> Parse.pfail

instance Text Outcome where
  disp NotTried = Disp.text "NotTried"
  disp Failed   = Disp.text "Failed"
  disp Ok       = Disp.text "Ok"
  parse = do
    name <- Parse.munch1 Char.isAlpha
    case name of
      "NotTried" -> return NotTried
      "Failed"   -> return Failed
      "Ok"       -> return Ok
      _          -> Parse.pfail

instance Text URI where
  disp uri = Disp.text (uriToString id uri [])
  parse = do
    str <- Parse.munch1 (\c -> isAlphaNum c || c `elem` "+-=._/*()@'$:;&!?")
    maybe Parse.pfail return (parseAbsoluteURI str)

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
                       -> InstallPlan.PlanPackage BuildResult
                       -> Maybe BuildReport
planPackageBuildReport os' arch' comp planPackage = case planPackage of

  InstallPlan.Installed pkg@(ConfiguredPackage (AvailablePackage {
                          packageSource = RepoTarballPackage repo }) _ _)
    -> Just $ buildReport os' arch' comp (repoURI repo) pkg BR.BuildOk

  InstallPlan.Failed pkg@(ConfiguredPackage (AvailablePackage {
                       packageSource = RepoTarballPackage repo }) _ _) result
    -> Just $ buildReport os' arch' comp (repoURI repo) pkg result

  _ -> Nothing
