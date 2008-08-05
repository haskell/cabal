-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Reporting
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
module Distribution.Client.Reporting (
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

import Distribution.Client.Types
         ( ConfiguredPackage(..), AvailablePackage(..), BuildResult
         , AvailablePackageSource(..), Repo(..), RemoteRepo(..) )
import qualified Distribution.Client.Types as BR
         ( BuildResult, BuildFailure(..), BuildSuccess(..)
         , DocsResult(..), TestsResult(..) )
import qualified Distribution.Client.InstallPlan as InstallPlan
import Distribution.Client.InstallPlan
         ( InstallPlan, PlanPackage )
import Distribution.Client.ParseUtils
         ( parseFields )
import qualified Paths_cabal_install (version)

import Distribution.Package
         ( PackageIdentifier(PackageIdentifier), Package(packageId) )
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
         ( FieldDescr(..), ParseResult(..), simpleField, listField, ppFields )
import qualified Distribution.Compat.ReadP as Parse
         ( ReadP, pfail, munch1, char, option, skipSpaces )
import Text.PrettyPrint.HughesPJ as Disp
         ( Doc, render, char, text, (<+>), (<>) )
import Distribution.Simple.Utils
         ( comparing, equating )

import Data.List
         ( unfoldr, groupBy, sortBy )
import Data.Maybe
         ( catMaybes )
import Data.Char as Char
         ( isAlpha, isAlphaNum )
import System.FilePath
         ( (</>) )

data BuildReport
   = BuildReport {
    -- | The package this build report is about
    package         :: PackageIdentifier,

    -- | The OS and Arch the package was built on
    os              :: OS,
    arch            :: Arch,

    -- | The Haskell compiler (and hopefully version) used
    compiler        :: CompilerId,

    -- | The uploading client, ie cabal-install-x.y.z
    client          :: PackageIdentifier,

    -- | Which configurations flags we used
    flagAssignment  :: FlagAssignment,

    -- | Which dependent packages we were using exactly
    dependencies    :: [PackageIdentifier],

    -- | Did installing work ok?
    installOutcome  :: InstallOutcome,

    --   Which version of the Cabal library was used to compile the Setup.hs
--    cabalVersion    :: Version,

    --   Which build tools we were using (with versions)
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

writeBuildReports :: [(BuildReport, Repo)] -> IO ()
writeBuildReports reports = sequence_
  [ appendFile file (concatMap format reports')
  | (repo, reports') <- separate reports
  , let file = repoLocalDir repo </> "build-reports.log" ]
  --TODO: make this concurrency safe, either lock the report file or make sure
  -- the writes for each report are atomic (under 4k and flush at boundaries)

  where
    format r = '\n' : showBuildReport r ++ "\n"
    separate :: [(BuildReport, Repo)]
             -> [(Repo, [BuildReport])]
    separate = map (\rs@((_,repo,_):_) -> (repo, [ r | (r,_,_) <- rs ]))
             . map concat
             . groupBy (equating (repoName . head))
             . sortBy (comparing (repoName . head))
             . groupBy (equating repoName)
             . onlyRemote
    repoName (_,_,rrepo) = remoteRepoName rrepo

    onlyRemote :: [(BuildReport, Repo)] -> [(BuildReport, Repo, RemoteRepo)]
    onlyRemote rs =
      [ (report, repo, remoteRepo)
      | (report, repo@Repo { repoKind = Left remoteRepo }) <- rs ]

buildReport :: OS -> Arch -> CompilerId -- -> Version
            -> ConfiguredPackage -> BR.BuildResult
            -> BuildReport
buildReport os' arch' comp (ConfiguredPackage pkg flags deps) result =
  BuildReport {
    package               = packageId pkg,
    os                    = os',
    arch                  = arch',
    compiler              = comp,
    client                = cabalInstallID,
    flagAssignment        = flags,
    dependencies          = deps,
    installOutcome        = convertInstallOutcome,
--    cabalVersion          = undefined
    docsOutcome           = convertDocsOutcome,
    testsOutcome          = convertTestsOutcome
  }
  where
    cabalInstallID =
      PackageIdentifier "cabal-install" Paths_cabal_install.version

    convertInstallOutcome = case result of
      Left  (BR.DependentFailed p) -> DependencyFailed p
      Left  (BR.UnpackFailed    _) -> UnpackFailed
      Left  (BR.ConfigureFailed _) -> ConfigureFailed
      Left  (BR.BuildFailed     _) -> BuildFailed
      Left  (BR.InstallFailed   _) -> InstallFailed
      Right (BR.BuildOk       _ _) -> InstallOk
    convertDocsOutcome = case result of
      Left _                                -> NotTried
      Right (BR.BuildOk BR.DocsNotTried _)  -> NotTried
      Right (BR.BuildOk BR.DocsFailed _)    -> Failed
      Right (BR.BuildOk BR.DocsOk _)        -> Ok
    convertTestsOutcome = case result of
      Left _                                -> NotTried
      Right (BR.BuildOk _ BR.TestsNotTried) -> NotTried
      Right (BR.BuildOk _ BR.TestsFailed)   -> Failed
      Right (BR.BuildOk _ BR.TestsOk)       -> Ok

-- ------------------------------------------------------------
-- * External format
-- ------------------------------------------------------------

initialBuildReport :: BuildReport
initialBuildReport = BuildReport {
    package         = requiredField "package",
    os              = requiredField "os",
    arch            = requiredField "arch",
    compiler        = requiredField "compiler",
    client          = requiredField "client",
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
parseBuildReport = parseFields fieldDescrs initialBuildReport

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
showBuildReport br = Disp.render (ppFields br fieldDescrs)

-- -----------------------------------------------------------------------------
-- Description of the fields, for parsing/printing

fieldDescrs :: [FieldDescr BuildReport]
fieldDescrs =
 [ simpleField "package"         disp           parse
                                 package        (\v r -> r { package = v })
 , simpleField "os"              disp           parse
                                 os             (\v r -> r { os = v })
 , simpleField "arch"            disp           parse
                                 arch           (\v r -> r { arch = v })
 , simpleField "compiler"        disp           parse
                                 compiler       (\v r -> r { compiler = v })
 , simpleField "client"          disp           parse
                                 client         (\v r -> r { client = v })
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
dispFlag (FlagName name, True)  =                  Disp.text name
dispFlag (FlagName name, False) = Disp.char '-' <> Disp.text name

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

-- ------------------------------------------------------------
-- * InstallPlan support
-- ------------------------------------------------------------

writeInstallPlanBuildReports :: InstallPlan -> IO ()
writeInstallPlanBuildReports = writeBuildReports . installPlanBuildReports

installPlanBuildReports :: InstallPlan -> [(BuildReport, Repo)]
installPlanBuildReports plan = catMaybes
                             . map (planPackageBuildReport os' arch' comp)
                             . InstallPlan.toList
                             $ plan
  where os'   = InstallPlan.planOS plan
        arch' = InstallPlan.planArch plan
        comp  = InstallPlan.planCompiler plan

planPackageBuildReport :: OS -> Arch -> CompilerId
                       -> InstallPlan.PlanPackage
                       -> Maybe (BuildReport, Repo)
planPackageBuildReport os' arch' comp planPackage = case planPackage of

  InstallPlan.Installed pkg@(ConfiguredPackage (AvailablePackage {
                          packageSource = RepoTarballPackage repo }) _ _) result
    -> Just $ (buildReport os' arch' comp pkg (Right result), repo)

  InstallPlan.Failed pkg@(ConfiguredPackage (AvailablePackage {
                       packageSource = RepoTarballPackage repo }) _ _) result
    -> Just $ (buildReport os' arch' comp pkg (Left result), repo)

  _ -> Nothing
