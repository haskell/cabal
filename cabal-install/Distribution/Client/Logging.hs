-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Logging
-- Copyright   :  (c) David Waern 2008
-- License     :  BSD-like
--
-- Maintainer  :  david.waern@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Build log data structure
--
-----------------------------------------------------------------------------
module Distribution.Client.Logging (
    BuildLogEntry(..),
    InstallOutcome(..),
    Outcome(..),

    -- * Constructing and writing reports
    buildLogEntry,
    writeBuildLog,

    -- * parsing and pretty printing
    parseBuildLogEntry,
    parseBuildLog,
    showBuildLogEntry,

    -- * 'InstallPlan' variants
    planPackageBuildLogEntry,
    installPlanBuildLog,
    writeInstallPlanBuildLog
  ) where

import Distribution.Client.Reporting
         ( InstallOutcome(..), Outcome(..) )
import Distribution.Client.Types
         ( ConfiguredPackage(..), BuildResult )
import Distribution.Client.Config
         ( defaultCabalDir )
import qualified Distribution.Client.Types as BR
         ( BuildResult(..) )
import qualified Distribution.Client.InstallPlan as InstallPlan
import Distribution.Client.InstallPlan
         ( InstallPlan, PlanPackage )
import Distribution.Client.ParseUtils
         ( showFields, parseBasicStanza )
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
         ( FieldDescr(..), ParseResult(..), simpleField, listField )
import qualified Distribution.Compat.ReadP as Parse
         ( ReadP, {-pfail,-} munch1, char, option )
import Text.PrettyPrint.HughesPJ as Disp
         ( Doc, char, text, (<>) )

import Data.List
         ( unfoldr )
import Data.Maybe
         ( catMaybes )
import Data.Char as Char
         ( isAlphaNum )
import System.FilePath
         ( (</>) )
--import Network.URI
--         ( URI, uriToString, parseAbsoluteURI )

type BuildLog = [BuildLogEntry]

data BuildLogEntry
   = BuildLogEntry {
    -- | The package this build report is about
    package         :: PackageIdentifier,

    -- | Which hackage server this package is from or local
--    server          :: Maybe URI,

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

    -- | Which version of the Cabal library was used to compile the Setup.hs
--    cabalVersion    :: Version,

    -- | Which build tools we were using (with versions)
--    tools      :: [PackageIdentifier],

    -- | Configure outcome, did configure work ok?
    docsOutcome     :: Outcome,

    -- | Configure outcome, did configure work ok?
    testsOutcome    :: Outcome
  }

writeBuildLog :: BuildLog -> IO ()
writeBuildLog reports = do
  cabalDir <- defaultCabalDir
  let file = cabalDir </> "build.log"
  appendFile file (concatMap format reports)
  --TODO: make this concurrency safe, either lock the report file or make sure
  -- the writes for each report are atomic (under 4k and flush at boundaries)

  where
    format r = '\n' : showBuildLogEntry r ++ "\n"

buildLogEntry :: OS -> Arch -> CompilerId -- -> Version
              -> ConfiguredPackage -> BR.BuildResult
              -> BuildLogEntry
buildLogEntry os' arch' comp (ConfiguredPackage pkg flags deps) result =
  BuildLogEntry {
    package               = packageId pkg,
--    server                = Nothing,
    os                    = os',
    arch                  = arch',
    compiler              = comp,
    client                = cabalInstallID,
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
  where
    cabalInstallID =
      PackageIdentifier "cabal-install" Paths_cabal_install.version

-- ------------------------------------------------------------
-- * External format
-- ------------------------------------------------------------

initialBuildLogEntry :: BuildLogEntry
initialBuildLogEntry = BuildLogEntry {
    package         = requiredField "package",
--    server          = Nothing,
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

parseBuildLogEntry :: String -> ParseResult BuildLogEntry
parseBuildLogEntry = parseBasicStanza fieldDescrs initialBuildLogEntry

parseBuildLog :: String -> [BuildLogEntry]
parseBuildLog str =
  [ report | ParseOk [] report <- map parseBuildLogEntry (split str) ]

  where
    split :: String -> [String]
    split = filter (not . null) . unfoldr chunk . lines
    chunk [] = Nothing
    chunk ls = case break null ls of
                 (r, rs) -> Just (unlines r, dropWhile null rs)

-- -----------------------------------------------------------------------------
-- Pretty-printing

showBuildLogEntry :: BuildLogEntry -> String
showBuildLogEntry = showFields fieldDescrs

-- -----------------------------------------------------------------------------
-- Description of the fields, for parsing/printing

fieldDescrs :: [FieldDescr BuildLogEntry]
fieldDescrs =
 [ simpleField "package"         disp           parse
                                 package        (\v r -> r { package = v })
-- , simpleField "server"          disp           parse
--                                 server         (\v r -> r { server = v })
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

{-
instance Text URI where
  disp uri = Disp.text (uriToString id uri [])
  parse = do
    str <- Parse.munch1 (\c -> isAlphaNum c || c `elem` "+-=._/*()@'$:;&!?")
    maybe Parse.pfail return (parseAbsoluteURI str)
-}
-- ------------------------------------------------------------
-- * InstallPlan support
-- ------------------------------------------------------------

writeInstallPlanBuildLog :: InstallPlan BuildResult -> IO ()
writeInstallPlanBuildLog = writeBuildLog . installPlanBuildLog

installPlanBuildLog :: InstallPlan BuildResult -> BuildLog
installPlanBuildLog plan = catMaybes
                             . map (planPackageBuildLogEntry os' arch' comp)
                             . InstallPlan.toList
                             $ plan
  where os'   = InstallPlan.planOS plan
        arch' = InstallPlan.planArch plan
        comp  = InstallPlan.planCompiler plan

planPackageBuildLogEntry :: OS -> Arch -> CompilerId
                         -> InstallPlan.PlanPackage BuildResult
                         -> Maybe BuildLogEntry
planPackageBuildLogEntry os' arch' comp planPackage = case planPackage of

  InstallPlan.Installed pkg
    -> Just $ buildLogEntry os' arch' comp pkg BR.BuildOk

  InstallPlan.Failed pkg result
    -> Just $ buildLogEntry os' arch' comp pkg result

  _ -> Nothing
