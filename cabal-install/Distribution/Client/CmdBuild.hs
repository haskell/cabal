-- | cabal-install CLI command: build
--
module Distribution.Client.CmdBuild (
    buildCommand,
    buildAction,
  ) where

import Distribution.Client.ProjectOrchestration

import Distribution.Client.Setup
         ( GlobalFlags, ConfigFlags(..), ConfigExFlags, InstallFlags )
import qualified Distribution.Client.Setup as Client
import Distribution.Simple.Setup
         ( HaddockFlags, fromFlagOrDefault )
import Distribution.Simple.Command
         ( CommandUI(..), usageAlternatives )
import Distribution.Package
         ( packageId )
import Distribution.Verbosity
         ( Verbosity, normal )
import Distribution.Text
         ( display )
import Distribution.Simple.Utils
         ( wrapText, die' )

import Data.List (nub, intercalate)
import qualified Data.Map as Map
import Control.Monad (when)


buildCommand :: CommandUI (ConfigFlags, ConfigExFlags, InstallFlags, HaddockFlags)
buildCommand = Client.installCommand {
  commandName         = "new-build",
  commandSynopsis     = "Compile targets within the project.",
  commandUsage        = usageAlternatives "new-build" [ "[TARGETS] [FLAGS]" ],
  commandDescription  = Just $ \_ -> wrapText $
        "Build one or more targets from within the project. The available "
     ++ "targets are the packages in the project as well as individual "
     ++ "components within those packages, including libraries, executables, "
     ++ "test-suites or benchmarks. Targets can be specified by name or "
     ++ "location. If no target is specified then the default is to build "
     ++ "the package in the current directory.\n\n"

     ++ "Dependencies are built or rebuilt as necessary. Additional "
     ++ "configuration flags can be specified on the command line and these "
     ++ "extend the project configuration from the 'cabal.project', "
     ++ "'cabal.project.local' and other files.",
  commandNotes        = Just $ \pname ->
        "Examples:\n"
     ++ "  " ++ pname ++ " new-build\n"
     ++ "    Build the package in the current directory or all packages in the project\n"
     ++ "  " ++ pname ++ " new-build pkgname\n"
     ++ "    Build the package named pkgname in the project\n"
     ++ "  " ++ pname ++ " new-build ./pkgfoo\n"
     ++ "    Build the package in the ./pkgfoo directory\n"
     ++ "  " ++ pname ++ " new-build cname\n"
     ++ "    Build the component named cname in the project\n"
     ++ "  " ++ pname ++ " new-build cname --enable-profiling\n"
     ++ "    Build the component in profiling mode (including dependencies as needed)\n\n"

     ++ cmdCommonHelpTextNewBuildBeta
   }


-- | The @build@ command does a lot. It brings the install plan up to date,
-- selects that part of the plan needed by the given or implicit targets and
-- then executes the plan.
--
-- For more details on how this works, see the module
-- "Distribution.Client.ProjectOrchestration"
--
buildAction :: (ConfigFlags, ConfigExFlags, InstallFlags, HaddockFlags)
            -> [String] -> GlobalFlags -> IO ()
buildAction (configFlags, configExFlags, installFlags, haddockFlags)
            targetStrings globalFlags = do

    baseCtx <- establishProjectBaseContext verbosity cliConfig

    targetSelectors <- either (reportTargetSelectorProblems verbosity) return
                   =<< readTargetSelectors (localPackages baseCtx) targetStrings

    buildCtx <-
      runProjectPreBuildPhase verbosity baseCtx $ \elaboratedPlan -> do

            -- Interpret the targets on the command line as build targets
            -- (as opposed to say repl or haddock targets).
            targets <- either (reportTargetProblems verbosity) return
                     $ resolveTargets
                         selectPackageTargets
                         selectComponentTarget
                         TargetProblemCommon
                         elaboratedPlan
                         targetSelectors

            --TODO: [required eventually] handle no targets case
            when (Map.null targets) $
              fail "TODO handle no targets case"

            let elaboratedPlan' = pruneInstallPlanToTargets
                                    TargetActionBuild
                                    targets
                                    elaboratedPlan
            elaboratedPlan'' <-
              if buildSettingOnlyDeps (buildSettings baseCtx)
                then either (reportCannotPruneDependencies verbosity) return $
                     pruneInstallPlanToDependencies (Map.keysSet targets)
                                                    elaboratedPlan'
                else return elaboratedPlan'

            return elaboratedPlan''

    printPlan verbosity baseCtx buildCtx

    buildOutcomes <- runProjectBuildPhase verbosity baseCtx buildCtx
    runProjectPostBuildPhase verbosity baseCtx buildCtx buildOutcomes
  where
    verbosity = fromFlagOrDefault normal (configVerbosity configFlags)
    cliConfig = commandLineFlagsToProjectConfig
                  globalFlags configFlags configExFlags
                  installFlags haddockFlags

-- For build: select all components except non-buildable and disabled
-- tests/benchmarks, fail if there are no such components
--
selectPackageTargets :: TargetSelector PackageId
                     -> [AvailableTarget k] -> Either TargetProblem [k]
selectPackageTargets bt ts
  | (_:_)  <- enabledts = Right enabledts
  | (_:_)  <- ts        = Left TargetPackageNoEnabledTargets -- allts
  | otherwise           = Left TargetPackageNoTargets
  where
    enabledts = [ k | TargetBuildable k requestedByDefault
                        <- map availableTargetStatus ts
                    , pruneReq bt requestedByDefault ]

    -- When there's a target filter like "pkg:tests" then we do select tests,
    -- but if it's just a target like "pkg" then we don't build tests unless
    -- they are requested by default (i.e. by using --enable-tests)
    pruneReq (TargetPackage _ _  Nothing) TargetNotRequestedByDefault = False
    pruneReq (TargetAllPackages  Nothing) TargetNotRequestedByDefault = False
    pruneReq _ _ = True

-- For checking an individual component target, for build there's no
-- additional checks we need beyond the basic ones.
--
selectComponentTarget :: PackageId -> ComponentName -> SubComponentTarget
                      -> AvailableTarget k -> Either TargetProblem k
selectComponentTarget pkgid cname subtarget =
    either (Left . TargetProblemCommon) Right
  . selectComponentTargetBasic pkgid cname subtarget

data TargetProblem =
     TargetProblemCommon       TargetProblemCommon
   | TargetPackageNoEnabledTargets
   | TargetPackageNoTargets
  deriving (Eq, Show)

reportTargetProblems :: Verbosity -> [TargetProblem] -> IO a
reportTargetProblems verbosity =
    die' verbosity . unlines . map renderTargetProblem

renderTargetProblem :: TargetProblem -> String
renderTargetProblem = show

reportCannotPruneDependencies :: Verbosity -> CannotPruneDependencies -> IO a
reportCannotPruneDependencies verbosity =
    die' verbosity . renderCannotPruneDependencies

renderCannotPruneDependencies :: CannotPruneDependencies -> String
renderCannotPruneDependencies (CannotPruneDependencies brokenPackages) =
      "Cannot select only the dependencies (as requested by the "
   ++ "'--only-dependencies' flag), "
   ++ (case pkgids of
          [pkgid] -> "the package " ++ display pkgid ++ " is "
          _       -> "the packages "
                     ++ intercalate ", " (map display pkgids) ++ " are ")
   ++ "required by a dependency of one of the other targets."
  where
    -- throw away the details and just list the deps that are needed
    pkgids :: [PackageId]
    pkgids = nub . map packageId . concatMap snd $ brokenPackages

{-
           ++ "Syntax:\n"
           ++ " - build [package]\n"
           ++ " - build [package:]component\n"
           ++ " - build [package:][component:]module\n"
           ++ " - build [package:][component:]file\n"
           ++ " where\n"
           ++ "  package is a package name, package dir or .cabal file\n\n"
           ++ "Examples:\n"
           ++ " - build foo            -- package name\n"
           ++ " - build tests          -- component name\n"
           ++ "    (name of library, executable, test-suite or benchmark)\n"
           ++ " - build Data.Foo       -- module name\n"
           ++ " - build Data/Foo.hsc   -- file name\n\n"
           ++ "An ambigious target can be qualified by package, component\n"
           ++ "and/or component kind (lib|exe|test|bench|flib)\n"
           ++ " - build foo:tests      -- component qualified by package\n"
           ++ " - build tests:Data.Foo -- module qualified by component\n"
           ++ " - build lib:foo        -- component qualified by kind"
-}
