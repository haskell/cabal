{-# LANGUAGE NamedFieldPuns #-}

-- | cabal-install CLI command: bench
--
module Distribution.Client.CmdBench (
    benchCommand,
    benchAction,
  ) where

import Distribution.Client.ProjectOrchestration

import Distribution.Client.Setup
         ( GlobalFlags, ConfigFlags(..), ConfigExFlags, InstallFlags )
import qualified Distribution.Client.Setup as Client
import Distribution.Simple.Setup
         ( HaddockFlags, fromFlagOrDefault )
import Distribution.Simple.Command
         ( CommandUI(..), usageAlternatives )
import Distribution.Verbosity
         ( normal )
import Distribution.Simple.Utils
         ( wrapText, die' )

import qualified Data.Map as Map
import Control.Monad (when)


benchCommand :: CommandUI (ConfigFlags, ConfigExFlags, InstallFlags, HaddockFlags)
benchCommand = Client.installCommand {
  commandName         = "new-bench",
  commandSynopsis     = "Run benchmarks",
  commandUsage        = usageAlternatives "new-bench" [ "[TARGETS] [FLAGS]" ],
  commandDescription  = Just $ \_ -> wrapText $
        "Runs the specified benchmarks, first ensuring they are up to "
     ++ "date.\n\n"

     ++ "Any benchmark in any package in the project can be specified. "
     ++ "A package can be specified in which case all the benchmarks in the "
     ++ "package are run. The default is to run all the benchmarks in the "
     ++ "package in the current directory.\n\n"

     ++ "Dependencies are built or rebuilt as necessary. Additional "
     ++ "configuration flags can be specified on the command line and these "
     ++ "extend the project configuration from the 'cabal.project', "
     ++ "'cabal.project.local' and other files.",
  commandNotes        = Just $ \pname ->
        "Examples:\n"
     ++ "  " ++ pname ++ " new-bench\n"
     ++ "    Run all the benchmarks in the package in the current directory\n"
     ++ "  " ++ pname ++ " new-bench pkgname\n"
     ++ "    Run all the benchmarks in the package named pkgname\n"
     ++ "  " ++ pname ++ " new-bench cname\n"
     ++ "    Run the benchmark named cname\n"
     ++ "  " ++ pname ++ " new-bench cname -O2\n"
     ++ "    Run the benchmark built with '-O2' (including local libs used)\n\n"

     ++ "Note: this command is part of the new project-based system (aka "
     ++ "nix-style\nlocal builds). These features are currently in beta. "
     ++ "Please see\n"
     ++ "http://cabal.readthedocs.io/en/latest/nix-local-build-overview.html "
     ++ "for\ndetails and advice on what you can expect to work. If you "
     ++ "encounter problems\nplease file issues at "
     ++ "https://github.com/haskell/cabal/issues and if you\nhave any time "
     ++ "to get involved and help with testing, fixing bugs etc then\nthat "
     ++ "is very much appreciated.\n"
   }


-- | The @build@ command does a lot. It brings the install plan up to date,
-- selects that part of the plan needed by the given or implicit targets and
-- then executes the plan.
--
-- For more details on how this works, see the module
-- "Distribution.Client.ProjectOrchestration"
--
benchAction :: (ConfigFlags, ConfigExFlags, InstallFlags, HaddockFlags)
            -> [String] -> GlobalFlags -> IO ()
benchAction (configFlags, configExFlags, installFlags, haddockFlags)
            targetStrings globalFlags = do

    baseCtx <- establishProjectBaseContext verbosity cliConfig
                                           configFlags installFlags --TODO: eliminate use of legacy config types

    targetSelectors <- either (reportTargetSelectorProblems verbosity) return
                   =<< readTargetSelectors (localPackages baseCtx) targetStrings

    buildCtx <-
      runProjectPreBuildPhase verbosity baseCtx $ \elaboratedPlan -> do

            when (buildSettingOnlyDeps (buildSettings baseCtx)) $
              die' verbosity $
                  "The bench command does not support '--only-dependencies'. "
               ++ "You may wish to use 'build --only-dependencies' and then "
               ++ "use 'bench'."

            -- Interpret the targets on the command line as bench targets
            -- (as opposed to say build or haddock targets).
            targets <- either reportBenchTargetProblems return
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
            return elaboratedPlan'

    printPlan verbosity baseCtx buildCtx

    buildOutcomes <- runProjectBuildPhase verbosity baseCtx buildCtx
    runProjectPostBuildPhase verbosity baseCtx buildCtx buildOutcomes
  where
    verbosity = fromFlagOrDefault normal (configVerbosity configFlags)
    cliConfig = commandLineFlagsToProjectConfig
                  globalFlags configFlags configExFlags
                  installFlags haddockFlags

-- For bench: select all buildable benchmarks
-- Fail if there are no benchmarks or no buildable benchmarks.
--
selectPackageTargets :: TargetSelector PackageId
                     -> [AvailableTarget k] -> Either BenchTargetProblem [k]
selectPackageTargets _bt ts
  | (_:_)  <- benchts    = Right benchts
  | (_:_)  <- allbenchts = Left (TargetPackageNoEnabledBenchmarks allbenchts')
  | otherwise            = Left (TargetPackageNoBenchmarks        allbenchts')
  where
    allbenchts = [ t | t@(AvailableTarget (CBenchName _) _ _) <- ts ]
    benchts    = [ k | TargetBuildable k _
                         <- map availableTargetStatus allbenchts ]
    allbenchts'= [ fmap (const ()) t | t <- allbenchts ]


selectComponentTarget :: TargetSelector PackageId
                      -> AvailableTarget k -> Either BenchTargetProblem  k
selectComponentTarget bt t
  | CBenchName _ <- availableTargetComponentName t
  = either (Left . TargetProblemCommon) return $
           selectComponentTargetBasic bt t
  | otherwise
  = Left (TargetComponentNotBenchmark (fmap (const ()) t))

data BenchTargetProblem =
     TargetPackageNoEnabledBenchmarks [AvailableTarget ()]
   | TargetPackageNoBenchmarks        [AvailableTarget ()]
   | TargetComponentNotBenchmark      (AvailableTarget ())
   | TargetProblemCommon               TargetProblem
  deriving Show

reportBenchTargetProblems :: [BenchTargetProblem] -> IO a
reportBenchTargetProblems = fail . show
