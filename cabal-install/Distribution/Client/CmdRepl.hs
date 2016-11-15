{-# LANGUAGE NamedFieldPuns #-}

-- | cabal-install CLI command: repl
--
module Distribution.Client.CmdRepl (
    replCommand,
    replAction,
  ) where

import Distribution.Client.ProjectOrchestration
import Distribution.Client.ProjectConfig
         ( BuildTimeSettings(..) )
import Distribution.Client.BuildTarget
         ( readUserBuildTargets )

import Distribution.Client.Setup
         ( GlobalFlags, ConfigFlags(..), ConfigExFlags, InstallFlags )
import Distribution.Simple.Setup
         ( HaddockFlags, fromFlagOrDefault )
import Distribution.Verbosity
         ( normal )

import qualified Data.Map as Map
import Control.Monad (when)

import Distribution.Simple.Command
         ( CommandUI(..), usageAlternatives )
import Distribution.Simple.Utils
         ( wrapText, die' )
import qualified Distribution.Client.Setup as Client

replCommand :: CommandUI (ConfigFlags, ConfigExFlags, InstallFlags, HaddockFlags)
replCommand = Client.installCommand {
  commandName         = "new-repl",
  commandSynopsis     = "Open a REPL for the current project",
  commandUsage        = usageAlternatives "new-repl" [ "[FLAGS] TARGET" ],
  commandDescription  = Just $ \_ -> wrapText $
        "Opens a REPL for a Nix-local build project.",
  commandNotes        = Just $ \pname ->
        "Examples:\n"
     ++ "  " ++ pname ++ " new-repl cname"
     ++ "    Open a REPL for the component named cname\n"
     ++ "  " ++ pname ++ " new-repl pkgname:cname"
     ++ "    Open a REPL for the component named cname in pkgname\n"
   }

-- | The @repl@ command is very much like @build@. It brings the install plan
-- up to date, selects that part of the plan needed by the given or implicit
-- repl target and then executes the plan.
--
-- Compared to @build@ the difference is that only one target is allowed
-- (given or implicit) and the target type is repl rather than build. The
-- general plan execution infrastructure handles both build and repl targets.
--
-- For more details on how this works, see the module
-- "Distribution.Client.ProjectOrchestration"
--
replAction :: (ConfigFlags, ConfigExFlags, InstallFlags, HaddockFlags)
              -> [String] -> GlobalFlags -> IO ()
replAction (configFlags, configExFlags, installFlags, haddockFlags)
           targetStrings globalFlags = do

    userTargets <- readUserBuildTargets verbosity targetStrings

    buildCtx <-
      runProjectPreBuildPhase
        verbosity
        ( globalFlags, configFlags, configExFlags
        , installFlags, haddockFlags )
        PreBuildHooks {
          hookPrePlanning      = \_ _ _ -> return (),

          hookSelectPlanSubset = \buildSettings elaboratedPlan -> do
            when (buildSettingOnlyDeps buildSettings) $
              die' verbosity $ "The repl command does not support '--only-dependencies'. "
                 ++ "You may wish to use 'build --only-dependencies' and then "
                 ++ "use 'repl'."

            -- Interpret the targets on the command line as repl targets
            -- (as opposed to say build or haddock targets).
            targets <- either reportReplTargetProblems return
                   =<< resolveTargets
                         selectPackageTargets
                         selectComponentTarget
                         TargetProblemCommon
                         elaboratedPlan
                         userTargets

            -- Reject multiple targets, or at least targets spanning multiple
            -- components. It is ok to have two module/file targets in the
            -- same component, but not two that live in different components.
            when (Map.size targets > 1) $
              let problem = TargetsMultiple (Map.elems targets)
               in reportReplTargetProblems [problem]

            --TODO: [required eventually] handle no targets case
            when (Map.null targets) $
              fail "TODO handle no targets case"

            let elaboratedPlan' = pruneInstallPlanToTargets
                                    TargetActionRepl
                                    targets
                                    elaboratedPlan
            return elaboratedPlan'
        }

    printPlan verbosity buildCtx

    buildOutcomes <- runProjectBuildPhase verbosity buildCtx
    runProjectPostBuildPhase verbosity buildCtx buildOutcomes
  where
    verbosity = fromFlagOrDefault normal (configVerbosity configFlags)

-- For repl: select the library if there is one and it's buildable,
-- or select the exe if there is only one and it's buildable.
--
-- Fail if there are no buildable lib/exe components, or if there are
-- multiple libs or exes.
--
selectPackageTargets  :: BuildTarget PackageId
                      -> [AvailableTarget k] -> Either ReplTargetProblem [k]
selectPackageTargets _bt ts
  | [libt] <- libts    = Right [libt]
  | (_:_)  <- libts    = Left TargetPackageMultipleLibs

  | [exet] <- exets    = Right [exet]
  | (_:_)  <- exets    = Left TargetPackageMultipleExes

  | (_:_)  <- alllibts = Left TargetPackageNoBuildableLibs
  | (_:_)  <- allexets = Left TargetPackageNoBuildableExes
  | otherwise          = Left TargetPackageNoTargets
  where
    alllibts = [ t | t@(AvailableTarget CLibName _) <- ts ]
    libts    = [ k | TargetBuildable k _ <- map availableTargetStatus alllibts ]
    allexets = [ t | t@(AvailableTarget (CExeName _) _) <- ts ]
    exets    = [ k | TargetBuildable k _ <- map availableTargetStatus allexets ]


-- For checking an individual component target, for build there's no
-- additional checks we need beyond the basic ones.
--
selectComponentTarget :: BuildTarget PackageId
                      -> AvailableTarget k -> Either ReplTargetProblem k
selectComponentTarget bt =
    either (Left . TargetProblemCommon) Right
  . selectComponentTargetBasic bt

data ReplTargetProblem =
     TargetPackageMultipleLibs
   | TargetPackageMultipleExes
   | TargetPackageNoBuildableLibs
   | TargetPackageNoBuildableExes
   | TargetPackageNoTargets
   | TargetProblemCommon TargetProblem
   | TargetsMultiple [[ComponentTarget]] --TODO: more detail needed
  deriving Show

reportReplTargetProblems :: [ReplTargetProblem] -> IO a
reportReplTargetProblems = fail . show
