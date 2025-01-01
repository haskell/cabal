{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Distribution.Client.CmdTarget
  ( targetCommand
  , targetAction
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import qualified Data.Map as Map
import Distribution.Client.CmdBuild (selectComponentTarget, selectPackageTargets)
import Distribution.Client.CmdErrorMessages
import Distribution.Client.InstallPlan
import qualified Distribution.Client.InstallPlan as InstallPlan
import Distribution.Client.NixStyleOptions
  ( NixStyleFlags (..)
  , defaultNixStyleFlags
  )
import Distribution.Client.ProjectOrchestration
import Distribution.Client.ProjectPlanning
import Distribution.Client.Setup
  ( ConfigFlags (..)
  , GlobalFlags
  )
import Distribution.Client.TargetProblem
  ( TargetProblem'
  )
import Distribution.Package
import Distribution.Simple.Command
  ( CommandUI (..)
  , usageAlternatives
  )
import Distribution.Simple.Flag (fromFlagOrDefault)
import Distribution.Simple.Utils
  ( safeHead
  , wrapText
  )
import Distribution.Verbosity
  ( normal
  )
import Text.PrettyPrint
import qualified Text.PrettyPrint as Pretty

-------------------------------------------------------------------------------
-- Command
-------------------------------------------------------------------------------

targetCommand :: CommandUI (NixStyleFlags ())
targetCommand =
  CommandUI
    { commandName = "v2-target"
    , commandSynopsis = "Disclose selected targets."
    , commandUsage = usageAlternatives "v2-target" ["[TARGETS]"]
    , commandDescription =
        Just . const . render $
          vcat
            [ intro
            , vcat $ punctuate (text "\n") [targetForms, ctypes, Pretty.empty]
            ]
    , commandNotes = Just $ \pname -> render $ examples pname
    , commandDefaultFlags = defaultNixStyleFlags ()
    , commandOptions = const []
    }
  where
    intro =
      text . wrapText $
        "Discover targets in a project for use with other commands taking [TARGETS].\n\n"
          ++ "Discloses fully-qualified targets from a selection of target form"
          ++ " [TARGETS] (or 'all' if none given).  Can also check if a target form is"
          ++ " unique as some commands require a unique TARGET."

    targetForms =
      vcat
        [ text "A [TARGETS] item can be one of these target forms:"
        , nest 1 . vcat $
            (char '-' <+>)
              <$> [ text "a package target (e.g. [pkg:]package)"
                  , text "a component target (e.g. [package:][ctype:]component)"
                  , text "all packages (e.g. all)"
                  , text "components of a particular type (e.g. package:ctypes or all:ctypes)"
                  , text "a module target: (e.g. [package:][ctype:]module)"
                  , text "a filepath target: (e.g. [package:][ctype:]filepath)"
                  ]
        ]

    ctypes =
      vcat
        [ text "The ctypes can be one of:"
        , nest 1 . vcat $
            (char '-' <+>)
              <$> [ "libs" <+> parens "libraries"
                  , "exes" <+> parens "executables"
                  , "tests"
                  , "benches" <+> parens "benchmarks"
                  , "flibs" <+> parens "foreign-libraries"
                  ]
        ]

    examples pname =
      vcat
        [ text "Examples" Pretty.<> colon
        , nest 2 $
            vcat
              [ vcat
                  [ text pname <+> text "v2-target all"
                  , nest 2 $ text "Targets of the package in the current directory or all packages in the project"
                  ]
              , vcat
                  [ text pname <+> text "v2-target pkgname"
                  , nest 2 $ text "Targets of the package named pkgname in the project"
                  ]
              , vcat
                  [ text pname <+> text "v2-target ./pkgfoo"
                  , nest 2 $ text "Targets of the package in the ./pkgfoo directory"
                  ]
              , vcat
                  [ text pname <+> text "v2-target cname"
                  , nest 2 $ text "Targets of the component named cname in the project"
                  ]
              ]
        ]

-------------------------------------------------------------------------------
-- Action
-------------------------------------------------------------------------------

targetAction :: NixStyleFlags () -> [String] -> GlobalFlags -> IO ()
targetAction flags@NixStyleFlags{..} ts globalFlags = do
  ProjectBaseContext
    { distDirLayout
    , cabalDirLayout
    , projectConfig
    , localPackages
    } <-
    establishProjectBaseContext verbosity cliConfig OtherCommand

  (_, elaboratedPlan, _, _, _) <-
    rebuildInstallPlan
      verbosity
      distDirLayout
      cabalDirLayout
      projectConfig
      localPackages
      Nothing

  targetSelectors <-
    either (reportTargetSelectorProblems verbosity) return
      =<< readTargetSelectors localPackages Nothing targetStrings

  targets :: TargetsMap <-
    either (reportBuildTargetProblems verbosity) return $
      resolveTargets
        selectPackageTargets
        selectComponentTarget
        elaboratedPlan
        Nothing
        targetSelectors

  printTargetForms targets elaboratedPlan
  where
    verbosity = fromFlagOrDefault normal (configVerbosity configFlags)
    targetStrings = if null ts then ["all"] else ts
    cliConfig =
      commandLineFlagsToProjectConfig
        globalFlags
        flags
        mempty

reportBuildTargetProblems :: Verbosity -> [TargetProblem'] -> IO a
reportBuildTargetProblems verbosity = reportTargetProblems verbosity "target"

printTargetForms :: TargetsMap -> ElaboratedInstallPlan -> IO ()
printTargetForms targets elaboratedPlan =
  putStrLn . render $
    vcat
      [ text "Fully qualified target forms" Pretty.<> colon
      , nest 1 $ vcat [text "-" <+> text tf | tf <- targetForms]
      ]
  where
    localPkgs =
      [x | Configured x@ElaboratedConfiguredPackage{elabLocalToProject = True} <- InstallPlan.toList elaboratedPlan]

    targetForm ct x =
      let pkgId@PackageIdentifier{pkgName = n} = elabPkgSourceId x
       in render $ pretty n Pretty.<> colon Pretty.<> text (showComponentTarget pkgId ct)

    targetForms =
      sort $
        catMaybes
          [ targetForm ct <$> pkg
          | (u :: UnitId, xs) <- Map.toAscList targets
          , let pkg = safeHead $ filter ((== u) . elabUnitId) localPkgs
          , (ct :: ComponentTarget, _) <- xs
          ]
