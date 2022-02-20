{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.CmdStatus
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Implementation of the 'status' command. Query for project information
-- such as targets in the project or which ghc version is going to be used
-- to build the project.
-----------------------------------------------------------------------------

module Distribution.Client.CmdStatus (
  statusCommand, statusAction,
  ) where

import Control.Monad
         ( mapM )
import qualified Data.Map as Map

import Prelude ()
import Distribution.Client.Compat.Prelude

import Distribution.Client.TargetProblem
import Distribution.Client.CmdErrorMessages
import qualified Distribution.Client.InstallPlan as InstallPlan
import Distribution.Client.NixStyleOptions
         ( NixStyleFlags (..), nixStyleOptions, defaultNixStyleFlags )
import Distribution.Client.ProjectOrchestration
import Distribution.Client.ProjectPlanning
import Distribution.Client.Setup
         ( GlobalFlags, ConfigFlags(..), yesNoOpt )
import Distribution.Client.Types
         ( PackageSpecifier, PackageLocation )
import Distribution.Client.TargetSelector
         ( TargetSelectorProblem )
import Distribution.Client.Utils.Json
         ( (.=) )
import qualified Distribution.Client.Utils.Json as Json
import Distribution.Client.Version
         ( cabalInstallVersion )

import qualified Distribution.Compat.CharParsing as P
import Distribution.ReadE
         ( ReadE(ReadE), parsecToReadE )
import Distribution.Simple.Command
         ( CommandUI(..), option, reqArg, ShowOrParseArgs, OptionField )
import Distribution.Simple.Compiler
import Distribution.Simple.Program
import Distribution.Simple.Flag
         ( Flag(..), fromFlagOrDefault )
import Distribution.Simple.Utils
         ( wrapText, die', withOutputMarker, ordNub )
import Distribution.Solver.Types.SourcePackage
import Distribution.Types.UnitId
import Distribution.Verbosity
         ( normal )
import Distribution.Version

-------------------------------------------------------------------------------
-- Command
-------------------------------------------------------------------------------

statusCommand :: CommandUI (NixStyleFlags StatusFlags)
statusCommand = CommandUI
  { commandName = "status"
  , commandSynopsis = "Query for simple project information"
  , commandDescription  = Just $ \_ -> wrapText $
      "Query for available targets and project information such as project GHC."
  , commandNotes = Just $ \pname ->
        "Examples:\n"
     ++ "  " ++ pname ++ " status --output-format=json --compiler\n"
     ++ "    Print the compiler that is used for this project in the json format.\n"
     ++ "  " ++ pname ++ " status --output-format=json --target=./src/Foo.hs\n"
     ++ "    Print the unit-id of the component \"src/Foo.hs\" belongs to.\n"
     ++ "  " ++ pname ++ " status --output-format=json --target=./src/Foo.hs\n"
     ++ "    Print both, compiler information and unit-id for the given target.\n"
     ++ "  " ++ pname ++ " status --output-format=json --target=./src/Foo.hs --target=./test/Bar.hs\n"
     ++ "    Print unit-id location for multiple targets.\n"
  , commandUsage = \pname ->
      "Usage: " ++ pname ++ " status [FLAGS]\n"
  , commandDefaultFlags = defaultNixStyleFlags defaultStatusFlags
  , commandOptions      = nixStyleOptions statusOptions

  }

-------------------------------------------------------------------------------
-- Flags
-------------------------------------------------------------------------------

data StatusOutputFormat
  = JSON
  deriving (Eq, Ord, Show, Read)

data StatusFlags = StatusFlags
  { statusTargets :: [String]
  , statusCompiler :: Flag Bool
  , statusOutputFormat :: Flag StatusOutputFormat
  } deriving (Eq, Show, Read)

defaultStatusFlags :: StatusFlags
defaultStatusFlags = StatusFlags
  { statusTargets = mempty
  , statusCompiler = mempty
  , statusOutputFormat = mempty
  }

statusOutputFormatParser :: ReadE (Flag StatusOutputFormat)
statusOutputFormatParser = ReadE $ \case
  "json" -> Right $ Flag JSON
  policy -> Left  $ "Cannot parse the status output format '"
            <> policy <> "'"

statusOutputFormatPrinter
  :: Flag StatusOutputFormat -> [String]
statusOutputFormatPrinter = \case
  (Flag JSON) -> ["json"]
  NoFlag      -> []

statusOptions :: ShowOrParseArgs -> [OptionField StatusFlags]
statusOptions showOrParseArgs =
  [ option [] ["output-format"]
    "Output Format for the information"
    statusOutputFormat (\v flags -> flags { statusOutputFormat = v })
    (reqArg "json"
      statusOutputFormatParser
      statusOutputFormatPrinter
    )
  , option [] ["target"]
    "Given a target, obtain the unit-id in the build-plan"
    statusTargets (\v flags -> flags { statusTargets = v ++ statusTargets flags})
    (reqArg "TARGET" buildInfoTargetReadE id)
  , option [] ["compiler"]
    "Print information of the project compiler"
    statusCompiler (\v flags -> flags { statusCompiler = v })
    (yesNoOpt showOrParseArgs)
  ]
  where
    buildInfoTargetReadE :: ReadE [String]
    buildInfoTargetReadE =
      fmap pure $ parsecToReadE
        -- This error should never be shown
        ("couldn't parse targets: " ++)
        (P.munch1 (const True))

-------------------------------------------------------------------------------
-- Action
-------------------------------------------------------------------------------

-- | Entry point for the 'status' command.
statusAction :: NixStyleFlags StatusFlags -> [String] -> GlobalFlags -> IO ()
statusAction flags@NixStyleFlags { extraFlags = statusFlags, ..} cliTargetStrings globalFlags = do
  when (NoFlag == statusOutputFormat statusFlags) $ do
    die' verbosity "The status command requires the flag '--output-format'."
  when (not $ null cliTargetStrings) $
    die' verbosity "The status command takes not target arguments directly. Use appropriate flags to pass in target information."

  baseCtx <- establishProjectBaseContext verbosity cliConfig OtherCommand
  (_, elaboratedPlan, elabSharedConfig, _, _) <-
    rebuildInstallPlan verbosity
                        (distDirLayout baseCtx)
                        (cabalDirLayout baseCtx)
                        (projectConfig baseCtx)
                        (localPackages baseCtx)

  compilerInformation <- if not $ fromFlagOrDefault False (statusCompiler statusFlags)
    then pure Nothing
    else do
      let compiler = pkgConfigCompiler elabSharedConfig
      compilerProg <- requireCompilerProg verbosity compiler
      let progDb = pkgConfigCompilerProgs elabSharedConfig
      (configuredCompilerProg, _) <- requireProgram verbosity compilerProg progDb
      pure $ Just $ mkCompilerInfo configuredCompilerProg compiler

  resolvedTargets <- if null (statusTargets statusFlags)
    then pure Nothing
    else do
      let targetStrings = statusTargets statusFlags
      mtargetSelectors <- mapM (readTargetSelector (localPackages baseCtx) Nothing) targetStrings
      let (unresolvable, targetSelectors) = partitionEithers
                    $ map (\(mts, str) -> case mts of
                        Left _ -> Left str
                        Right ts -> Right (ts, str)
                    )
                    $ zip mtargetSelectors targetStrings
      -- Interpret the targets on the command line as build targets
      -- (as opposed to say repl or haddock targets).
      -- TODO: don't throw on targets that are invalid.
      -- TODO: why might this still fail? should we try to avoid that?
      targets <- either (reportBuildTargetProblems verbosity) return
                $ resolveTargets
                    selectPackageTargets
                    selectComponentTarget
                    elaboratedPlan
                    Nothing
                    (map fst targetSelectors)

      pure $ Just $ mkBuildInfoJson elaboratedPlan targets (Map.fromList targetSelectors) unresolvable

  let si = StatusInformation
        { siCabalVersion = cabalInstallVersion
        , siCompiler = compilerInformation
        , siTargetResolving = resolvedTargets
        }

  serialisedStatusInformation <- serialise verbosity (statusOutputFormat statusFlags) si

  -- Final output
  putStrLn $ withOutputMarker verbosity serialisedStatusInformation
  where
    verbosity = fromFlagOrDefault normal (configVerbosity configFlags)
    cliConfig = commandLineFlagsToProjectConfig globalFlags flags mempty

-- ----------------------------------------------------------------------------
-- Big Datatype that can be serialised to different formats
-- ----------------------------------------------------------------------------

data StatusInformation = StatusInformation
  { siCabalVersion :: Version
  , siCompiler :: Maybe CompilerInformation
  , siTargetResolving :: Maybe [ResolvedTarget]
  }
  deriving (Show, Read, Eq, Ord)

data CompilerInformation = CompilerInformation
  { ciFlavour :: CompilerFlavor
  , ciCompilerId :: CompilerId
  , ciPath :: FilePath
  }
  deriving (Show, Read, Eq, Ord)

data ResolvedTarget = ResolvedTarget
  { rtOriginalTarget :: String
  -- | UnitId of the resolved target.
  -- If 'Nothing', then the given target can not be resolved
  -- to a target in this project.
  , rtUnitId :: Maybe UnitId
  }
  deriving (Show, Read, Eq, Ord)

serialise :: Verbosity -> Flag StatusOutputFormat -> StatusInformation -> IO String
serialise verbosity NoFlag      _  =
  die' verbosity $ "Could not serialise Status information. "
                ++ "The flag '--output-format' is required."

serialise _         (Flag JSON) si = pure $ Json.encodeToString $ Json.object $
  [ "cabal-version" .= jdisplay (siCabalVersion si)
  ]
  ++ prettyCompilerInfo (siCompiler si)
  ++ prettyTargetResolving (siTargetResolving si)
  where
    prettyCompilerInfo Nothing = []
    prettyCompilerInfo (Just ci) =
      [ "compiler" .= Json.object
        [ "flavour"     .= jdisplay (ciFlavour ci)
        , "compiler-id" .= jdisplay (ciCompilerId ci)
        , "path"        .= Json.String (ciPath ci)
        ]
      ]

    prettyTargetResolving Nothing = []
    prettyTargetResolving (Just rts) =
      [ "targets" .= Json.Array (fmap prettyResolvedTarget rts)
      ]
      where
        prettyResolvedTarget rt = Json.object
          [ "target" .= Json.String (rtOriginalTarget rt)
          , "unit-id" .= maybe Json.Null jdisplay (rtUnitId rt)
          ]


-- ----------------------------------------------------------------------------
-- Helpers for determining and serialising compiler information
-- ----------------------------------------------------------------------------

requireCompilerProg :: Verbosity -> Compiler -> IO Program
requireCompilerProg verbosity compiler =
  case compilerFlavor compiler of
    GHC -> pure ghcProgram
    GHCJS -> pure ghcjsProgram
    flavour -> die' verbosity $
      "status: Unsupported compiler flavour: "
      <> prettyShow flavour

mkCompilerInfo :: ConfiguredProgram -> Compiler -> CompilerInformation
mkCompilerInfo compilerProgram compiler =
  CompilerInformation (compilerFlavor compiler) (compilerId compiler) (programPath compilerProgram)

-- ----------------------------------------------------------------------------
-- Helpers for determining and serialising the unit-id
-- ----------------------------------------------------------------------------

mkBuildInfoJson :: ElaboratedInstallPlan -> TargetsMap -> Map TargetSelector String -> [String] -> [ResolvedTarget]
mkBuildInfoJson elaboratedPlan targetsMap tsMap unresolvableTargetStrings =
  [ ResolvedTarget str (Just uid)
  | uid <- Map.keys subsetInstallPlan
  , (_, tss) <- targetsMap Map.! uid
  , str <- ordNub $ map tsToOriginalTarget $ toList tss
  ]
  ++ map mkUnresolvedTarget unresolvableTargetStrings
  where
    subsetInstallPlan = Map.restrictKeys (InstallPlan.toMap elaboratedPlan) (Map.keysSet targetsMap)

    tsToOriginalTarget ts = tsMap Map.! ts

    mkUnresolvedTarget :: String -> ResolvedTarget
    mkUnresolvedTarget s = ResolvedTarget s Nothing

-- ----------------------------------------------------------------------------
-- Target selectors and helpers
-- ----------------------------------------------------------------------------

-- | This defines what a 'TargetSelector' means for the @status@ command.
-- It selects the 'AvailableTarget's that the 'TargetSelector' refers to,
-- or otherwise classifies the problem.
--
-- For the @status@ command select all components except non-buildable
-- and disabled tests\/benchmarks, fail if there are no such
-- components
--
selectPackageTargets :: TargetSelector
                     -> [AvailableTarget k] -> Either TargetProblem' [k]
selectPackageTargets targetSelector targets

    -- If there are any buildable targets then we select those
  | not (null targetsBuildable)
  = Right targetsBuildable

    -- If there are targets but none are buildable then we report those
  | not (null targets)
  = Left (TargetProblemNoneEnabled targetSelector targets')

    -- If there are no targets at all then we report that
  | otherwise
  = Left (TargetProblemNoTargets targetSelector)
  where
    targets'         = forgetTargetsDetail targets
    targetsBuildable = selectBuildableTargetsWith
                         (buildable targetSelector)
                         targets

    -- When there's a target filter like "pkg:tests" then we do select tests,
    -- but if it's just a target like "pkg" then we don't build tests unless
    -- they are requested by default (i.e. by using --enable-tests)
    buildable (TargetPackage _ _  Nothing) TargetNotRequestedByDefault = False
    buildable (TargetAllPackages  Nothing) TargetNotRequestedByDefault = False
    buildable _ _ = True

-- | For a 'TargetComponent' 'TargetSelector', check if the component can be
-- selected.
--
-- For the @build@ command we just need the basic checks on being buildable etc.
--
selectComponentTarget :: SubComponentTarget
                      -> AvailableTarget k -> Either TargetProblem' k
selectComponentTarget = selectComponentTargetBasic

reportBuildTargetProblems :: Verbosity -> [TargetProblem'] -> IO a
reportBuildTargetProblems verbosity problems =
  reportTargetProblems verbosity "status" problems

readTargetSelector :: [PackageSpecifier (SourcePackage (PackageLocation a))]
                    -> Maybe ComponentKindFilter
                    -> String
                    -> IO (Either TargetSelectorProblem TargetSelector)
readTargetSelector pkgs mfilter targetStr =
  readTargetSelectors pkgs mfilter [targetStr] >>= \case
    Left [problem] -> pure $ Left problem
    Right [ts] -> pure $ Right ts
    _ -> error $ "CmdStatus.readTargetSelector: invariant broken, more than "
              ++ "one target passed *somehow*."

-- ----------------------------------------------------------------------------
-- JSON serialisation helpers
-- ----------------------------------------------------------------------------

jdisplay :: Pretty a => a -> Json.Value
jdisplay = Json.String . prettyShow
