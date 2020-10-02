{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | cabal-install CLI command: run
--
module Distribution.Client.CmdRun (
    -- * The @run@ CLI and action
    runCommand,
    runAction,
    handleShebang, validScript,

    -- * Internals exposed for testing
    matchesMultipleProblem,
    noExesProblem,
    selectPackageTargets,
    selectComponentTarget
  ) where

import Prelude ()
import Distribution.Client.Compat.Prelude hiding (toList)

import Distribution.Client.ProjectOrchestration
import Distribution.Client.CmdErrorMessages
         ( renderTargetSelector, showTargetSelector,
           renderTargetProblem,
           renderTargetProblemNoTargets, plural, targetSelectorPluralPkgs,
           targetSelectorFilter, renderListCommaAnd )
import Distribution.Client.TargetProblem
         ( TargetProblem (..) )

import Distribution.Client.NixStyleOptions
         ( NixStyleFlags (..), nixStyleOptions, defaultNixStyleFlags )
import Distribution.Client.Setup
         ( GlobalFlags(..), ConfigFlags(..) )
import Distribution.Client.GlobalFlags
         ( defaultGlobalFlags )
import Distribution.Simple.Flag
         ( fromFlagOrDefault )
import Distribution.Simple.Command
         ( CommandUI(..), usageAlternatives )
import Distribution.Types.ComponentName
         ( showComponentName )
import Distribution.CabalSpecVersion (CabalSpecVersion (..), cabalSpecLatest)
import Distribution.Verbosity
         ( normal )
import Distribution.Simple.Utils
         ( wrapText, warn, die', ordNub, info
         , createTempDirectory, handleDoesNotExist )
import Distribution.Client.ProjectConfig
         ( ProjectConfig(..), ProjectConfigShared(..)
         , withProjectOrGlobalConfig )
import Distribution.Client.ProjectFlags
         ( flagIgnoreProject )
import Distribution.Client.ProjectPlanning
         ( ElaboratedConfiguredPackage(..)
         , ElaboratedInstallPlan, binDirectoryFor )
import Distribution.Client.ProjectPlanning.Types
         ( dataDirsEnvironmentForPlan )
import Distribution.Client.TargetSelector
         ( TargetSelectorProblem(..), TargetString(..) )
import Distribution.Client.InstallPlan
         ( toList, foldPlanPackage )
import Distribution.Types.UnqualComponentName
         ( UnqualComponentName, unUnqualComponentName )
import Distribution.Simple.Program.Run
         ( runProgramInvocation, ProgramInvocation(..),
           emptyProgramInvocation )
import Distribution.Types.UnitId
         ( UnitId )

import Distribution.Client.Types
         ( PackageLocation(..), PackageSpecifier(..) )
import Distribution.FieldGrammar
         ( takeFields, parseFieldGrammar )
import Distribution.PackageDescription.FieldGrammar
         ( executableFieldGrammar )
import Distribution.PackageDescription.PrettyPrint
         ( writeGenericPackageDescription )
import Distribution.Parsec
         ( Position(..) )
import Distribution.Fields
         ( ParseResult, parseString, parseFatalFailure, readFields )
import qualified Distribution.SPDX.License as SPDX
import Distribution.Solver.Types.SourcePackage as SP
         ( SourcePackage(..) )
import Distribution.Types.BuildInfo
         ( BuildInfo(..) )
import Distribution.Types.CondTree
         ( CondTree(..) )
import Distribution.Types.Executable
         ( Executable(..) )
import Distribution.Types.GenericPackageDescription as GPD
         ( GenericPackageDescription(..), emptyGenericPackageDescription )
import Distribution.Types.PackageDescription
         ( PackageDescription(..), emptyPackageDescription )
import Distribution.Types.PackageName.Magic
         ( fakePackageId )
import Language.Haskell.Extension
         ( Language(..) )

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Text.Parsec as P
import System.Directory
         ( getTemporaryDirectory, removeDirectoryRecursive, doesFileExist )
import System.FilePath
         ( (</>), isValid, isPathSeparator, takeExtension )


runCommand :: CommandUI (NixStyleFlags ())
runCommand = CommandUI
  { commandName         = "v2-run"
  , commandSynopsis     = "Run an executable."
  , commandUsage        = usageAlternatives "v2-run"
                          [ "[TARGET] [FLAGS] [-- EXECUTABLE_FLAGS]" ]
  , commandDescription  = Just $ \pname -> wrapText $
         "Runs the specified executable-like component (an executable, a test, "
      ++ "or a benchmark), first ensuring it is up to date.\n\n"

      ++ "Any executable-like component in any package in the project can be "
      ++ "specified. A package can be specified if contains just one "
      ++ "executable-like. The default is to use the package in the current "
      ++ "directory if it contains just one executable-like.\n\n"

      ++ "Extra arguments can be passed to the program, but use '--' to "
      ++ "separate arguments for the program from arguments for " ++ pname
      ++ ". The executable is run in an environment where it can find its "
      ++ "data files inplace in the build tree.\n\n"

      ++ "Dependencies are built or rebuilt as necessary. Additional "
      ++ "configuration flags can be specified on the command line and these "
      ++ "extend the project configuration from the 'cabal.project', "
      ++ "'cabal.project.local' and other files."
  , commandNotes        = Just $ \pname ->
         "Examples:\n"
      ++ "  " ++ pname ++ " v2-run\n"
      ++ "    Run the executable-like in the package in the current directory\n"
      ++ "  " ++ pname ++ " v2-run foo-tool\n"
      ++ "    Run the named executable-like (in any package in the project)\n"
      ++ "  " ++ pname ++ " v2-run pkgfoo:foo-tool\n"
      ++ "    Run the executable-like 'foo-tool' in the package 'pkgfoo'\n"
      ++ "  " ++ pname ++ " v2-run foo -O2 -- dothing --fooflag\n"
      ++ "    Build with '-O2' and run the program, passing it extra arguments.\n"

  , commandDefaultFlags = defaultNixStyleFlags ()
  , commandOptions      = nixStyleOptions (const [])
  }

-- | The @run@ command runs a specified executable-like component, building it
-- first if necessary. The component can be either an executable, a test,
-- or a benchmark. This is particularly useful for passing arguments to
-- exes/tests/benchs by simply appending them after a @--@.
--
-- For more details on how this works, see the module
-- "Distribution.Client.ProjectOrchestration"
--
runAction :: NixStyleFlags () -> [String] -> GlobalFlags -> IO ()
runAction flags@NixStyleFlags {..} targetStrings globalFlags = do
    globalTmp <- getTemporaryDirectory
    tmpDir <- createTempDirectory globalTmp "cabal-repl."

    let
      with =
        establishProjectBaseContext verbosity cliConfig OtherCommand
      without config = do
        distDirLayout <- establishDummyDistDirLayout verbosity (config <> cliConfig) tmpDir
        establishDummyProjectBaseContext verbosity (config <> cliConfig) distDirLayout [] OtherCommand

    baseCtx <- withProjectOrGlobalConfig verbosity ignoreProject globalConfigFlag with without

    let
      scriptOrError script err = do
        exists <- doesFileExist script
        let pol | takeExtension script == ".lhs" = LiterateHaskell
                | otherwise                      = PlainHaskell
        if exists
          then BS.readFile script >>= handleScriptCase verbosity pol baseCtx tmpDir
          else reportTargetSelectorProblems verbosity err

    (baseCtx', targetSelectors) <-
      readTargetSelectors (localPackages baseCtx) (Just ExeKind) (take 1 targetStrings)
        >>= \case
          Left err@(TargetSelectorNoTargetsInProject:_)
            | (script:_) <- targetStrings -> scriptOrError script err
          Left err@(TargetSelectorNoSuch t _:_)
            | TargetString1 script <- t   -> scriptOrError script err
          Left err@(TargetSelectorExpected t _ _:_)
            | TargetString1 script <- t   -> scriptOrError script err
          Left err   -> reportTargetSelectorProblems verbosity err
          Right sels -> return (baseCtx, sels)

    buildCtx <-
      runProjectPreBuildPhase verbosity baseCtx' $ \elaboratedPlan -> do

            when (buildSettingOnlyDeps (buildSettings baseCtx')) $
              die' verbosity $
                  "The run command does not support '--only-dependencies'. "
               ++ "You may wish to use 'build --only-dependencies' and then "
               ++ "use 'run'."

            -- Interpret the targets on the command line as build targets
            -- (as opposed to say repl or haddock targets).
            targets <- either (reportTargetProblems verbosity) return
                     $ resolveTargets
                         selectPackageTargets
                         selectComponentTarget
                         elaboratedPlan
                         Nothing
                         targetSelectors

            -- Reject multiple targets, or at least targets in different
            -- components. It is ok to have two module/file targets in the
            -- same component, but not two that live in different components.
            --
            -- Note that we discard the target and return the whole 'TargetsMap',
            -- so this check will be repeated (and must succeed) after
            -- the 'runProjectPreBuildPhase'. Keep it in mind when modifying this.
            _ <- singleExeOrElse
                   (reportTargetProblems
                      verbosity
                      [multipleTargetsProblem targets])
                   targets

            let elaboratedPlan' = pruneInstallPlanToTargets
                                    TargetActionBuild
                                    targets
                                    elaboratedPlan
            return (elaboratedPlan', targets)

    (selectedUnitId, selectedComponent) <-
      -- Slight duplication with 'runProjectPreBuildPhase'.
      singleExeOrElse
        (die' verbosity $ "No or multiple targets given, but the run "
                       ++ "phase has been reached. This is a bug.")
        $ targetsMap buildCtx

    printPlan verbosity baseCtx' buildCtx

    buildOutcomes <- runProjectBuildPhase verbosity baseCtx' buildCtx
    runProjectPostBuildPhase verbosity baseCtx' buildCtx buildOutcomes


    let elaboratedPlan = elaboratedPlanToExecute buildCtx
        matchingElaboratedConfiguredPackages =
          matchingPackagesByUnitId
            selectedUnitId
            elaboratedPlan

    let exeName = unUnqualComponentName selectedComponent

    -- In the common case, we expect @matchingElaboratedConfiguredPackages@
    -- to consist of a single element that provides a single way of building
    -- an appropriately-named executable. In that case we take that
    -- package and continue.
    --
    -- However, multiple packages/components could provide that
    -- executable, or it's possible we don't find the executable anywhere
    -- in the build plan. I suppose in principle it's also possible that
    -- a single package provides an executable in two different ways,
    -- though that's probably a bug if. Anyway it's a good lint to report
    -- an error in all of these cases, even if some seem like they
    -- shouldn't happen.
    pkg <- case matchingElaboratedConfiguredPackages of
      [] -> die' verbosity $ "Unknown executable "
                          ++ exeName
                          ++ " in package "
                          ++ prettyShow selectedUnitId
      [elabPkg] -> do
        info verbosity $ "Selecting "
                       ++ prettyShow selectedUnitId
                       ++ " to supply " ++ exeName
        return elabPkg
      elabPkgs -> die' verbosity
        $ "Multiple matching executables found matching "
        ++ exeName
        ++ ":\n"
        ++ unlines (fmap (\p -> " - in package " ++ prettyShow (elabUnitId p)) elabPkgs)
    let exePath = binDirectoryFor (distDirLayout baseCtx)
                                  (elaboratedShared buildCtx)
                                  pkg
                                  exeName
               </> exeName
    let args = drop 1 targetStrings
    runProgramInvocation
      verbosity
      emptyProgramInvocation {
        progInvokePath  = exePath,
        progInvokeArgs  = args,
        progInvokeEnv   = dataDirsEnvironmentForPlan
                            (distDirLayout baseCtx)
                            elaboratedPlan
      }

    handleDoesNotExist () (removeDirectoryRecursive tmpDir)
  where
    verbosity = fromFlagOrDefault normal (configVerbosity configFlags)
    ignoreProject = flagIgnoreProject projectFlags
    cliConfig = commandLineFlagsToProjectConfig globalFlags flags mempty -- ClientInstallFlags, not needed here
    globalConfigFlag = projectConfigConfigFile (projectConfigShared cliConfig)

-- | Used by the main CLI parser as heuristic to decide whether @cabal@ was
-- invoked as a script interpreter, i.e. via
--
-- > #! /usr/bin/env cabal
--
-- or
--
-- > #! /usr/bin/cabal
--
-- As the first argument passed to `cabal` will be a filepath to the
-- script to be interpreted.
--
-- See also 'handleShebang'
validScript :: String -> IO Bool
validScript script
  | isValid script && any isPathSeparator script = doesFileExist script
  | otherwise = return False

-- | Handle @cabal@ invoked as script interpreter, see also 'validScript'
--
-- First argument is the 'FilePath' to the script to be executed; second
-- argument is a list of arguments to be passed to the script.
handleShebang :: FilePath -> [String] -> IO ()
handleShebang script args =
  runAction (commandDefaultFlags runCommand) (script:args) defaultGlobalFlags

parseScriptBlock :: BS.ByteString -> ParseResult Executable
parseScriptBlock str =
    case readFields str of
        Right fs -> do
            let (fields, _) = takeFields fs
            parseFieldGrammar cabalSpecLatest fields (executableFieldGrammar "script")
        Left perr -> parseFatalFailure pos (show perr) where
            ppos = P.errorPos perr
            pos  = Position (P.sourceLine ppos) (P.sourceColumn ppos)

readScriptBlock :: Verbosity -> BS.ByteString -> IO Executable
readScriptBlock verbosity = parseString parseScriptBlock verbosity "script block"

readScriptBlockFromScript :: Verbosity -> PlainOrLiterate -> BS.ByteString -> IO (Executable, BS.ByteString)
readScriptBlockFromScript verbosity pol str = do
    str' <- case extractScriptBlock pol str of
              Left e -> die' verbosity $ "Failed extracting script block: " ++ e
              Right x -> return x
    when (BS.all isSpace str') $ warn verbosity "Empty script block"
    (\x -> (x, noShebang)) <$> readScriptBlock verbosity str'
  where
    noShebang = BS.unlines . filter (not . BS.isPrefixOf "#!") . BS.lines $ str

-- | Extract the first encountered script metadata block started end
-- terminated by the tokens
--
-- * @{- cabal:@
--
-- * @-}@
--
-- appearing alone on lines (while tolerating trailing whitespace).
-- These tokens are not part of the 'Right' result.
--
-- In case of missing or unterminated blocks a 'Left'-error is
-- returned.
extractScriptBlock :: PlainOrLiterate -> BS.ByteString -> Either String BS.ByteString
extractScriptBlock _pol str = goPre (BS.lines str)
  where
    isStartMarker = (== startMarker) . stripTrailSpace
    isEndMarker   = (== endMarker) . stripTrailSpace

    stripTrailSpace = fst . BS.spanEnd isSpace

    -- before start marker
    goPre ls = case dropWhile (not . isStartMarker) ls of
                 [] -> Left $ "`" ++ BS.unpack startMarker ++ "` start marker not found"
                 (_:ls') -> goBody [] ls'

    goBody _ [] = Left $ "`" ++ BS.unpack endMarker ++ "` end marker not found"
    goBody acc (l:ls)
      | isEndMarker l = Right $! BS.unlines $ reverse acc
      | otherwise     = goBody (l:acc) ls

    startMarker, endMarker :: BS.ByteString
    startMarker = fromString "{- cabal:"
    endMarker   = fromString "-}"

data PlainOrLiterate
    = PlainHaskell
    | LiterateHaskell

handleScriptCase
  :: Verbosity
  -> PlainOrLiterate
  -> ProjectBaseContext
  -> FilePath
  -> BS.ByteString
  -> IO (ProjectBaseContext, [TargetSelector])
handleScriptCase verbosity pol baseCtx tmpDir scriptContents = do
  (executable, contents') <- readScriptBlockFromScript verbosity pol scriptContents

  -- We need to create a dummy package that lives in our dummy project.
  let
    mainName = case pol of
      PlainHaskell    -> "Main.hs"
      LiterateHaskell -> "Main.lhs"

    sourcePackage = SourcePackage
      { srcpkgPackageId      = pkgId
      , srcpkgDescription    = genericPackageDescription
      , srcpkgSource         = LocalUnpackedPackage tmpDir
      , srcpkgDescrOverride  = Nothing
      }
    genericPackageDescription  = emptyGenericPackageDescription
      { GPD.packageDescription = packageDescription
      , condExecutables        = [("script", CondNode executable' targetBuildDepends [])]
      }
    executable' = executable
      { modulePath = mainName
      , buildInfo = binfo
        { defaultLanguage =
          case defaultLanguage of
            just@(Just _) -> just
            Nothing       -> Just Haskell2010
        }
      }
    binfo@BuildInfo{..} = buildInfo executable
    packageDescription = emptyPackageDescription
      { package = pkgId
      , specVersion = CabalSpecV2_2
      , licenseRaw = Left SPDX.NONE
      }
    pkgId = fakePackageId

  writeGenericPackageDescription (tmpDir </> "fake-package.cabal") genericPackageDescription
  BS.writeFile (tmpDir </> mainName) contents'

  let
    baseCtx' = baseCtx
      { localPackages = localPackages baseCtx ++ [SpecificSourcePackage sourcePackage] }
    targetSelectors = [TargetPackage TargetExplicitNamed [pkgId] Nothing]

  return (baseCtx', targetSelectors)

singleExeOrElse :: IO (UnitId, UnqualComponentName) -> TargetsMap -> IO (UnitId, UnqualComponentName)
singleExeOrElse action targetsMap =
  case Set.toList . distinctTargetComponents $ targetsMap
  of [(unitId, CExeName component)] -> return (unitId, component)
     [(unitId, CTestName component)] -> return (unitId, component)
     [(unitId, CBenchName component)] -> return (unitId, component)
     _   -> action

-- | Filter the 'ElaboratedInstallPlan' keeping only the
-- 'ElaboratedConfiguredPackage's that match the specified
-- 'UnitId'.
matchingPackagesByUnitId :: UnitId
                         -> ElaboratedInstallPlan
                         -> [ElaboratedConfiguredPackage]
matchingPackagesByUnitId uid =
          catMaybes
          . fmap (foldPlanPackage
                    (const Nothing)
                    (\x -> if elabUnitId x == uid
                           then Just x
                           else Nothing))
          . toList

-- | This defines what a 'TargetSelector' means for the @run@ command.
-- It selects the 'AvailableTarget's that the 'TargetSelector' refers to,
-- or otherwise classifies the problem.
--
-- For the @run@ command we select the exe if there is only one and it's
-- buildable. Fail if there are no or multiple buildable exe components.
--
selectPackageTargets :: TargetSelector
                     -> [AvailableTarget k] -> Either RunTargetProblem [k]
selectPackageTargets targetSelector targets

    -- If there is exactly one buildable executable then we select that
  | [target] <- targetsExesBuildable
  = Right [target]

    -- but fail if there are multiple buildable executables.
  | not (null targetsExesBuildable)
  = Left (matchesMultipleProblem targetSelector targetsExesBuildable')

    -- If there are executables but none are buildable then we report those
  | not (null targetsExes)
  = Left (TargetProblemNoneEnabled targetSelector targetsExes)

    -- If there are no executables but some other targets then we report that
  | not (null targets)
  = Left (noExesProblem targetSelector)

    -- If there are no targets at all then we report that
  | otherwise
  = Left (TargetProblemNoTargets targetSelector)
  where
    -- Targets that can be executed
    targetsExecutableLike =
      concatMap (\kind -> filterTargetsKind kind targets)
                [ExeKind, TestKind, BenchKind]
    (targetsExesBuildable,
     targetsExesBuildable') = selectBuildableTargets' targetsExecutableLike

    targetsExes             = forgetTargetsDetail targetsExecutableLike


-- | For a 'TargetComponent' 'TargetSelector', check if the component can be
-- selected.
--
-- For the @run@ command we just need to check it is a executable-like
-- (an executable, a test, or a benchmark), in addition
-- to the basic checks on being buildable etc.
--
selectComponentTarget :: SubComponentTarget
                      -> AvailableTarget k -> Either RunTargetProblem  k
selectComponentTarget subtarget@WholeComponent t
  = case availableTargetComponentName t
    of CExeName _ -> component
       CTestName _ -> component
       CBenchName _ -> component
       _ -> Left (componentNotExeProblem pkgid cname)
    where pkgid = availableTargetPackageId t
          cname = availableTargetComponentName t
          component = selectComponentTargetBasic subtarget t

selectComponentTarget subtarget t
  = Left (isSubComponentProblem (availableTargetPackageId t)
           (availableTargetComponentName t)
           subtarget)

-- | The various error conditions that can occur when matching a
-- 'TargetSelector' against 'AvailableTarget's for the @run@ command.
--
data RunProblem =
     -- | The 'TargetSelector' matches targets but no executables
     TargetProblemNoExes      TargetSelector

     -- | A single 'TargetSelector' matches multiple targets
   | TargetProblemMatchesMultiple TargetSelector [AvailableTarget ()]

     -- | Multiple 'TargetSelector's match multiple targets
   | TargetProblemMultipleTargets TargetsMap

     -- | The 'TargetSelector' refers to a component that is not an executable
   | TargetProblemComponentNotExe PackageId ComponentName

     -- | Asking to run an individual file or module is not supported
   | TargetProblemIsSubComponent  PackageId ComponentName SubComponentTarget
  deriving (Eq, Show)

type RunTargetProblem = TargetProblem RunProblem

noExesProblem :: TargetSelector -> RunTargetProblem
noExesProblem = CustomTargetProblem . TargetProblemNoExes

matchesMultipleProblem :: TargetSelector -> [AvailableTarget ()] -> RunTargetProblem
matchesMultipleProblem selector targets = CustomTargetProblem $
    TargetProblemMatchesMultiple selector targets

multipleTargetsProblem :: TargetsMap -> TargetProblem RunProblem
multipleTargetsProblem = CustomTargetProblem . TargetProblemMultipleTargets

componentNotExeProblem :: PackageId -> ComponentName -> TargetProblem RunProblem
componentNotExeProblem pkgid name = CustomTargetProblem $
    TargetProblemComponentNotExe pkgid name

isSubComponentProblem
  :: PackageId
  -> ComponentName
  -> SubComponentTarget
  -> TargetProblem RunProblem
isSubComponentProblem pkgid name subcomponent = CustomTargetProblem $
    TargetProblemIsSubComponent pkgid name subcomponent

reportTargetProblems :: Verbosity -> [RunTargetProblem] -> IO a
reportTargetProblems verbosity =
    die' verbosity . unlines . map renderRunTargetProblem

renderRunTargetProblem :: RunTargetProblem -> String
renderRunTargetProblem (TargetProblemNoTargets targetSelector) =
    case targetSelectorFilter targetSelector of
      Just kind | kind /= ExeKind
        -> "The run command is for running executables, but the target '"
           ++ showTargetSelector targetSelector ++ "' refers to "
           ++ renderTargetSelector targetSelector ++ "."

      _ -> renderTargetProblemNoTargets "run" targetSelector
renderRunTargetProblem problem =
    renderTargetProblem "run" renderRunProblem problem

renderRunProblem :: RunProblem -> String
renderRunProblem (TargetProblemMatchesMultiple targetSelector targets) =
    "The run command is for running a single executable at once. The target '"
 ++ showTargetSelector targetSelector ++ "' refers to "
 ++ renderTargetSelector targetSelector ++ " which includes "
 ++ renderListCommaAnd ( ("the "++) <$>
                         showComponentName <$>
                         availableTargetComponentName <$>
                         foldMap
                           (\kind -> filterTargetsKind kind targets)
                           [ExeKind, TestKind, BenchKind] )
 ++ "."

renderRunProblem (TargetProblemMultipleTargets selectorMap) =
    "The run command is for running a single executable at once. The targets "
 ++ renderListCommaAnd [ "'" ++ showTargetSelector ts ++ "'"
                       | ts <- ordNub (concatMap snd (concat (Map.elems selectorMap))) ]
 ++ " refer to different executables."

renderRunProblem (TargetProblemComponentNotExe pkgid cname) =
    "The run command is for running executables, but the target '"
 ++ showTargetSelector targetSelector ++ "' refers to "
 ++ renderTargetSelector targetSelector ++ " from the package "
 ++ prettyShow pkgid ++ "."
  where
    targetSelector = TargetComponent pkgid cname WholeComponent

renderRunProblem (TargetProblemIsSubComponent pkgid cname subtarget) =
    "The run command can only run an executable as a whole, "
 ++ "not files or modules within them, but the target '"
 ++ showTargetSelector targetSelector ++ "' refers to "
 ++ renderTargetSelector targetSelector ++ "."
  where
    targetSelector = TargetComponent pkgid cname subtarget

renderRunProblem (TargetProblemNoExes targetSelector) =
    "Cannot run the target '" ++ showTargetSelector targetSelector
 ++ "' which refers to " ++ renderTargetSelector targetSelector
 ++ " because "
 ++ plural (targetSelectorPluralPkgs targetSelector) "it does" "they do"
 ++ " not contain any executables."
