module Distribution.Client.SingleCompTargetProblem (
    -- * Single component or else
    singleComponentOrElse,
    -- * SingleCompTargetProblem
    SingleCompTargetProblem,
    reportTargetProblems,
    selectPackageTargets,
    selectComponentTarget,
    -- * Problem smart constructor
    noCompsProblem,
    multipleTargetsProblem,
    matchesMultipleProblem,
) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Distribution.Client.CmdErrorMessages
       (componentKind, plural, renderListCommaAnd, renderTargetProblem,
       renderTargetProblemNoTargets, renderTargetSelector, showTargetSelector,
       targetSelectorFilter, targetSelectorPluralPkgs)
import Distribution.Client.ProjectOrchestration
import Distribution.Client.TargetProblem        (TargetProblem (..))
import Distribution.Simple.Utils                (die', ordNub)
import Distribution.Types.UnitId                 (UnitId)
import Distribution.Types.ComponentName         (showComponentName)

import qualified Data.Set                                as Set
import qualified Data.Map as Map

singleComponentOrElse
    :: [ComponentKind]
    -> IO (UnitId, ComponentName)
    -> TargetsMap
    -> IO (UnitId, ComponentName)
singleComponentOrElse kinds action tmap =
    case Set.toList $ distinctTargetComponents tmap of
        [(unitId, component)]
            | componentKind component `elem` kinds  -> return (unitId, component)
        _   -> action

selectPackageTargets
    :: [ComponentKind]
    -> TargetSelector
    -> [AvailableTarget k]
    -> Either SingleCompTargetProblem [k]
selectPackageTargets kinds targetSelector targets
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
    = Left (noCompsProblem targetSelector)

      -- If there are no targets at all then we report that
    | otherwise
    = Left (TargetProblemNoTargets targetSelector)
  where
    -- Targets that can be executed
    targetsExecutableLike =
        concatMap (\kind -> filterTargetsKind kind targets) kinds
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
selectComponentTarget
    :: [ComponentKind]
    -> SubComponentTarget
    -> AvailableTarget k
    -> Either SingleCompTargetProblem  k
selectComponentTarget kinds subtarget@WholeComponent t
    | componentKind (availableTargetComponentName t) `elem` kinds = component
    | otherwise = Left (componentNotRightKindProblem pkgid cname)
  where
    pkgid = availableTargetPackageId t
    cname = availableTargetComponentName t
    component = selectComponentTargetBasic subtarget t
selectComponentTarget _ subtarget t
  = Left (isSubComponentProblem (availableTargetPackageId t)
           (availableTargetComponentName t)
           subtarget)

-- | The various error conditions that can occur when matching a
-- 'TargetSelector' against 'AvailableTarget's for the @run@ command.
--
data SingleCompProblem =
     -- | The 'TargetSelector' matches targets but no executables
     TargetProblemNoComps      TargetSelector

     -- | A single 'TargetSelector' matches multiple targets
   | TargetProblemMatchesMultiple TargetSelector [AvailableTarget ()]

     -- | Multiple 'TargetSelector's match multiple targets
   | TargetProblemMultipleTargets TargetsMap

     -- | The 'TargetSelector' refers to a component that is not an executable
   | TargetProblemComponentNotRightKind PackageId ComponentName

     -- | Asking to run an individual file or module is not supported
   | TargetProblemIsSubComponent  PackageId ComponentName SubComponentTarget
  deriving (Eq, Show)

type SingleCompTargetProblem = TargetProblem SingleCompProblem

noCompsProblem :: TargetSelector -> SingleCompTargetProblem
noCompsProblem = CustomTargetProblem . TargetProblemNoComps

matchesMultipleProblem :: TargetSelector -> [AvailableTarget ()] -> SingleCompTargetProblem
matchesMultipleProblem selector targets = CustomTargetProblem $
    TargetProblemMatchesMultiple selector targets

multipleTargetsProblem :: TargetsMap -> TargetProblem SingleCompProblem
multipleTargetsProblem = CustomTargetProblem . TargetProblemMultipleTargets

componentNotRightKindProblem :: PackageId -> ComponentName -> TargetProblem SingleCompProblem
componentNotRightKindProblem pkgid name = CustomTargetProblem $
    TargetProblemComponentNotRightKind pkgid name

isSubComponentProblem
  :: PackageId
  -> ComponentName
  -> SubComponentTarget
  -> TargetProblem SingleCompProblem
isSubComponentProblem pkgid name subcomponent = CustomTargetProblem $
    TargetProblemIsSubComponent pkgid name subcomponent

reportTargetProblems :: Verbosity -> String -> [SingleCompTargetProblem] -> IO a
reportTargetProblems verbosity verb =
    die' verbosity . unlines . map (renderSingleCompTargetProblem verb)

renderSingleCompTargetProblem
    :: String
    -> SingleCompTargetProblem -> String
renderSingleCompTargetProblem verb (TargetProblemNoTargets targetSelector) =
    case targetSelectorFilter targetSelector of
      Just kind | kind /= ExeKind
        -> "The " ++ verb ++ " is for running executables, but the target '"
           ++ showTargetSelector targetSelector ++ "' refers to "
           ++ renderTargetSelector targetSelector ++ "."

      _ -> renderTargetProblemNoTargets verb targetSelector
renderSingleCompTargetProblem verb problem =
    renderTargetProblem verb (renderSingleCompProblem verb) problem

renderSingleCompProblem
    :: String
    ->SingleCompProblem
    -> String
renderSingleCompProblem cmdname (TargetProblemMatchesMultiple targetSelector targets) =
    "The " ++ cmdname ++ " is for finding a single executable at once. The target '"
 ++ showTargetSelector targetSelector ++ "' refers to "
 ++ renderTargetSelector targetSelector ++ " which includes "
 ++ renderListCommaAnd ( ("the "++) <$>
                         showComponentName <$>
                         availableTargetComponentName <$>
                         foldMap
                           (\kind -> filterTargetsKind kind targets)
                           [ExeKind, TestKind, BenchKind] )
 ++ "."

renderSingleCompProblem cmdname (TargetProblemMultipleTargets selectorMap) =
    "The " ++ cmdname ++ " is for finding a single executable at once. The targets "
 ++ renderListCommaAnd [ "'" ++ showTargetSelector ts ++ "'"
                       | ts <- ordNub (concatMap snd (concat (Map.elems selectorMap))) ]
 ++ " refer to different executables."

renderSingleCompProblem cmdname (TargetProblemComponentNotRightKind pkgid cname) =
    "The " ++ cmdname ++ " is for finding executables, but the target '"
 ++ showTargetSelector targetSelector ++ "' refers to "
 ++ renderTargetSelector targetSelector ++ " from the package "
 ++ prettyShow pkgid ++ "."
  where
    targetSelector = TargetComponent pkgid cname WholeComponent

renderSingleCompProblem cmdname (TargetProblemIsSubComponent pkgid cname subtarget) =
    "The " ++ cmdname ++ " can only find an executable as a whole, "
 ++ "not files or modules within them, but the target '"
 ++ showTargetSelector targetSelector ++ "' refers to "
 ++ renderTargetSelector targetSelector ++ "."
  where
    targetSelector = TargetComponent pkgid cname subtarget

renderSingleCompProblem cmdname (TargetProblemNoComps targetSelector) =
    "Cannot " ++ cmdname ++ " the target '" ++ showTargetSelector targetSelector
 ++ "' which refers to " ++ renderTargetSelector targetSelector
 ++ " because "
 ++ plural (targetSelectorPluralPkgs targetSelector) "it does" "they do"
 ++ " not contain any executables."
