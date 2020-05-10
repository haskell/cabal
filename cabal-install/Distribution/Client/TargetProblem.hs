{-# LANGUAGE LambdaCase #-}

module Distribution.Client.TargetProblem
  ( TargetProblem,
    ExtensibleTargetProblem (..),
    reportTargetProblems,
    renderTargetProblem,
    commonTargetProblem,
    noneEnabledTargetProblem,
    noTargetsProblem,
    customTargetProblem,
  )
where

import Distribution.Client.CmdErrorMessages
  ( renderTargetProblemCommon,
    renderTargetProblemNoTargets,
    renderTargetProblemNoneEnabled,
  )
import Distribution.Client.Compat.Prelude
import Distribution.Client.ProjectOrchestration
  ( TargetProblemCommon (..),
  )
import Distribution.Client.ProjectPlanning
  ( AvailableTarget,
  )
import Distribution.Client.TargetSelector
  ( TargetSelector,
  )
import Distribution.Simple.Utils
  ( die',
  )
import Distribution.Verbosity
  ( Verbosity,
  )
import Prelude ()

-- | Type alias for a 'TargetProblem' with no user-defined problems/errors.
--
-- Can use the utilities below for reporting/rendering problems.
type TargetProblem = ExtensibleTargetProblem ()

-- | The various error conditions that can occur when matching a
-- 'TargetSelector' against 'AvailableTarget's which can be extended
-- with command specific target problems as described by 'e'.
data ExtensibleTargetProblem e
  = ExtensibleTargetProblemCommon TargetProblemCommon
  | -- | The 'TargetSelector' matches benchmarks but none are buildable
    ExtensibleTargetProblemNoneEnabled TargetSelector [AvailableTarget ()]
  | -- | There are no targets at all
    ExtensibleTargetProblemNoTargets TargetSelector
  | -- | A custom target problem
    ExtensibleTargetProblemCustomProblem e
  deriving (Eq, Show)

commonTargetProblem :: TargetProblemCommon -> ExtensibleTargetProblem e
commonTargetProblem = ExtensibleTargetProblemCommon

noneEnabledTargetProblem :: TargetSelector -> [AvailableTarget ()] -> ExtensibleTargetProblem e
noneEnabledTargetProblem = ExtensibleTargetProblemNoneEnabled

noTargetsProblem :: TargetSelector -> ExtensibleTargetProblem e
noTargetsProblem = ExtensibleTargetProblemNoTargets

customTargetProblem :: e -> ExtensibleTargetProblem e
customTargetProblem = ExtensibleTargetProblemCustomProblem

-- | Default implementation of 'reportTargetProblems' simply renders one problem per line.
reportTargetProblems :: Verbosity -> String -> [TargetProblem] -> IO a
reportTargetProblems verbosity verb =
  die' verbosity . unlines . map (renderTargetProblem verb)

-- | Default implementation of 'renderTargetProblem'.
renderTargetProblem :: String -> TargetProblem -> String
renderTargetProblem verb = \case
  (ExtensibleTargetProblemCommon problem) ->
    renderTargetProblemCommon verb problem
  (ExtensibleTargetProblemNoneEnabled targetSelector targets) ->
    renderTargetProblemNoneEnabled verb targetSelector targets
  (ExtensibleTargetProblemNoTargets targetSelector) ->
    renderTargetProblemNoTargets verb targetSelector
  (ExtensibleTargetProblemCustomProblem ()) ->
    ""
