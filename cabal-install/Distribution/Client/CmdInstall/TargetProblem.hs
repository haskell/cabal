module Distribution.Client.CmdInstall.TargetProblem
  ( TargetProblem (..)
  , reportTargetProblems
  ) where

import Distribution.Client.CmdErrorMessages
       ( renderTargetProblemCommon
       , renderTargetProblemNoneEnabled
       , renderTargetProblemNoTargets )
import Distribution.Client.ProjectOrchestration
       ( TargetProblemCommon(..) )
import Distribution.Client.ProjectPlanning
       ( AvailableTarget(..) )
import Distribution.Client.TargetSelector
       ( TargetSelector(..) )
import Distribution.Verbosity
       ( Verbosity )
import Distribution.Simple.Utils
       ( die' )

-- | The various error conditions that can occur when matching a
-- 'TargetSelector' against 'AvailableTarget's for the @build@ command.
--
data TargetProblem =
     TargetProblemCommon       TargetProblemCommon

     -- | The 'TargetSelector' matches targets but none are buildable
   | TargetProblemNoneEnabled TargetSelector [AvailableTarget ()]

     -- | There are no targets at all
   | TargetProblemNoTargets   TargetSelector
  deriving (Eq, Show)

reportTargetProblems :: Verbosity -> [TargetProblem] -> IO a
reportTargetProblems verbosity =
    die' verbosity . unlines . map renderTargetProblem

renderTargetProblem :: TargetProblem -> String
renderTargetProblem (TargetProblemCommon problem) =
    renderTargetProblemCommon "build" problem
renderTargetProblem (TargetProblemNoneEnabled targetSelector targets) =
    renderTargetProblemNoneEnabled "build" targetSelector targets
renderTargetProblem(TargetProblemNoTargets targetSelector) =
    renderTargetProblemNoTargets "build" targetSelector
