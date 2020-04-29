module Distribution.Client.CmdInstall.Utils
  ( reportCannotPruneDependencies
  , selectComponentTarget
  , warnIfNoExes
  ) where

import Prelude ()
import Distribution.Client.Compat.Prelude

import qualified Data.Map as Map

import Distribution.Client.CmdInstall.TargetProblem
       ( TargetProblem(..) )

import Distribution.Client.CmdErrorMessages
       ( plural, listPlural, renderCannotPruneDependencies, showTargetSelector )
import Distribution.Client.ProjectOrchestration
       ( CannotPruneDependencies(..), ComponentName(..), SubComponentTarget(..)
       , ComponentTarget(..) , ProjectBuildContext(..), selectComponentTargetBasic )
import Distribution.Client.ProjectPlanning
       ( AvailableTarget(..) )
import Distribution.Simple.Utils
       ( die', warn )
import Distribution.Verbosity
       ( Verbosity )

-- | For a 'TargetComponent' 'TargetSelector', check if the component can be
-- selected.
--
-- For the @build@ command we just need the basic checks on being buildable etc.
--
selectComponentTarget
  :: SubComponentTarget
  -> AvailableTarget k -> Either TargetProblem k
selectComponentTarget subtarget =
    either (Left . TargetProblemCommon) Right
  . selectComponentTargetBasic subtarget

-- | Emits a warning if the build context does not contain any executables.
warnIfNoExes :: Verbosity -> ProjectBuildContext -> IO ()
warnIfNoExes verbosity buildCtx =
  when noExes $
    warn verbosity $
    "You asked to install executables, but there are no executables in "
    <> plural (listPlural selectors) "target" "targets" <> ": "
    <> intercalate ", " (showTargetSelector <$> selectors) <> ". "
    <> "Perhaps you want to use --lib to install libraries instead."
  where
    targets    = concat $ Map.elems $ targetsMap buildCtx
    components = fst <$> targets
    selectors  = concatMap snd targets
    noExes     = null $ catMaybes $ exeMaybe <$> components

    exeMaybe (ComponentTarget (CExeName exe) _) = Just exe
    exeMaybe _                                  = Nothing

reportCannotPruneDependencies :: Verbosity -> CannotPruneDependencies -> IO a
reportCannotPruneDependencies verbosity =
    die' verbosity . renderCannotPruneDependencies
