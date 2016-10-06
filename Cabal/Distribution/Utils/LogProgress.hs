module Distribution.Utils.LogProgress (
    LogProgress,
    LogMsg(..),
    runLogProgress,
    warnProgress,
    infoProgress,
) where

import Distribution.Utils.Progress
import Distribution.Verbosity
import Distribution.Simple.Utils
import Text.PrettyPrint (Doc, (<+>), text, render)
import Control.Monad (when)

-- | The 'Progress' monad with specialized logging and
-- error messages.
type LogProgress a = Progress LogMsg Doc a

-- | A tracing message which will be output at some verbosity.
data LogMsg = LogMsg Verbosity Doc

-- | Run 'LogProgress', outputting traces according to 'Verbosity',
-- 'die' if there is an error.
runLogProgress :: Verbosity -> LogProgress a -> IO a
runLogProgress verbosity = foldProgress step_fn fail_fn return
  where
    step_fn :: LogMsg -> IO a -> IO a
    step_fn (LogMsg v doc) go = do
        when (verbosity >= v) $
            putStrLn (render doc)
        go
    fail_fn :: Doc -> IO a
    fail_fn doc = die (render doc)

-- | Output a warning trace message in 'LogProgress'.
warnProgress :: Doc -> LogProgress ()
warnProgress s  = stepProgress (LogMsg normal (text "Warning:" <+> s))

-- | Output an informational trace message in 'LogProgress'.
infoProgress :: Doc -> LogProgress ()
infoProgress s  = stepProgress (LogMsg verbose s)
