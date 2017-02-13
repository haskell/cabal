{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Distribution.Utils.LogProgress (
    LogProgress,
    LogMsg(..),
    runLogProgress,
    warnProgress,
    infoProgress,
    dieProgress,
) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Utils.Progress
import Distribution.Verbosity
import Distribution.Simple.Utils
import Text.PrettyPrint (Doc, (<+>), text, render)
import Control.Monad (when)

-- | The 'Progress' monad with specialized logging and
-- error messages.
newtype LogProgress a = LogProgress (Progress LogMsg Doc a)
    deriving (Functor, Applicative, Monad)

-- | A tracing message which will be output at some verbosity.
data LogMsg = LogMsg Verbosity Doc

-- | Run 'LogProgress', outputting traces according to 'Verbosity',
-- 'die' if there is an error.
runLogProgress :: Verbosity -> LogProgress a -> NoCallStackIO a
runLogProgress verbosity (LogProgress m) = foldProgress step_fn fail_fn return m
  where
    step_fn :: LogMsg -> NoCallStackIO a -> NoCallStackIO a
    step_fn (LogMsg v doc) go = do
        when (verbosity >= v) $
            putStrLn (render doc)
        go
    fail_fn :: Doc -> NoCallStackIO a
    fail_fn doc = do
        dieMsgNoWrap (render doc ++ "\n")
        die "Configuration failed"

-- | Output a warning trace message in 'LogProgress'.
warnProgress :: Doc -> LogProgress ()
warnProgress s = LogProgress $ stepProgress (LogMsg normal (text "Warning:" <+> s))

-- | Output an informational trace message in 'LogProgress'.
infoProgress :: Doc -> LogProgress ()
infoProgress s = LogProgress $ stepProgress (LogMsg verbose s)

-- | Fail the computation with an error message.
dieProgress :: Doc -> LogProgress a
dieProgress s = LogProgress $ failProgress s
