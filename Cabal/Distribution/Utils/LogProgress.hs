{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
module Distribution.Utils.LogProgress (
    LogProgress,
    runLogProgress,
    warnProgress,
    infoProgress,
    dieProgress,
    addProgressCtx,
) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Outputable
import Distribution.Utils.Progress
import Distribution.Verbosity
import Distribution.Simple.Utils

type CtxMsg = SDoc
type LogMsg = SDoc
type ErrMsg = SDoc

data LogEnv = LogEnv {
        le_verbosity :: Verbosity,
        le_context   :: [CtxMsg]
    }

-- | The 'Progress' monad with specialized logging and
-- error messages.
newtype LogProgress a = LogProgress { unLogProgress :: LogEnv -> Progress LogMsg ErrMsg a }

instance Functor LogProgress where
    fmap f (LogProgress m) = LogProgress (fmap (fmap f) m)

instance Applicative LogProgress where
    pure x = LogProgress (pure (pure x))
    LogProgress f <*> LogProgress x = LogProgress $ \r -> f r `ap` x r

instance Monad LogProgress where
    return = pure
    LogProgress m >>= f = LogProgress $ \r -> m r >>= \x -> unLogProgress (f x) r

-- | Run 'LogProgress', outputting traces according to 'Verbosity',
-- 'die' if there is an error.
runLogProgress :: Verbosity -> LogProgress a -> NoCallStackIO a
runLogProgress verbosity (LogProgress m) =
    foldProgress step_fn fail_fn return (m env)
  where
    env = LogEnv {
        le_verbosity = verbosity,
        le_context   = []
      }
    step_fn :: LogMsg -> NoCallStackIO a -> NoCallStackIO a
    step_fn doc go = do
        putStrLn (showSDoc verbosity doc)
        go
    fail_fn :: SDoc -> NoCallStackIO a
    fail_fn doc = do
        dieNoWrap verbosity (showSDoc verbosity doc)

-- | Output a warning trace message in 'LogProgress'.
warnProgress :: SDoc -> LogProgress ()
warnProgress s = LogProgress $ \env ->
    when (le_verbosity env >= normal) $
        stepProgress $
            hang (text "Warning:") 4 (formatMsg (le_context env) s)

-- | Output an informational trace message in 'LogProgress'.
infoProgress :: SDoc -> LogProgress ()
infoProgress s = LogProgress $ \env ->
    when (le_verbosity env >= verbose) $
        stepProgress s

-- | Fail the computation with an error message.
dieProgress :: SDoc -> LogProgress a
dieProgress s = LogProgress $ \env ->
    failProgress $
        hang (text "Error:") 4 (formatMsg (le_context env) s)

-- | Format a message with context. (Something simple for now.)
formatMsg :: [CtxMsg] -> SDoc -> SDoc
formatMsg ctx doc = doc $$ vcat ctx

-- | Add a message to the error/warning context.
addProgressCtx :: CtxMsg -> LogProgress a -> LogProgress a
addProgressCtx s (LogProgress m) = LogProgress $ \env ->
    m env { le_context = s : le_context env }
