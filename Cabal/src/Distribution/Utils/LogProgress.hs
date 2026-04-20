{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}

module Distribution.Utils.LogProgress
  ( LogProgress
  , runLogProgress
  , warnProgress
  , infoProgress
  , dieProgress
  , addProgressCtx
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Simple.Utils
import Distribution.Utils.Progress
import Distribution.Verbosity
import System.IO (hFlush, hPutStr, hPutStrLn)
import Text.PrettyPrint

type CtxMsg = Doc
data LogMsg = WarnMsg Doc | InfoMsg Doc
type ErrMsg = Doc

data LogEnv = LogEnv
  { le_verbosity :: Verbosity
  , le_context :: [CtxMsg]
  }

-- | The 'Progress' monad with specialized logging and
-- error messages.
newtype LogProgress a = LogProgress {unLogProgress :: LogEnv -> Progress LogMsg ErrMsg a}

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
runLogProgress :: Verbosity -> LogProgress a -> IO a
runLogProgress verbosity (LogProgress m) =
  foldProgress step_fn fail_fn return (m env)
  where
    env =
      LogEnv
        { le_verbosity = verbosity
        , le_context = []
        }
    step_fn :: LogMsg -> IO a -> IO a
    step_fn (WarnMsg doc) go = do
      -- Log the warning to the stderr handle, but flush the stdout handle first,
      -- to prevent interleaving (see Distribution.Simple.Utils.warnMessage).
      let h = verbosityErrorHandle verbosity
          flags = verbosityFlags verbosity
      hFlush (verbosityChosenOutputHandle verbosity)
      hPutStr h $ withOutputMarker flags (render doc ++ "\n")
      go
    step_fn (InfoMsg doc) go = do
      -- Don't mark 'infoProgress' messages (mostly Backpack internals)
      hPutStrLn (verbosityChosenOutputHandle verbosity) (render doc)
      go
    fail_fn :: ErrMsg -> IO a
    fail_fn doc = do
      dieNoWrap verbosity (render doc)

-- | Output a warning trace message in 'LogProgress'.
warnProgress :: Doc -> LogProgress ()
warnProgress s = LogProgress $ \env ->
  when (verbosityLevel (le_verbosity env) >= Normal) $
    stepProgress $
      WarnMsg $
        hang (text "Warning:") 4 (formatMsg (le_context env) s)

-- | Output an informational trace message in 'LogProgress'.
infoProgress :: Doc -> LogProgress ()
infoProgress s = LogProgress $ \env ->
  when (verbosityLevel (le_verbosity env) >= Verbose) $
    stepProgress $
      InfoMsg s

-- | Fail the computation with an error message.
dieProgress :: Doc -> LogProgress a
dieProgress s = LogProgress $ \env ->
  failProgress $
    hang (text "Error:") 4 (formatMsg (le_context env) s)

-- | Format a message with context. (Something simple for now.)
formatMsg :: [CtxMsg] -> Doc -> Doc
formatMsg ctx doc = doc $$ vcat ctx

-- | Add a message to the error/warning context.
addProgressCtx :: CtxMsg -> LogProgress a -> LogProgress a
addProgressCtx s (LogProgress m) = LogProgress $ \env ->
  m env{le_context = s : le_context env}
