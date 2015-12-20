{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Distribution.Simple.Reconfigure.Generic
       ( Reconfigure, Requirement, reconfigure
       , readArgs, writeArgs
       , ReconfigureError(..)
       ) where

import Distribution.Simple.Command
    ( CommandParse(..), CommandUI(..), commandParseArgs )
import Distribution.Simple.UserHooks ( Args )
import Distribution.Simple.Utils
    ( createDirectoryIfMissingVerbose, info, notice )
import Distribution.Verbosity (Verbosity, lessVerbose)

import Control.Exception
    ( Exception, SomeException(..), catch, throwIO )
import Control.Monad (liftM)
import Data.Foldable (foldlM)
import Data.List (intercalate)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Typeable
#if __GLASGOW_HASKELL__ < 706
import Prelude hiding ( catch )
#endif
import System.Directory ( doesFileExist )
import System.FilePath ( takeDirectory )

type Requirement flags = flags -> Maybe (flags, String)
type Reconfigure flags a = [Requirement flags] -> Verbosity -> FilePath -> IO a

-- -----------------------------------------------------------------------------
-- * Reconfiguration
-- -----------------------------------------------------------------------------

-- | Read the 'localBuildInfoFile', reconfiguring if necessary. Throws
-- 'ConfigStateFileError' if the file cannot be read and the package cannot
-- be reconfigured.
reconfigure :: Bool  -- ^ reconfigure even if unnecessary
            -> CommandUI flags
            -> (flags -> Verbosity -> flags) -- ^ set verbosity
            -> (FilePath -> IO a)  -- ^ get configuration
            -> (flags -> Args -> IO a)
               -- ^ configure action
            -> (FilePath -> FilePath)
               -- ^ path to saved command-line arguments, relative to @--build-dir@
            -> Reconfigure flags a
reconfigure force cmd setVerbosity getConfig configureAction
            argsFile reqs verbosity distPref = do
    savedArgs <- liftM (fromMaybe []) (readArgs (argsFile distPref))
    (savedFlags, savedExtraArgs) <- case (commandParseArgs cmd True savedArgs) of
      CommandHelp _ -> throwIO (ReconfigureErrorHelp savedArgs)
      CommandList _ -> throwIO (ReconfigureErrorList savedArgs)
      CommandErrors errs -> throwIO (ReconfigureErrorOther savedArgs errs)
      CommandReadyToGo (mkFlags, extraArgs) ->
        return (mkFlags (commandDefaultFlags cmd), extraArgs)

    let forceReconfigure = do
          flags <- setRequirements (setVerbosity savedFlags verbosity)
          configureAction flags savedExtraArgs

    if force || checkRequirements savedFlags
      then forceReconfigure
      else catch (getConfig distPref)
           (\(SomeException exn) -> do info verbosity (show exn)
                                       forceReconfigure)
  where
    -- needs to be reconfigured?
    checkRequirements flags = (not . null . catMaybes) (map ($ flags) reqs)

    setRequirements orig = foldlM setRequirements_go orig reqs where
      setRequirements_go flags req =
        case req flags of
          Nothing -> return flags
          Just (flags', reason) -> do
            notice verbosity ("reconfigure: " ++ reason)
            return flags'

-- | Write command-line arguments to a file, separated by null characters. This
-- format is also suitable for the @xargs -0@ command. Using the null
-- character also avoids the problem of escaping newlines or spaces,
-- because unlike other whitespace characters, the null character is
-- not valid in command-line arguments.
writeArgs :: Verbosity -> FilePath -> [String] -> IO ()
writeArgs verbosity path args = do
    createDirectoryIfMissingVerbose
      (lessVerbose verbosity) True (takeDirectory path)
    writeFile path (intercalate "\0" args)

-- | Read command-line arguments, separated by null characters, from a file.
-- Returns 'Nothing' if the file does not exist.
readArgs :: FilePath -> IO (Maybe [String])
readArgs path = do
  exists <- doesFileExist path
  if exists
     then liftM (Just . unintersperse '\0') (readFile path)
    else return Nothing

unintersperse :: Eq a => a -> [a] -> [[a]]
unintersperse _ [] = []
unintersperse mark list =
  let (this, rest) = break (== mark) list
  in case rest of
       [] -> [this]
       (_:rest') -> this : unintersperse mark rest'

-- -----------------------------------------------------------------------------
-- * Exceptions
-- -----------------------------------------------------------------------------

data ReconfigureError
    = ReconfigureErrorHelp Args
    | ReconfigureErrorList Args
    | ReconfigureErrorOther Args [String]
  deriving (Typeable)

instance Show ReconfigureError where
  show (ReconfigureErrorHelp args) =
    "reconfigure: unexpected flag '--help', saved command line was:\n"
    ++ intercalate " " args
  show (ReconfigureErrorList args) =
    "reconfigure: unexpected flag '--list-options', saved command line was:\n"
    ++ intercalate " " args
  show (ReconfigureErrorOther args errs) =
    "reconfigure: saved command line was:\n"
    ++ intercalate " " args ++ "\n"
    ++ "encountered errors:\n"
    ++ intercalate "\n" errs

instance Exception ReconfigureError
