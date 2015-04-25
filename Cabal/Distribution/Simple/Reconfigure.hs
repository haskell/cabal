{-# LANGUAGE DeriveDataTypeable #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Reconfigure
-- Copyright   :  Thomas Tuegel 2015
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This implements the /reconfigure/ command and the bookkeeping that goes with
-- it. It saves the configuration flags in a version-independent format and
-- restores them on demand.

module Distribution.Simple.Reconfigure
       ( ReconfigureError(..)
       , forceReconfigure
       , readArgs
       , reconfigure
       , setupConfigArgsFile
       , writeArgs
       ) where

import Distribution.Simple.Configure
    ( checkPersistBuildConfigOutdated, tryGetPersistBuildConfig )
import Distribution.Simple.Command
    ( CommandParse(..), commandAddAction, commandsRun )
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(..))
import Distribution.Simple.Program
    ( addKnownPrograms, builtinPrograms, defaultProgramConfiguration
    , restoreProgramConfiguration )
import Distribution.Simple.Setup (ConfigFlags, configureCommand, globalCommand)
import Distribution.Simple.UserHooks (Args, UserHooks(..))
import Distribution.Simple.Utils
    ( createDirectoryIfMissingVerbose, info, notice )
import Distribution.Verbosity (Verbosity, lessVerbose)

import Control.Exception (Exception, throwIO)
import Control.Monad (liftM)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Typeable
import System.Directory (doesFileExist)
import System.FilePath ((</>), takeDirectory)

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

-- | The path (relative to @--build-dir@) where the arguments to @configure@
-- should be saved.
setupConfigArgsFile :: FilePath -> FilePath
setupConfigArgsFile = (</> "setup-config-args")

-- | Read the 'localBuildInfoFile', reconfiguring if necessary. Throws
-- 'ConfigStateFileError' if the file cannot be read and the package cannot
-- be reconfigured.
reconfigure :: (UserHooks -> Args -> ConfigFlags -> Args -> IO LocalBuildInfo)
               -- ^ configure action
            -> (FilePath -> FilePath)
               -- ^ path to saved command-line arguments,
               -- relative to @--build-dir@
            -> UserHooks -> Verbosity -> FilePath
            -> IO LocalBuildInfo
reconfigure configureAction configArgsFile hooks verbosity distPref = do
    elbi <- tryGetPersistBuildConfig distPref
    lbi <- case elbi of
      Left err -> do info verbosity (show err)
                     forceReconfigure_
      Right lbi_wo_programs ->
        -- Restore info about unconfigured programs, since it is not serialized
        return lbi_wo_programs {
          withPrograms = restoreProgramConfiguration
                         (builtinPrograms ++ hookedPrograms hooks)
                         (withPrograms lbi_wo_programs)
        }
    case pkgDescrFile lbi of
      Nothing -> return lbi
      Just pkg_descr_file -> do
        outdated <- checkPersistBuildConfigOutdated distPref pkg_descr_file
        if outdated
          then do
            notice verbosity
              (pkg_descr_file ++ " has changed; reconfiguring...")
            forceReconfigure_
          else return lbi
  where
    forceReconfigure_ =
      forceReconfigure configureAction configArgsFile hooks verbosity distPref


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

-- | Reconfigure the package unconditionally.
forceReconfigure :: (UserHooks -> Args -> ConfigFlags -> Args -> IO LocalBuildInfo)
                    -- ^ configure action
                 -> (FilePath -> FilePath)
                    -- ^ path to saved command-line arguments,
                    -- relative to @--build-dir@
                 -> UserHooks -> Verbosity -> FilePath -> IO LocalBuildInfo
forceReconfigure configureAction configArgsFile hooks _ distPref = do
    saved <- readArgs (configArgsFile distPref)
    let args = fromMaybe ["configure"] saved
        commands =
          [ commandAddAction
              (configureCommand progs)
              (configureAction hooks args)
          ]
    case commandsRun (globalCommand commands) commands args of
      CommandHelp _ -> throwIO (ReconfigureErrorHelp args)
      CommandList _ -> throwIO (ReconfigureErrorList args)
      CommandErrors errs -> throwIO (ReconfigureErrorOther args errs)
      CommandReadyToGo (_, commandParse)  ->
        case commandParse of
          CommandHelp _ -> throwIO (ReconfigureErrorHelp args)
          CommandList _ -> throwIO (ReconfigureErrorList args)
          CommandErrors errs -> throwIO (ReconfigureErrorOther args errs)
          CommandReadyToGo action -> action
  where
    progs = addKnownPrograms (hookedPrograms hooks) defaultProgramConfiguration
