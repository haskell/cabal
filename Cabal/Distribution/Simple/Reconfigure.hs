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
       ( forceReconfigure
       , readArgs
       , reconfigure
       , setupConfigArgsFile
       , writeArgs
       ) where

import Distribution.Simple.Configure
    ( checkPersistBuildConfigOutdated, tryGetPersistBuildConfig )
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(..))
import Distribution.Simple.Program
    ( builtinPrograms, restoreProgramConfiguration )
import Distribution.Simple.UserHooks (Args, UserHooks(..))
import Distribution.Simple.Utils
    ( createDirectoryIfMissingVerbose, notice )
import Distribution.Verbosity (Verbosity, lessVerbose)

import Control.Exception (throwIO)
import Control.Monad (liftM)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
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
reconfigure :: (UserHooks -> Args -> IO ())
               -- ^ entry point, used to reconfigure when needed
               -- (defaults to 'defaultMainHelper')
            -> (FilePath -> FilePath)
               -- ^ path, relative to @--build-dir@, to the
               -- saved @configure@ command-line arguments
            -> UserHooks -> Verbosity -> FilePath
            -> IO LocalBuildInfo
reconfigure entryPoint configArgsFile hooks verbosity distPref =
  reconfigure_go True where
    reconfigure_go retry = do
      elbi <- tryGetPersistBuildConfig distPref
      lbi <- case elbi of
        Left err | retry -> do notice verbosity (show err)
                               forceReconfigure entryPoint configArgsFile hooks verbosity distPref
                               reconfigure_go False
                 | otherwise -> throwIO err
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
              notice verbosity $ pkg_descr_file ++ " has changed; reconfiguring..."
              forceReconfigure entryPoint configArgsFile hooks verbosity distPref
              reconfigure_go False
            else return lbi

forceReconfigure :: (UserHooks -> Args -> IO ()) -> (FilePath -> FilePath) -> UserHooks -> Verbosity -> FilePath -> IO ()
forceReconfigure entryPoint configArgsFile hooks _ distPref = do
    saved <- readArgs (configArgsFile distPref)
    let args = fromMaybe ["configure"] saved
    entryPoint hooks args
