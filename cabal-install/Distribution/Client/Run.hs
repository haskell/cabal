-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Run
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Implementation of the 'run' command.
-----------------------------------------------------------------------------

module Distribution.Client.Run ( run, splitRunArgs )
       where

import Distribution.Client.Utils             (tryCanonicalizePath)

import Distribution.PackageDescription       (Executable (..),
                                              PackageDescription (..))
import Distribution.Simple.Build.PathsModule (pkgPathEnvVar)
import Distribution.Simple.BuildPaths        (exeExtension)
import Distribution.Simple.LocalBuildInfo    (ComponentName (..),
                                              LocalBuildInfo (..),
                                              getComponentLocalBuildInfo)
import Distribution.Simple.GHC               (toRPaths)
import Distribution.Simple.Utils             (die, notice, rawSystemExitWithEnv)
import Distribution.System                   (Platform (..), OS (..))
import Distribution.Utils.NubList            (fromNubListR)
import Distribution.Verbosity                (Verbosity)

import Data.Functor                          ((<$>))
import Data.List                             (find, intercalate)
import System.Directory                      (getCurrentDirectory)
import Distribution.Compat.Environment       (getEnvironment)
import Distribution.Client.Compat.Environment (lookupEnv)
import System.FilePath                       ((<.>), (</>), searchPathSeparator)


-- | Return the executable to run and any extra arguments that should be
-- forwarded to it.
splitRunArgs :: LocalBuildInfo -> [String] -> IO (Executable, [String])
splitRunArgs lbi args =
  case exes of
    []    -> die "Couldn't find any executables."
    [exe] -> case args of
      []                        -> return (exe, [])
      (x:xs) | x == exeName exe -> return (exe, xs)
             | otherwise        -> return (exe, args)
    _     -> case args of
      []     -> die $ "This package contains multiple executables. "
                ++ "You must pass the executable name as the first argument "
                ++ "to 'cabal run'."
      (x:xs) -> case find (\exe -> exeName exe == x) exes of
        Nothing  -> die $ "No executable named '" ++ x ++ "'."
        Just exe -> return (exe, xs)
  where
    pkg_descr = localPkgDescr lbi
    exes      = executables pkg_descr


-- | Run a given executable.
run :: Verbosity -> LocalBuildInfo -> Executable -> [String] -> IO ()
run verbosity lbi exe exeArgs = do
  curDir <- getCurrentDirectory
  let buildPref     = buildDir lbi
      pkg_descr     = localPkgDescr lbi
      dataDirEnvVar = (pkgPathEnvVar pkg_descr "datadir",
                       curDir </> dataDir pkg_descr)

  path <- tryCanonicalizePath $
          buildPref </> exeName exe </> (exeName exe <.> exeExtension)
  env  <- (dataDirEnvVar:) <$> getEnvironment
  env' <- addLibraryPath lbi exe env
  notice verbosity $ "Running " ++ exeName exe ++ "..."
  rawSystemExitWithEnv verbosity path exeArgs env'

addLibraryPath :: LocalBuildInfo -> Executable -> [(String,String)]
               -> IO [(String,String)]
addLibraryPath lbi exe env | relocatable lbi && withDynExe lbi = do
  let clbi = getComponentLocalBuildInfo lbi (CExeName (exeName exe))
  rpaths <- fromNubListR <$> toRPaths True False lbi clbi
  let libPaths = intercalate [searchPathSeparator] rpaths

  let (Platform _ os) = hostPlatform lbi
      ldPath = case os of
                 OSX -> "DYLD_LIBRARY_PATH"
                 _   -> "LD_LIBRARY_PATH"
  ldEnv <- maybe libPaths (++ (searchPathSeparator:libPaths)) <$>
           lookupEnv ldPath


  return (env ++ [(ldPath,ldEnv)])

addLibraryPath _ _ env = return env
