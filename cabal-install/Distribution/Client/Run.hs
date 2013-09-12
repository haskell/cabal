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
import Distribution.Simple.LocalBuildInfo    (LocalBuildInfo (..))
import Distribution.Simple.Utils             (die, rawSystemExitWithEnv)
import Distribution.Verbosity                (Verbosity)

import Data.Functor                          ((<$>))
import Data.List                             (find)
import System.Directory                      (getCurrentDirectory)
import Distribution.Compat.Environment       (getEnvironment)
import System.FilePath                       ((<.>), (</>))


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
  rawSystemExitWithEnv verbosity path exeArgs env
