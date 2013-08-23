-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Run
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Implementation of the 'run' command.
-----------------------------------------------------------------------------

module Distribution.Client.Run ( run )
       where

import Distribution.Client.Setup             (BuildFlags (..))
import Distribution.Client.SetupWrapper      (SetupScriptOptions (..),
                                              defaultSetupScriptOptions)
import Distribution.Client.Utils             (tryCanonicalizePath)

import Distribution.PackageDescription       (Executable (..),
                                              PackageDescription (..))
import Distribution.Simple.Build.PathsModule (pkgPathEnvVar)
import Distribution.Simple.BuildPaths        (exeExtension)
import Distribution.Simple.Configure         (getPersistBuildConfig)
import Distribution.Simple.LocalBuildInfo    (LocalBuildInfo (..))
import Distribution.Simple.Setup             (fromFlagOrDefault)
import Distribution.Simple.Utils             (die, rawSystemExitWithEnv)
import Distribution.Verbosity                (Verbosity)

import Data.Functor                          ((<$>))
import Data.List                             (find)
import System.Directory                      (getCurrentDirectory)
import Distribution.Compat.Environment       (getEnvironment)
import System.FilePath                       ((<.>), (</>))


run :: Verbosity -> BuildFlags -> [String] -> IO ()
run verbosity buildFlags args = do
  let distPref = fromFlagOrDefault (useDistPref defaultSetupScriptOptions)
                 (buildDistPref buildFlags)
  -- The package must have been configured by now.
  lbi <- getPersistBuildConfig distPref

  curDir <- getCurrentDirectory
  let buildPref     = buildDir lbi
      pkg_descr     = localPkgDescr lbi
      exes          = executables pkg_descr
      dataDirEnvVar = (pkgPathEnvVar pkg_descr "datadir",
                       curDir </> dataDir pkg_descr)

      exePath :: Executable -> FilePath
      exePath exe = buildPref </> exeName exe </> (exeName exe <.> exeExtension)

      doRun :: Executable -> [String] -> IO ()
      doRun exe exeArgs = do
        path <- tryCanonicalizePath $ exePath exe
        env <- (dataDirEnvVar:) <$> getEnvironment
        rawSystemExitWithEnv verbosity path exeArgs env

  case exes of
    []    -> die "Couldn't find any executables."
    [exe] -> case args of
      []                        -> doRun exe []
      (x:xs) | x == exeName exe -> doRun exe xs
             | otherwise        -> doRun exe args
    _     -> case args of
      []     -> die $ "This package contains multiple executables. "
                ++ "You must pass the executable name as the first argument "
                ++ "to run."
      (x:xs) -> case find (\exe -> exeName exe == x) exes of
        Nothing  -> die $ "No executable named '" ++ x ++ "'."
        Just exe -> doRun exe xs
