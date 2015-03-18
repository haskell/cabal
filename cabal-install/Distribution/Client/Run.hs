{-# LANGUAGE CPP #-}
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
import Distribution.Simple.Compiler          (compilerFlavor, CompilerFlavor(..))
import Distribution.Simple.Build.PathsModule (pkgPathEnvVar)
import Distribution.Simple.BuildPaths        (exeExtension)
import Distribution.Simple.LocalBuildInfo    (ComponentName (..),
                                              LocalBuildInfo (..),
                                              getComponentLocalBuildInfo,
                                              depLibraryPaths)
import Distribution.Simple.Utils             (die, notice, rawSystemExitWithEnv,
                                              addLibraryPath)
import Distribution.System                   (Platform (..))
import Distribution.Verbosity                (Verbosity)

import qualified Distribution.Simple.GHCJS as GHCJS

#if !MIN_VERSION_base(4,8,0)
import Data.Functor                          ((<$>))
#endif
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

  (path, runArgs) <-
    case compilerFlavor (compiler lbi) of
      GHCJS -> do
        let (script, cmd, cmdArgs) =
              GHCJS.runCmd (withPrograms lbi)
                           (buildPref </> exeName exe </> exeName exe)
        script' <- tryCanonicalizePath script
        return (cmd, cmdArgs ++ [script'])
      _     -> do
         p <- tryCanonicalizePath $
            buildPref </> exeName exe </> (exeName exe <.> exeExtension)
         return (p, [])

  env  <- (dataDirEnvVar:) <$> getEnvironment
  -- Add (DY)LD_LIBRARY_PATH if needed
  env' <- if withDynExe lbi
             then do let (Platform _ os) = hostPlatform lbi
                         clbi = getComponentLocalBuildInfo lbi
                                  (CExeName (exeName exe))
                     paths <- depLibraryPaths True False lbi clbi
                     return (addLibraryPath os paths env)
             else return env
  notice verbosity $ "Running " ++ exeName exe ++ "..."
  rawSystemExitWithEnv verbosity path (runArgs++exeArgs) env'
