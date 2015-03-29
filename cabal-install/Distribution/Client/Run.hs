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
                                              TestSuite(..),
                                              Benchmark(..),
                                              PackageDescription (..),
                                              BuildInfo(buildable))
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
import Distribution.Verbosity                (Verbosity, normal)

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
  -- cannot printWarning at the start, for the case where the first
  -- parameter matches the executable that is run.
  case enabledExes of
    []    -> do
               printWarning
               die "Couldn't find any enabled executables."
    [exe] -> case args of
      []                        -> return (exe, [])
      (x:xs) | x == exeName exe -> return (exe, xs)
             | otherwise -> do
                              case firstParamComponentWarning of
                                Nothing -> return ()
                                Just s ->
                                  notice normal
                                    (s ++ " Interpreting '" ++ x
                                       ++ "' as a parameter to the default"
                                       ++ " executable.")
                              return (exe, args)
    _     -> case args of
      []     -> die $ "This package contains multiple executables. "
                ++ "You must pass the executable name as the first argument "
                ++ "to 'cabal run'."
      (x:xs) -> case find (\exe -> exeName exe == x) enabledExes of
        Nothing  -> do
                      printWarning
                      die $ "No executable named '" ++ x ++ "'."
        Just exe -> return (exe, xs)
  where
    pkg_descr   = localPkgDescr lbi
    enabledExes = filter (buildable . buildInfo) (executables pkg_descr)
    firstParamComponentWarning :: Maybe String
    firstParamComponentWarning = case args of
      []    -> Nothing
      (x:_) -> lookup x components
      where
        components :: [(String, String)] -- component name, label
        components = [ (name, "The executable '" ++ name ++ "' is disabled.")
                     | e <- executables pkg_descr
                     , let name = exeName e]
                  ++ [ (name, "There is a test-suite '" ++ name ++ "',"
                           ++ " but the `run` command is only for"
                           ++ " executables.")
                     | t <- testSuites pkg_descr
                     , let name = testName t]
                  ++ [ (name, "There is a benchmark '" ++ name ++ "',"
                           ++ " but the run command is only for"
                           ++ " executables.")
                     | b <- benchmarks pkg_descr
                     , let name = benchmarkName b]
    printWarning :: IO ()
    printWarning = case firstParamComponentWarning of
      Nothing -> return ()
      Just x  -> notice normal x


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
