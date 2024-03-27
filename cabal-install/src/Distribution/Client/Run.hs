-----------------------------------------------------------------------------
{-# LANGUAGE DataKinds #-}

-----------------------------------------------------------------------------

-- |
-- Module      :  Distribution.Client.Run
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Implementation of the 'run' command.
module Distribution.Client.Run (run, splitRunArgs)
where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Distribution.Types.LocalBuildInfo (componentNameTargets')
import Distribution.Types.TargetInfo (targetCLBI)

import Distribution.Client.Utils (tryCanonicalizePath)

import Distribution.PackageDescription
  ( Benchmark (..)
  , BuildInfo (buildable)
  , Executable (..)
  , PackageDescription (..)
  , TestSuite (..)
  )
import Distribution.Simple.Build.PathsModule (pkgPathEnvVar)
import Distribution.Simple.BuildPaths (exeExtension)
import Distribution.Simple.Compiler (CompilerFlavor (..), compilerFlavor)
import Distribution.Simple.LocalBuildInfo
  ( ComponentName (..)
  , LocalBuildInfo (..)
  , buildDir
  , depLibraryPaths
  , interpretSymbolicPathLBI
  , mbWorkDirLBI
  )
import Distribution.Simple.Utils
  ( addLibraryPath
  , dieWithException
  , notice
  , rawSystemExitWithEnvCwd
  , warn
  )
import Distribution.System (Platform (..))
import Distribution.Types.UnqualComponentName

import qualified Distribution.Simple.GHCJS as GHCJS

import Distribution.Client.Errors
import Distribution.Compat.Environment (getEnvironment)
import Distribution.Utils.Path

-- | Return the executable to run and any extra arguments that should be
-- forwarded to it. Die in case of error.
splitRunArgs
  :: Verbosity
  -> LocalBuildInfo
  -> [String]
  -> IO (Executable, [String])
splitRunArgs verbosity lbi args =
  case whichExecutable of -- Either err (wasManuallyChosen, exe, paramsRest)
    Left err -> do
      warn verbosity `traverse_` maybeWarning -- If there is a warning, print it.
      dieWithException verbosity $ SplitRunArgs err
    Right (True, exe, xs) -> return (exe, xs)
    Right (False, exe, xs) -> do
      let addition =
            " Interpreting all parameters to `run` as a parameter to"
              ++ " the default executable."
      -- If there is a warning, print it together with the addition.
      warn verbosity `traverse_` fmap (++ addition) maybeWarning
      return (exe, xs)
  where
    pkg_descr = localPkgDescr lbi
    whichExecutable
      :: Either
          String -- Error string.
          ( Bool -- If it was manually chosen.
          , Executable -- The executable.
          , [String] -- The remaining parameters.
          )
    whichExecutable = case (enabledExes, args) of
      ([], _) -> Left "Couldn't find any enabled executables."
      ([exe], []) -> return (False, exe, [])
      ([exe], (x : xs))
        | x == unUnqualComponentName (exeName exe) -> return (True, exe, xs)
        | otherwise -> return (False, exe, args)
      (_, []) ->
        Left $
          "This package contains multiple executables. "
            ++ "You must pass the executable name as the first argument "
            ++ "to 'cabal run'."
      (_, (x : xs)) ->
        case find (\exe -> unUnqualComponentName (exeName exe) == x) enabledExes of
          Nothing -> Left $ "No executable named '" ++ x ++ "'."
          Just exe -> return (True, exe, xs)
      where
        enabledExes = filter (buildable . buildInfo) (executables pkg_descr)

    maybeWarning :: Maybe String
    maybeWarning = case args of
      [] -> Nothing
      (x : _) -> lookup (mkUnqualComponentName x) components
      where
        components :: [(UnqualComponentName, String)] -- Component name, message.
        components =
          [ (name, "The executable '" ++ prettyShow name ++ "' is disabled.")
          | e <- executables pkg_descr
          , not . buildable . buildInfo $ e
          , let name = exeName e
          ]
            ++ [ ( name
                 , "There is a test-suite '"
                    ++ prettyShow name
                    ++ "',"
                    ++ " but the `run` command is only for executables."
                 )
               | t <- testSuites pkg_descr
               , let name = testName t
               ]
            ++ [ ( name
                 , "There is a benchmark '"
                    ++ prettyShow name
                    ++ "',"
                    ++ " but the `run` command is only for executables."
                 )
               | b <- benchmarks pkg_descr
               , let name = benchmarkName b
               ]

-- | Run a given executable.
run :: Verbosity -> LocalBuildInfo -> Executable -> [String] -> IO ()
run verbosity lbi exe exeArgs = do
  let buildPref = buildDir lbi
      pkg_descr = localPkgDescr lbi
      i = interpretSymbolicPathLBI lbi -- See Note [Symbolic paths] in Distribution.Utils.Path
      mbWorkDir = mbWorkDirLBI lbi
      rawDataDir = dataDir pkg_descr
      datDir
        | null $ getSymbolicPath rawDataDir =
            i sameDirectory
        | otherwise =
            i rawDataDir
      dataDirEnvVar =
        ( pkgPathEnvVar pkg_descr "datadir"
        , datDir
        )

  (path, runArgs) <-
    let exeName' = prettyShow $ exeName exe
     in case compilerFlavor (compiler lbi) of
          GHCJS -> do
            let (script, cmd, cmdArgs) =
                  GHCJS.runCmd
                    (withPrograms lbi)
                    (i buildPref </> exeName' </> exeName')
            script' <- tryCanonicalizePath script
            return (cmd, cmdArgs ++ [script'])
          _ -> do
            p <-
              tryCanonicalizePath $
                i buildPref </> exeName' </> (exeName' <.> exeExtension (hostPlatform lbi))
            return (p, [])

  env <- (dataDirEnvVar :) <$> getEnvironment
  -- Add (DY)LD_LIBRARY_PATH if needed
  env' <-
    if withDynExe lbi
      then do
        let (Platform _ os) = hostPlatform lbi
        clbi <- case componentNameTargets' pkg_descr lbi (CExeName (exeName exe)) of
          [target] -> return (targetCLBI target)
          [] -> dieWithException verbosity CouldNotFindExecutable
          _ -> dieWithException verbosity FoundMultipleMatchingExes
        paths <- depLibraryPaths True False lbi clbi
        return (addLibraryPath os paths env)
      else return env
  notice verbosity $ "Running " ++ prettyShow (exeName exe) ++ "..."
  rawSystemExitWithEnvCwd verbosity mbWorkDir path (runArgs ++ exeArgs) env'
