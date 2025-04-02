{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Distribution.Client.Toolchain
  ( Stage (..)
  , Staged (..)
  , Toolchain (..)
  , mkProgramDb
  , configToolchain
  , module Distribution.Solver.Types.Stage
  , module Distribution.Solver.Types.Toolchain
  )
where

import Distribution.Simple.Compiler (interpretPackageDBStack)
import Distribution.Simple.Configure
import Distribution.Simple.Program (ProgArg)
import Distribution.Simple.Program.Db
import Distribution.Simple.Setup
import Distribution.Solver.Types.Stage
import Distribution.Solver.Types.Toolchain
import Distribution.Utils.NubList
import Distribution.Verbosity (Verbosity)

mkProgramDb
  :: Verbosity
  -> [FilePath]
  -> [(String, FilePath)]
  -> [(String, [ProgArg])]
  -> IO ProgramDb
mkProgramDb verbosity extraSearchPath extraPaths extraArgs = do
  progdb <- prependProgramSearchPath verbosity extraSearchPath [] defaultProgramDb
  -- ProgramDb with directly user specified paths
  return $
    userSpecifyPaths extraPaths $
      userSpecifyArgss extraArgs progdb

-- | Configure the toolchain
configToolchain :: ConfigFlags -> IO Toolchain
configToolchain configFlags@ConfigFlags{..} = do
  programDb <-
    mkProgramDb
      verbosity
      (fromNubList configProgramPathExtra)
      configProgramPaths
      configProgramArgs

  (toolchainCompiler, toolchainPlatform, progdb) <-
    configCompilerEx
      (flagToMaybe configHcFlavor)
      (flagToMaybe configHcPath)
      (flagToMaybe configHcPkg)
      programDb
      verbosity

  -- TODO: Redesign ProgramDB API to prevent such problems as #2241 in the
  -- future.
  toolchainProgramDb <- configureAllKnownPrograms verbosity progdb
  let toolchainPackageDBs = interpretPackageDBStack Nothing $ interpretPackageDbFlags False $ configPackageDBs

  return Toolchain{..}
  where
    -- FIXME
    verbosity = fromFlag (configVerbosity configFlags)
