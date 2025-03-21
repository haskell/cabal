{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Distribution.Client.Toolchain
  ( Stage (..)
  , Staged (..)
  , Toolchain (..)
  , mkProgramDb
  , configToolchain
  , configToolchains
  , module Distribution.Solver.Types.Stage
  , module Distribution.Solver.Types.Toolchain
  )
where

import Distribution.Client.Setup (ConfigExFlags (..))
import Distribution.Simple (Compiler, CompilerFlavor)
import Distribution.Simple.Compiler (interpretPackageDBStack)
import Distribution.Simple.Configure
import Distribution.Simple.Program (ProgArg)
import Distribution.Simple.Program.Db
import Distribution.Simple.Setup
import Distribution.Solver.Types.Stage
import Distribution.Solver.Types.Toolchain
import Distribution.System (Platform)
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

configToolchains :: Verbosity -> ConfigFlags -> ConfigExFlags -> IO (Staged Toolchain)
configToolchains verbosity ConfigFlags{..} ConfigExFlags{..} = do
  programDb <-
    mkProgramDb
      verbosity
      (fromNubList configProgramPathExtra)
      configProgramPaths
      configProgramArgs

  hostToolchain <- do
    (toolchainCompiler, toolchainPlatform, toolchainProgramDb) <-
      configCompilerExSafe
        verbosity
        (flagToMaybe configHcFlavor)
        (flagToMaybe configHcPath)
        (flagToMaybe configHcPkg)
        programDb
    let toolchainPackageDBs = interpretPackageDBStack Nothing $ interpretPackageDbFlags False $ configPackageDBs
    return Toolchain{..}

  buildToolchain <- do
    (toolchainCompiler, toolchainPlatform, toolchainProgramDb) <-
      configCompilerExSafe
        verbosity
        (flagToMaybe configBuildHcFlavor)
        (flagToMaybe configBuildHcPath)
        (flagToMaybe configBuildHcPkg)
        programDb
    let toolchainPackageDBs = interpretPackageDBStack Nothing $ interpretPackageDbFlags False $ configPackageDBs
    return Toolchain{..}

  return $ Staged (\case Build -> buildToolchain; Host -> hostToolchain)

configCompilerExSafe
  :: Verbosity
  -> Maybe CompilerFlavor
  -> Maybe FilePath
  -> Maybe FilePath
  -> ProgramDb
  -> IO (Compiler, Platform, ProgramDb)
configCompilerExSafe verbosity hcFlavor hcPath hcPkg progdb = do
  (compiler, platform, progdb') <-
    configCompilerEx
      hcFlavor
      hcPath
      hcPkg
      progdb
      verbosity

  -- TODO: Redesign ProgramDB API to prevent such problems as #2241 in the future.
  -- I think this should be fixed in configCompilerExAux or even configCompilerEx
  progdb'' <- configureAllKnownPrograms verbosity progdb'
  return (compiler, platform, progdb'')
