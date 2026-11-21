-- |
-- Module      :  Distribution.Simple.Program.Compiler
--
-- The executables found when configuring the compiler.
--
-- 'CompilerPrograms' is abstract: it gives the paths of the configured
-- executables, to say which compiler is in use, and gives programs to run
-- only together with a 'ProgramDb'. See
-- Note [Compiler, compiler program and ProgramDb] in
-- "Distribution.Types.LocalBuildConfig".
module Distribution.Simple.Program.Compiler
  ( CompilerPrograms
  , mkCompilerPrograms
  , compilerPath
  , compilerPkgPath
  , compilerSupports
  , compilerProgramToRun
  , compilerPkgProgramToRun
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Simple.Program.Db (ProgramDb, lookupProgramByName)
import Distribution.Simple.Program.GHC (GhcFeature, ghcSupports)
import Distribution.Simple.Program.Types
import Distribution.Simple.Utils (warn)
import Distribution.Verbosity (Verbosity)

-- | The compiler's executable and its package tool (@ghc-pkg@ for GHC), as
-- found when the compiler was configured.
data CompilerPrograms = CompilerPrograms ConfiguredProgram (Maybe ConfiguredProgram)
  deriving (Eq, Generic, Read, Show)

instance Binary CompilerPrograms
instance Structured CompilerPrograms

-- | Record the compiler's executable and its package tool, if it has one.
mkCompilerPrograms :: ConfiguredProgram -> Maybe ConfiguredProgram -> CompilerPrograms
mkCompilerPrograms = CompilerPrograms

-- | The path of the compiler's executable.
compilerPath :: CompilerPrograms -> FilePath
compilerPath (CompilerPrograms hc _) = programPath hc

-- | The path of the compiler's package tool, if it has one.
compilerPkgPath :: CompilerPrograms -> Maybe FilePath
compilerPkgPath (CompilerPrograms _ hcPkg) = programPath <$> hcPkg

-- | Whether the compiler supports a GHC feature, as detected when it was
-- configured. See 'ghcSupports'.
compilerSupports :: GhcFeature -> CompilerPrograms -> Bool
compilerSupports feature (CompilerPrograms hc _) = ghcSupports feature hc

-- | The compiler, ready to run with the arguments and environment that the
-- 'ProgramDb' adds to it. See 'programToRun'.
compilerProgramToRun :: Verbosity -> CompilerPrograms -> ProgramDb -> IO ConfiguredProgram
compilerProgramToRun verbosity (CompilerPrograms hc _) = programToRun verbosity hc

-- | The compiler's package tool, if it has one, ready to run with the
-- arguments and environment that the 'ProgramDb' adds to it. See
-- 'programToRun'.
compilerPkgProgramToRun :: Verbosity -> CompilerPrograms -> ProgramDb -> IO (Maybe ConfiguredProgram)
compilerPkgProgramToRun verbosity (CompilerPrograms _ hcPkg) progdb =
  traverse (\p -> programToRun verbosity p progdb) hcPkg

-- | Run the configured executable, never one found by searching, with the
-- arguments and environment of the program of the same name in the
-- 'ProgramDb'. If the 'ProgramDb' names a different executable, for instance
-- because a custom @Setup.hs@ replaced it, warn and run the configured one.
programToRun :: Verbosity -> ConfiguredProgram -> ProgramDb -> IO ConfiguredProgram
programToRun verbosity prog progdb =
  case lookupProgramByName (programId prog) progdb of
    Nothing -> return prog
    Just dbProg -> do
      when (programPath dbProg /= programPath prog) $
        warn verbosity $
          "The program database names "
            ++ programPath dbProg
            ++ " as "
            ++ programId prog
            ++ ", but the package was configured with "
            ++ programPath prog
            ++ "; running the latter."
      return
        prog
          { programDefaultArgs = programDefaultArgs dbProg
          , programOverrideArgs = programOverrideArgs dbProg
          , programOverrideEnv = programOverrideEnv dbProg
          }
