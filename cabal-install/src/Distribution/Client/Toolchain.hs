{-# LANGUAGE DeriveGeneric #-}

module Distribution.Client.Toolchain
  ( Toolchain (..)
  , Toolchains
  , module Distribution.Solver.Types.Stage
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Distribution.Simple.Compiler (Compiler)
import Distribution.Simple.Program.Db (ProgramDb)
import Distribution.System (Platform)

import Distribution.Solver.Types.Stage

-- | Everything the build tooling for one build 'Stage' consists of: the
-- compiler, the platform it targets, and the program database used to invoke
-- it. These three values are configured together and, from here on, travel
-- together instead of as a loose @(Compiler, Platform, ProgramDb)@ tuple.
data Toolchain = Toolchain
  { toolchainCompiler :: Compiler
  , toolchainPlatform :: Platform
  , toolchainProgramDb :: ProgramDb
  }
  deriving (Show, Generic)

-- | A 'Toolchain' per build 'Stage'. In an ordinary build both stages hold the
-- same toolchain (see 'always'); under cross-compilation the build and host
-- stages differ.
type Toolchains = Staged Toolchain
