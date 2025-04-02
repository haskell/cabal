{-# LANGUAGE DeriveGeneric #-}

module Distribution.Solver.Types.Toolchain
  ( Toolchain (..)
  , Toolchains
  , Stage (..)
  , Staged (..)
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Simple.Compiler
import Distribution.Simple.Program.Db
import Distribution.Solver.Types.Stage (getStage, Stage (..), Staged (..))
import Distribution.System

---------------------------
-- Toolchain
--

data Toolchain = Toolchain
  { toolchainPlatform :: Platform
  , toolchainCompiler :: Compiler
  , toolchainProgramDb :: ProgramDb
  -- NOTE: actually the solver does not care about package dbs, perhaps it's better
  -- to have a separate Toolchain type for project planning.
  , toolchainPackageDBs :: PackageDBStackCWD
  }
  deriving (Show, Generic)

-- TODO: review this
instance Eq Toolchain where
  lhs == rhs =
    (((==) `on` toolchainPlatform) lhs rhs)
      && (((==) `on` toolchainCompiler) lhs rhs)
      && ((((==)) `on` (configuredPrograms . toolchainProgramDb)) lhs rhs)

instance Binary Toolchain
instance Structured Toolchain

type Toolchains = Staged Toolchain
