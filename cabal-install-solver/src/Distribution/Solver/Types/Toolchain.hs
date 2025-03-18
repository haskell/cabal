{-# LANGUAGE DeriveGeneric #-}

module Distribution.Solver.Types.Toolchain
  ( Toolchain (..)
  , Toolchains (..)
  , toolchainFor
  , mkToolchainsWithHost
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Simple.Compiler
import Distribution.Simple.Program.Db
import Distribution.Solver.Types.Stage (Stage (..))
import Distribution.System

---------------------------
-- Toolchain
--

data Toolchain = Toolchain
  { toolchainPlatform :: Platform
  , toolchainCompiler :: Compiler
  , toolchainProgramDb :: ProgramDb
  }
  deriving (Show, Generic, Typeable)

-- TODO: review this
instance Eq Toolchain where
  lhs == rhs =
    (((==) `on` toolchainPlatform) lhs rhs)
      && (((==) `on` toolchainCompiler) lhs rhs)
      && ((((==)) `on` (configuredPrograms . toolchainProgramDb)) lhs rhs)

instance Binary Toolchain
instance Structured Toolchain

data Toolchains = Toolchains
  { buildToolchain :: Toolchain
  , hostToolchain :: Toolchain
  }
  deriving (Eq, Show, Generic, Typeable)

toolchainFor :: Stage -> Toolchains -> Toolchain
toolchainFor Build = buildToolchain
toolchainFor Host = hostToolchain

instance Binary Toolchains
instance Structured Toolchains

mkToolchainsWithHost :: Platform -> Compiler -> Toolchains
mkToolchainsWithHost platform compiler =
  Toolchains
    { buildToolchain = Toolchain platform compiler (error "build program db missing")
    , hostToolchain = Toolchain platform compiler (error "host program db missing")
    }
