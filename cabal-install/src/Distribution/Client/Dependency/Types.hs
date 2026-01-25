{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}

module Distribution.Client.Dependency.Types
  ( PreSolver (..)
  , Solver (..)
  , PackagesPreferenceDefault (..)
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import qualified Distribution.Compat.CharParsing as P
import GHC.IO (unsafePerformIO)

-- | All the solvers that can be selected.
data PreSolver = AlwaysModular
  deriving (Eq, Ord, Show, Bounded, Enum, Generic)

-- | All the solvers that can be used.
data Solver = Modular
  deriving (Eq, Ord, Show, Bounded, Enum, Generic)

instance Binary PreSolver
instance Binary Solver

instance NFData PreSolver

instance Structured PreSolver
instance Structured Solver

instance Parsec PreSolver where
  parsec :: CabalParsing m => m PreSolver
  parsec = do
    name <- P.munch1 isAlpha
    case map toLower name of
      "modular" -> return $ unsafePerformIO $ do
        putStrLn "[WARNING] The `solver' config option is deprecated and will be removed in a future release."
        return AlwaysModular
      _ -> P.unexpected $ "PreSolver: " ++ name

-- | Global policy for all packages to say if we prefer package versions that
-- are already installed locally or if we just prefer the latest available.
data PackagesPreferenceDefault
  = -- | Always prefer the latest version irrespective of any existing
    -- installed version.
    --
    -- * This is the standard policy for upgrade.
    PreferAllLatest
  | -- | Always prefer the oldest version irrespective of any existing
    -- installed version or packages explicitly requested.
    --
    -- * This is enabled by --prefer-oldest.
    PreferAllOldest
  | -- | Always prefer the installed versions over ones that would need to be
    -- installed. Secondarily, prefer latest versions (eg the latest installed
    -- version or if there are none then the latest source version).
    PreferAllInstalled
  | -- | Prefer the latest version for packages that are explicitly requested
    -- but prefers the installed version for any other packages.
    --
    -- * This is the standard policy for install.
    PreferLatestForSelected
  deriving (Show)
