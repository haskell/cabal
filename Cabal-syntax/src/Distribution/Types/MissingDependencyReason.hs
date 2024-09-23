module Distribution.Types.MissingDependencyReason
  ( MissingDependencyReason (..)
  ) where

import Data.List.NonEmpty (NonEmpty)
import Distribution.Types.LibraryName (LibraryName)
import Distribution.Types.PackageName (PackageName)
import Distribution.Types.Version (Version)

-- | A reason for a depency failing to solve.
--
-- This helps pinpoint dependencies that are installed with an incorrect
-- version vs. dependencies that are not installed at all.
data MissingDependencyReason
  = -- | One or more libraries is missing.
    MissingLibrary (NonEmpty LibraryName)
  | -- | A package is not installed.
    MissingPackage
  | -- | A package is installed, but the versions don't match.
    --
    -- Contains the available versions.
    WrongVersion [Version]
  | -- | A component is not installed.
    MissingComponent PackageName
  deriving (Show)
