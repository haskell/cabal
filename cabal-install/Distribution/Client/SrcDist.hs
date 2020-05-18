-- | Utilities to implemenet cabal @v2-sdist@.
module Distribution.Client.SrcDist (
    allPackageSourceFiles,
)  where

import Distribution.Solver.Compat.Prelude
import Prelude ()

import Distribution.PackageDescription.Configuration (flattenPackageDescription)
import Distribution.PackageDescription.Parsec        (readGenericPackageDescription)
import Distribution.Simple.PreProcess                (knownSuffixHandlers)
import Distribution.Simple.SrcDist                   (listPackageSourcesWithDie)
import Distribution.Verbosity                        (Verbosity)

import Distribution.Client.Utils (tryFindAddSourcePackageDesc)

-- | List all source files of a given add-source dependency. Exits with error if
-- something is wrong (e.g. there is no .cabal file in the given directory).
--
-- Used in sandbox and projectbuilding.
-- TODO: when sandboxes are removed, move to ProjectBuilding.
--
allPackageSourceFiles :: Verbosity -> FilePath -> IO [FilePath]
allPackageSourceFiles verbosity packageDir = do
  pd <- do
    let err = "Error reading source files of package."
    desc <- tryFindAddSourcePackageDesc verbosity packageDir err
    flattenPackageDescription `fmap` readGenericPackageDescription verbosity desc

  listPackageSourcesWithDie verbosity (\_ _ -> return []) packageDir pd knownSuffixHandlers

