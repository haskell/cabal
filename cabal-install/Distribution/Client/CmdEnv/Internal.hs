{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Distribution.Client.CmdEnv.Internal
  ( environmentFileToSpecifiers
  , globalPackages
  ) where

import Prelude ()
import Distribution.Client.Compat.Prelude

import Distribution.Client.Types
       ( PackageSpecifier(..) )

import Distribution.Package
       ( PackageName, mkPackageName )
import Distribution.Simple.GHC
       ( GhcEnvironmentFileEntry(..) )
import Distribution.Types.InstalledPackageInfo
       ( InstalledPackageInfo(..) )
import Distribution.Types.PackageId
       ( PackageIdentifier(..) )
import Distribution.Types.VersionRange
         ( thisVersion )
import qualified Distribution.Simple.PackageIndex as PI
       ( InstalledPackageIndex, lookupUnitId )
import Distribution.Solver.Types.PackageConstraint
       ( PackageProperty(..) )

environmentFileToSpecifiers
  :: PI.InstalledPackageIndex -> [GhcEnvironmentFileEntry]
  -> ([PackageSpecifier a], [GhcEnvironmentFileEntry])
environmentFileToSpecifiers ipi = foldMap $ \case
    (GhcEnvFilePackageId unitId)
        | Just InstalledPackageInfo
          { sourcePackageId = PackageIdentifier{..}, installedUnitId }
          <- PI.lookupUnitId ipi unitId
        , let pkgSpec = NamedPackage pkgName
                        [PackagePropertyVersion (thisVersion pkgVersion)]
        -> if pkgName `elem` globalPackages
          then ([pkgSpec], [])
          else ([pkgSpec], [GhcEnvFilePackageId installedUnitId])
    _ -> ([], [])

-- | List of globally installed packages that come with GHC.
globalPackages :: [PackageName]
globalPackages = mkPackageName <$>
  [ "ghc", "hoopl", "bytestring", "unix", "base", "time", "hpc", "filepath"
  , "process", "array", "integer-gmp", "containers", "ghc-boot", "binary"
  , "ghc-prim", "ghci", "rts", "terminfo", "transformers", "deepseq"
  , "ghc-boot-th", "pretty", "template-haskell", "directory", "text"
  , "bin-package-db"
  ]
