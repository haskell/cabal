-- | Magic 'PackageName's.
--
-- @since 3.0.0.0
module Distribution.Types.PackageName.Magic where

import Distribution.Types.PackageId
import Distribution.Types.PackageName
import Distribution.Types.Version

-- | Used as a placeholder in "Distribution.Backpack.ReadyComponent"
nonExistentPackageThisIsCabalBug :: PackageName
nonExistentPackageThisIsCabalBug = mkPackageName "nonexistent-package-this-is-a-cabal-bug"

-- | Used by @cabal new-repl@, @cabal new-run@ and @cabal new-build@
fakePackageName :: PackageName
fakePackageName = mkPackageName "fake-package"

-- | Used by @cabal new-run@ and @cabal new-build@
fakePackageCabalFileName :: FilePath
fakePackageCabalFileName = "fake-package.cabal"

-- | 'fakePackageName' with 'version0'.
fakePackageId :: PackageId
fakePackageId = PackageIdentifier fakePackageName version0
