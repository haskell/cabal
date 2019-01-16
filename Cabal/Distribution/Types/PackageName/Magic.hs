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

-- | Used by @cabal new-repl@ and @cabal new-run@
fakePackageName :: PackageName
fakePackageName = mkPackageName "fake-package"

-- | 'fakePackageName' with 'version0'.
fakePackageId :: PackageId
fakePackageId = PackageIdentifier fakePackageName version0
