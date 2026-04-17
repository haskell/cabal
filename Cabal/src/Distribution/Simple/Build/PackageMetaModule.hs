-----------------------------------------------------------------------------

-- |
-- Module      :  Distribution.Simple.Build.PackageMetaModule
-- Copyright   :  Moritz Angermann <moritz.angermann@iohk.io>
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Generating the PackageMeta_pkgname module.
--
-- This is a module that Cabal generates for the benefit of packages. It
-- enables them to find build-environment metadata such as compiler info,
-- platform, cabal flags, and git revision.
module Distribution.Simple.Build.PackageMetaModule
  ( generatePackageMetaModule
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Compiler (CompilerId (..), compilerId)
import Distribution.Package
  ( PackageName
  , packageName
  , unPackageName
  )
import Distribution.Pretty (prettyShow)
import Distribution.System (Platform (..))
import Distribution.Types.Flag (FlagName, unFlagAssignment, unFlagName)
import Distribution.Types.LocalBuildInfo (LocalBuildInfo (..))
import Distribution.Types.PackageDescription (PackageDescription)
import Distribution.Types.Version (versionNumbers)

import qualified Distribution.Simple.Build.PackageMetaModule.Z as Z

-- ------------------------------------------------------------

-- * Building PackageMeta_<pkg>.hs

-- ------------------------------------------------------------

-- | Generate the source code for the @PackageMeta_<pkgname>@ module.
--
-- The @gitRev@ and @gitDirty@ parameters are passed in because computing
-- them requires IO (running @git@ commands), and this function is pure.
generatePackageMetaModule
  :: PackageDescription
  -> LocalBuildInfo
  -> String
  -- ^ Git revision hash (empty string if unavailable)
  -> Bool
  -- ^ Whether the working tree has uncommitted changes
  -> String
generatePackageMetaModule pkg_descr lbi gitRev gitDirty =
  Z.render
    Z.Z
      { Z.zPackageName = showPkgName $ packageName pkg_descr
      , Z.zCompilerFlavour = prettyShow flavour
      , Z.zCompilerName = prettyShow cid
      , Z.zCompilerVersionDigits = show $ versionNumbers ver
      , Z.zOs = prettyShow os
      , Z.zArch = prettyShow arch
      , Z.zGitRevision = gitRev
      , Z.zGitDirty = gitDirty
      , Z.zFlags = map toFlagZ $ unFlagAssignment $ flagAssignment lbi
      }
  where
    cid@(CompilerId flavour ver) = compilerId $ compiler lbi
    Platform arch os = hostPlatform lbi

    toFlagZ :: (FlagName, Bool) -> Z.FlagZ
    toFlagZ (fn, val) =
      Z.FlagZ
        { Z.zFlagName = unFlagName fn
        , Z.zFlagHaskellName = "flag_" ++ map fixchar (unFlagName fn)
        , Z.zFlagValue = val
        }

showPkgName :: PackageName -> String
showPkgName = map fixchar . unPackageName

fixchar :: Char -> Char
fixchar '-' = '_'
fixchar c = c
