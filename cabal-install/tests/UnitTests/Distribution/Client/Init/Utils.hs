module UnitTests.Distribution.Client.Init.Utils
( dummyFlags
, emptyFlags
, mkLicense
, mangleBaseDep
, (@?!)
, (@!?)
) where


import Distribution.Client.Init.Types

import qualified Distribution.SPDX as SPDX

import Distribution.CabalSpecVersion
import Distribution.Simple.Setup
import Distribution.Types.PackageName
import Distribution.Types.Version
import Language.Haskell.Extension
import Test.Tasty.HUnit
import Distribution.Types.Dependency
import Distribution.Types.VersionRange


-- -------------------------------------------------------------------- --
-- Test flags

dummyFlags :: InitFlags
dummyFlags = emptyFlags
  { noComments          = Flag True
  , packageName         = Flag (mkPackageName "QuxPackage")
  , version             = Flag (mkVersion [4,2,6])
  , cabalVersion        = Flag CabalSpecV2_2
  , license             = Flag $ SPDX.License $ SPDX.ELicense (SPDX.ELicenseId SPDX.MIT) Nothing
  , author              = Flag "Foobar"
  , email               = Flag "foobar@qux.com"
  , homepage            = Flag "qux.com"
  , synopsis            = Flag "We are Qux, and this is our package"
  , category            = Flag "Control"
  , language            = Flag Haskell98
  , initializeTestSuite = Flag True
  , sourceDirs          = Flag ["quxSrc"]
  , testDirs            = Flag ["quxTest"]
  , applicationDirs     = Flag ["quxApp"]
  }

emptyFlags :: InitFlags
emptyFlags = mempty

-- -------------------------------------------------------------------- --
-- Test utils

mkLicense :: SPDX.LicenseId -> SPDX.License
mkLicense lid = SPDX.License (SPDX.ELicense (SPDX.ELicenseId lid) Nothing)

mangleBaseDep :: a -> (a -> [Dependency]) -> [Dependency]
mangleBaseDep target f =
    [ if unPackageName x == "base"
        then Dependency x anyVersion z
        else dep
    | dep@(Dependency x _ z) <- f target
    ]

infix 1 @?!, @!?

-- | Just like @'@?='@, except it checks for difference rather than equality.
(@?!)
  :: (Eq a, Show a, HasCallStack)
  => a
  -> a
  -> Assertion
actual @?! unexpected = assertBool
                          ("unexpected: " ++ show unexpected)
                          (actual /= unexpected)

-- | Just like @'@=?'@, except it checks for difference rather than equality.
(@!?)
  :: (Eq a, Show a, HasCallStack)
  => a
  -> a
  -> Assertion
(@!?) = flip (@?!)
