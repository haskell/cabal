module UnitTests.Distribution.Client.Init.Utils
( dummyFlags
, emptyFlags
, mkLicense
, baseVersion
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
import Distribution.Simple.Compiler
import Distribution.Pretty


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

-- | Retireves the proper base version based on the GHC version
baseVersion :: Compiler -> VersionRange 
baseVersion Compiler {compilerId = CompilerId GHC ver} =
  let ghcToBase = baseVersion' . prettyShow $ ver in
        if null ghcToBase
          then anyVersion
          else majorBoundVersion $ mkVersion ghcToBase
baseVersion _ = anyVersion

baseVersion' :: String -> [Int]
baseVersion' "9.0.1"  = [4,15,0,0]
baseVersion' "8.10.4" = [4,14,1,0]
baseVersion' "8.10.3" = [4,14,1,0]
baseVersion' "8.10.2" = [4,14,1,0]
baseVersion' "8.10.1" = [4,14,0,0]
baseVersion' "8.8.4"  = [4,13,0,0]
baseVersion' "8.8.3"  = [4,13,0,0]
baseVersion' "8.8.2"  = [4,13,0,0]
baseVersion' "8.8.1"  = [4,13,0,0]
baseVersion' "8.6.5"  = [4,12,0,0]
baseVersion' "8.6.4"  = [4,12,0,0]
baseVersion' "8.6.3"  = [4,12,0,0]
baseVersion' "8.6.2"  = [4,12,0,0]
baseVersion' "8.6.1"  = [4,12,0,0]
baseVersion' "8.4.4"  = [4,11,1,0]
baseVersion' "8.4.3"  = [4,11,1,0]
baseVersion' "8.4.2"  = [4,11,1,0]
baseVersion' "8.4.1"  = [4,11,0,0]
baseVersion' "8.2.2"  = [4,10,1,0]
baseVersion' "8.2.1"  = [4,10,0,0]
baseVersion' "8.0.2"  = [4,10,0,0]
baseVersion' "8.0.1"  = [4,9,1,0]
baseVersion' "7.10.3" = [4,9,0,0]
baseVersion' "7.10.2" = [4,8,2,0]
baseVersion' "7.10.1" = [4,8,1,0]
baseVersion' "7.8.4"  = [4,8,0,0]
baseVersion' "7.8.3"  = [4,7,0,2]
baseVersion' "7.8.2"  = [4,7,0,1]
baseVersion' "7.8.1"  = [4,7,0,0]
baseVersion' "7.6.3"  = [4,7,0,0]
baseVersion' "7.6.2"  = [4,6,0,1]
baseVersion' "7.6.1"  = [4,6,0,0]
baseVersion' "7.4.2"  = [4,5,1,0]
baseVersion' "7.4.1"  = [4,5,0,0]
baseVersion' "7.2.2"  = [4,4,1,0]
baseVersion' "7.2.1"  = [4,4,0,0]
baseVersion' "7.0.4"  = [4,3,1,0]
baseVersion' "7.0.3"  = [4,3,1,0]
baseVersion' "7.0.2"  = [4,3,1,0]
baseVersion' "7.0.1"  = [4,3,0,0]
baseVersion' _ = []

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
