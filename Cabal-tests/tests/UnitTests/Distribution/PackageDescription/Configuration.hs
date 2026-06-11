module UnitTests.Distribution.PackageDescription.Configuration (tests) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Compiler
  ( AbiTag (NoAbiTag)
  , CompilerFlavor (GHC, GHCJS)
  , CompilerId (CompilerId)
  , CompilerInfo
  , unknownCompilerInfo
  )
import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration (buildToolVersion, finalizePD)
import Distribution.System (Arch (X86_64), OS (Linux), Platform (Platform))
import Distribution.Types.BuildTool (BuildTool (Cabal, MCabal, OtherBuildTool))
import Distribution.Types.ComponentRequestedSpec (defaultComponentRequestedSpec)
import Distribution.Types.DependencySatisfaction (DependencySatisfaction (Satisfied))
import Distribution.Version (anyVersion, laterVersion, mkVersion, orLaterVersion)

import Test.Tasty
import Test.Tasty.HUnit

tests :: [TestTree]
tests =
  [ -- The version constraint is satisfied: the Cabal library version is
    -- (trivially) at least itself, so the @if@ branch is chosen.
    selects "builder(cabal >= self)" (builder Cabal (orLaterVersion buildToolVersion)) "IF"
  , -- An omitted version range parses to 'anyVersion', which always matches.
    selects "builder(cabal) [no range]" (builder Cabal anyVersion) "IF"
  , -- The version constraint is not satisfiable: nothing is strictly later
    -- than itself, so the @else@ branch is chosen.
    selects "builder(cabal > self)" (builder Cabal (laterVersion buildToolVersion)) "ELSE"
  , -- A different known tool never matches when Cabal is the builder.
    selects "builder(mcabal)" (builder MCabal anyVersion) "ELSE"
  , -- An unknown tool parses to 'OtherBuildTool' and never matches.
    selects "builder(frobnicate)" (builder (OtherBuildTool "frobnicate") anyVersion) "ELSE"
  , -- builder(...) is evaluated against the Cabal library version, not the
    -- configured compiler, so the chosen branch must not depend on it.
    testCase "builder is compiler-independent" $
      let cond = builder Cabal (orLaterVersion buildToolVersion)
       in finalize ghc (mkGPD cond) @?= finalize ghcjs (mkGPD cond)
  ]
  where
    builder t vr = Var (Builder t vr)

    selects name cond expected =
      testCase name $ finalize ghc (mkGPD cond) @?= Right [expected]

-- | A package whose library sets @cpp-options@ to a single marker, chosen by
-- @if cond@ (marker @"IF"@) vs. its @else@ branch (marker @"ELSE"@).
mkGPD :: Condition ConfVar -> GenericPackageDescription
mkGPD cond =
  emptyGenericPackageDescription
    { condLibrary = Just (CondNode emptyLibrary [CondBranch cond (leaf "IF") (Just (leaf "ELSE"))])
    }
  where
    leaf marker = CondNode emptyLibrary{libBuildInfo = emptyBuildInfo{cppOptions = [marker]}} []

-- | Finalize and read back the library's @cpp-options@ (the branch marker).
finalize :: CompilerInfo -> GenericPackageDescription -> Either String [String]
finalize ci gpd =
  case finalizePD mempty defaultComponentRequestedSpec (const Satisfied) (Platform X86_64 Linux) ci [] gpd of
    Left missing -> Left (show missing)
    Right (pd, _) -> Right (maybe [] (cppOptions . libBuildInfo) (library pd))

ghc :: CompilerInfo
ghc = unknownCompilerInfo (CompilerId GHC (mkVersion [9, 10])) NoAbiTag

ghcjs :: CompilerInfo
ghcjs = unknownCompilerInfo (CompilerId GHCJS (mkVersion [1, 0])) NoAbiTag
