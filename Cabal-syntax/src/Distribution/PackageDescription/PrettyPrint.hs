{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      :  Distribution.PackageDescription.PrettyPrint
-- Copyright   :  Jürgen Nicklisch-Franken 2010
-- License     :  BSD3
--
-- Maintainer  : cabal-devel@haskell.org
-- Stability   : provisional
-- Portability : portable
--
-- Pretty printing for cabal files
module Distribution.PackageDescription.PrettyPrint
  ( -- * Generic package descriptions
    writeGenericPackageDescription
  , showGenericPackageDescription
  , ppGenericPackageDescription
  , ppGenericPackageDescriptionAnn

    -- * Package descriptions
  , writePackageDescription
  , showPackageDescription

    -- ** Supplementary build information
  , writeHookedBuildInfo
  , showHookedBuildInfo
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.CabalSpecVersion
import Distribution.Compat.Lens
import Distribution.FieldGrammar (PrettyFieldGrammarWith', PrettyFieldGrammar', prettyFieldGrammar)
import Distribution.Fields.Pretty
import Distribution.Parsec.Position
import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration (transformAllBuildInfos)
import Distribution.PackageDescription.FieldGrammar
  ( benchmarkFieldGrammar
  , buildInfoFieldGrammar
  , executableFieldGrammar
  , flagFieldGrammar
  , foreignLibFieldGrammar
  , libraryFieldGrammar
  , packageDescriptionFieldGrammar
  , setupBInfoFieldGrammar
  , sourceRepoFieldGrammar
  , testSuiteFieldGrammar
  )
import Distribution.Pretty
import Distribution.Utils.Generic (writeFileAtomic, writeUTF8File)

import qualified Distribution.PackageDescription.FieldGrammar as FG
import qualified Distribution.Types.BuildInfo.Lens as L
import qualified Distribution.Types.SetupBuildInfo.Lens as L

import Text.PrettyPrint (Doc, char, hsep, parens, text)

import qualified Data.ByteString.Lazy.Char8 as BS.Char8
import qualified Distribution.Compat.NonEmptySet as NES

import qualified Distribution.Types.Modify as Mod

-- | Writes a .cabal file from a generic package description
writeGenericPackageDescription :: FilePath -> GenericPackageDescription -> IO ()
writeGenericPackageDescription fpath pkg = writeUTF8File fpath (showGenericPackageDescription pkg)

-- | Writes a generic package description to a string
showGenericPackageDescription :: GenericPackageDescription -> String
showGenericPackageDescription gpd = showFields (const NoComment) $ ppGenericPackageDescription v gpd
  where
    v = specVersion $ packageDescription gpd

-- | Convert a generic package description to 'PrettyField's.
ppGenericPackageDescription :: CabalSpecVersion -> GenericPackageDescription -> [PrettyField]
ppGenericPackageDescription v gpd0 =
  concat
    [ ppPackageDescription v (packageDescription gpd)
    , ppSetupBInfo v (setupBuildInfo (packageDescription gpd))
    , ppGenPackageFlags v (genPackageFlags gpd)
    , ppCondLibrary v (condLibrary gpd)
    , ppCondSubLibraries v (condSubLibraries gpd)
    , ppCondForeignLibs v (condForeignLibs gpd)
    , ppCondExecutables v (condExecutables gpd)
    , ppCondTestSuites v (condTestSuites gpd)
    , ppCondBenchmarks v (condBenchmarks gpd)
    ]
  where
    gpd = preProcessInternalDeps (specVersion (packageDescription gpd0)) gpd0

-- | Convert a generic package description to 'PrettyField's.
ppGenericPackageDescriptionAnn :: CabalSpecVersion -> GenericPackageDescriptionWith Mod.HasAnn -> [PrettyFieldWith Mod.HasAnn]
ppGenericPackageDescriptionAnn v gpd0 =
  concat
    [ ppPackageDescriptionAnn v (packageDescription gpd)
    , ppSetupBInfoAnn v (setupBuildInfo (packageDescription gpd))
    , ppGenPackageFlagsAnn v (genPackageFlags gpd)
    , ppCondLibraryAnn v (condLibrary gpd)
    , ppCondSubLibrariesAnn v (condSubLibraries gpd)
    , ppCondForeignLibsAnn v (condForeignLibs gpd)
    , ppCondExecutablesAnn v (condExecutables gpd)
    -- TODO(leana8959): think of a strategy to handle endomorphisms
    -- , ppCondTestSuitesAnn v (condTestSuites gpd)
    -- , ppCondBenchmarksAnn v (condBenchmarks gpd)
    ]
  where
    -- TODO(leana8959): handle endomorphisms conversions / validations
    -- gpd = preProcessInternalDeps (specVersion (packageDescription gpd0)) gpd0
    gpd = gpd0

ppPackageDescription :: CabalSpecVersion -> PackageDescription -> [PrettyField]
ppPackageDescription v pd =
  prettyFieldGrammar v packageDescriptionFieldGrammar pd
    ++ ppSourceRepos v (sourceRepos pd)

ppPackageDescriptionAnn :: CabalSpecVersion -> PackageDescriptionWith Mod.HasAnn -> [PrettyFieldWith Mod.HasAnn]
ppPackageDescriptionAnn v pd =
  prettyFieldGrammar v packageDescriptionFieldGrammar pd
    ++ ppSourceReposAnn v (sourceRepos pd)

ppSourceRepos :: CabalSpecVersion -> [SourceRepo] -> [PrettyField]
ppSourceRepos = map . ppSourceRepo

ppSourceReposAnn :: CabalSpecVersion -> [SourceRepo] -> [PrettyFieldWith Mod.HasAnn]
ppSourceReposAnn = map . ppSourceRepoAnn

ppSourceRepo :: CabalSpecVersion -> SourceRepo -> PrettyField
ppSourceRepo v repo =
  PrettySection "source-repository" [pretty kind] $
    prettyFieldGrammar v (sourceRepoFieldGrammar @Mod.HasNoAnn kind) repo
  where
    kind = repoKind repo

ppSourceRepoAnn :: CabalSpecVersion -> SourceRepo -> PrettyFieldWith Mod.HasAnn
ppSourceRepoAnn v repo =
  -- TODO(leana8959): push out position
  PrettySection (zeroPos, "source-repository") [pretty kind] $
    prettyFieldGrammar v (sourceRepoFieldGrammar @Mod.HasAnn kind) repo
  where
    kind = repoKind repo

ppSetupBInfo :: CabalSpecVersion -> Maybe SetupBuildInfo -> [PrettyField]
ppSetupBInfo _ Nothing = mempty
ppSetupBInfo v (Just sbi)
  | defaultSetupDepends sbi = mempty
  | otherwise =
      pure $
        PrettySection "custom-setup" [] $
          prettyFieldGrammar v (setupBInfoFieldGrammar @Mod.HasNoAnn False) sbi

ppSetupBInfoAnn :: CabalSpecVersion -> Maybe SetupBuildInfo -> [PrettyFieldWith Mod.HasAnn]
ppSetupBInfoAnn _ Nothing = mempty
ppSetupBInfoAnn v (Just sbi)
  | defaultSetupDepends sbi = mempty
  | otherwise =
    -- TODO(leana8959): push out position
      pure $
        PrettySection (zeroPos, "custom-setup") [] $
          prettyFieldGrammar v (setupBInfoFieldGrammar @Mod.HasAnn False) sbi

ppGenPackageFlags :: CabalSpecVersion -> [PackageFlag] -> [PrettyField]
ppGenPackageFlags = map . ppFlag

ppGenPackageFlagsAnn :: CabalSpecVersion -> [PackageFlag] -> [PrettyFieldWith Mod.HasAnn]
ppGenPackageFlagsAnn = map . ppFlagAnn

ppFlag :: CabalSpecVersion -> PackageFlag -> PrettyField
ppFlag v flag@(MkPackageFlag name _ _ _) =
  PrettySection "flag" [ppFlagName name] $
    prettyFieldGrammar v (flagFieldGrammar @Mod.HasNoAnn name) flag

ppFlagAnn :: CabalSpecVersion -> PackageFlag -> PrettyFieldWith Mod.HasAnn
ppFlagAnn v flag@(MkPackageFlag name _ _ _) =
  -- TODO(leana8959): push out position
  PrettySection (zeroPos, "flag") [ppFlagName name] $
    prettyFieldGrammar v (flagFieldGrammar @Mod.HasAnn name) flag

ppCondTree2 :: CabalSpecVersion -> PrettyFieldGrammar' s -> CondTree ConfVar s -> [PrettyField]
ppCondTree2 v grammar = go
  where
    -- TODO: recognise elif opportunities
    go (CondNode it ifs) =
      prettyFieldGrammar v grammar it
        ++ concatMap ppIf ifs

    ppIf (CondBranch c thenTree Nothing)
      --        | isEmpty thenDoc = mempty
      | otherwise = [ppIfCondition c thenDoc]
      where
        thenDoc = go thenTree
    ppIf (CondBranch c thenTree (Just elseTree)) =
      -- See #6193
      [ ppIfCondition c (go thenTree)
      , PrettySection "else" [] (go elseTree)
      ]

ppCondTree2Ann :: CabalSpecVersion -> PrettyFieldGrammarWith' Mod.HasAnn s -> CondTree ConfVar s -> [PrettyFieldWith Mod.HasAnn]
ppCondTree2Ann v grammar = go
  where
    -- TODO: recognise elif opportunities
    go (CondNode it ifs) =
      prettyFieldGrammar v grammar it
        ++ concatMap ppIf ifs

    ppIf (CondBranch c thenTree Nothing)
      --        | isEmpty thenDoc = mempty
      | otherwise = [ppIfConditionAnn c thenDoc]
      where
        thenDoc = go thenTree
    ppIf (CondBranch c thenTree (Just elseTree)) =
      -- See #6193
      [ ppIfConditionAnn c (go thenTree)
      , PrettySection (zeroPos, "else") [] (go elseTree)
      ]

ppCondLibrary :: CabalSpecVersion -> Maybe (CondTree ConfVar Library) -> [PrettyField]
ppCondLibrary _ Nothing = mempty
ppCondLibrary v (Just condTree) =
  pure $
    PrettySection "library" [] $
      ppCondTree2 v (libraryFieldGrammar LMainLibName) condTree

ppCondLibraryAnn :: CabalSpecVersion -> Maybe (CondTree ConfVar (LibraryWith Mod.HasAnn)) -> [PrettyFieldWith Mod.HasAnn]
ppCondLibraryAnn _ Nothing = mempty
ppCondLibraryAnn v (Just condTree) =
  pure $
    PrettySection (zeroPos, "library") [] $
      ppCondTree2Ann v (libraryFieldGrammar LMainLibName) condTree

ppCondSubLibraries :: CabalSpecVersion -> [(UnqualComponentName, CondTree ConfVar Library)] -> [PrettyField]
ppCondSubLibraries v libs =
  [ PrettySection "library" [pretty n] $
    ppCondTree2 v (libraryFieldGrammar $ LSubLibName n) condTree
  | (n, condTree) <- libs
  ]

ppCondSubLibrariesAnn :: CabalSpecVersion -> [(UnqualComponentName, CondTree ConfVar (LibraryWith Mod.HasAnn))] -> [PrettyFieldWith Mod.HasAnn]
ppCondSubLibrariesAnn v libs =
  [ PrettySection (zeroPos, "library") [pretty n] $
    ppCondTree2Ann v (libraryFieldGrammar $ LSubLibName n) condTree
  | (n, condTree) <- libs
  ]

ppCondForeignLibs :: CabalSpecVersion -> [(UnqualComponentName, CondTree ConfVar ForeignLib)] -> [PrettyField]
ppCondForeignLibs v flibs =
  [ PrettySection "foreign-library" [pretty n] $
    ppCondTree2 v (foreignLibFieldGrammar n) condTree
  | (n, condTree) <- flibs
  ]

ppCondForeignLibsAnn :: CabalSpecVersion -> [(UnqualComponentName, CondTree ConfVar (ForeignLibWith Mod.HasAnn))] -> [PrettyFieldWith Mod.HasAnn]
ppCondForeignLibsAnn v flibs =
  [ PrettySection (zeroPos, "foreign-library") [pretty n] $
    ppCondTree2Ann v (foreignLibFieldGrammar n) condTree
  | (n, condTree) <- flibs
  ]

ppCondExecutables :: CabalSpecVersion -> [(UnqualComponentName, CondTree ConfVar Executable)] -> [PrettyField]
ppCondExecutables v exes =
  [ PrettySection "executable" [pretty n] $
    ppCondTree2 v (executableFieldGrammar n) condTree
  | (n, condTree) <- exes
  ]

ppCondExecutablesAnn :: CabalSpecVersion -> [(UnqualComponentName, CondTree ConfVar (ExecutableWith Mod.HasAnn))] -> [PrettyFieldWith Mod.HasAnn]
ppCondExecutablesAnn v exes =
  [ PrettySection (zeroPos, "executable") [pretty n] $
    ppCondTree2Ann v (executableFieldGrammar n) condTree
  | (n, condTree) <- exes
  ]

ppCondTestSuites :: CabalSpecVersion -> [(UnqualComponentName, CondTree ConfVar TestSuite)] -> [PrettyField]
ppCondTestSuites v suites =
  [ PrettySection "test-suite" [pretty n] $
    ppCondTree2 v testSuiteFieldGrammar (fmap FG.unvalidateTestSuite condTree)
  | (n, condTree) <- suites
  ]

-- ppCondTestSuitesAnn :: CabalSpecVersion -> [(UnqualComponentName, CondTree ConfVar (TestSuiteWith Mod.HasAnn))] -> [PrettyFieldWith Mod.HasAnn]
-- ppCondTestSuitesAnn v suites =
--   [ PrettySection "test-suite" [pretty n] $
--     ppCondTree2Ann v testSuiteFieldGrammar (fmap FG.unvalidateTestSuite condTree)
--   | (n, condTree) <- suites
--   ]


ppCondBenchmarks :: CabalSpecVersion -> [(UnqualComponentName, CondTree ConfVar Benchmark)] -> [PrettyField]
ppCondBenchmarks v suites =
  [ PrettySection "benchmark" [pretty n] $
    ppCondTree2 v benchmarkFieldGrammar (fmap FG.unvalidateBenchmark condTree)
  | (n, condTree) <- suites
  ]

-- ppCondBenchmarksAnn :: CabalSpecVersion -> [(UnqualComponentName, CondTree ConfVar (BenchmarkWith Mod.HasAnn))] -> [PrettyFieldWith Mod.HasAnn]
-- ppCondBenchmarksAnn v suites =
--   [ PrettySection "benchmark" [pretty n] $
--     ppCondTree2 v benchmarkFieldGrammar (fmap FG.unvalidateBenchmark condTree)
--   | (n, condTree) <- suites
--   ]

ppCondition :: Condition ConfVar -> Doc
ppCondition (Var x) = ppConfVar x
ppCondition (Lit b) = text (show b)
ppCondition (CNot c) = char '!' <<>> (ppCondition c)
ppCondition (COr c1 c2) =
  parens
    ( hsep
        [ ppCondition c1
        , text "||"
            <+> ppCondition c2
        ]
    )
ppCondition (CAnd c1 c2) =
  parens
    ( hsep
        [ ppCondition c1
        , text "&&"
            <+> ppCondition c2
        ]
    )
ppConfVar :: ConfVar -> Doc
ppConfVar (OS os) = text "os" <<>> parens (pretty os)
ppConfVar (Arch arch) = text "arch" <<>> parens (pretty arch)
ppConfVar (PackageFlag name) = text "flag" <<>> parens (ppFlagName name)
ppConfVar (Impl c v) = text "impl" <<>> parens (pretty c <+> pretty v)

ppFlagName :: FlagName -> Doc
ppFlagName = text . unFlagName

ppIfCondition :: Condition ConfVar -> [PrettyField] -> PrettyField
ppIfCondition c = PrettySection "if" [ppCondition c]

ppIfConditionAnn :: Condition ConfVar -> [PrettyFieldWith Mod.HasAnn] -> PrettyFieldWith Mod.HasAnn
ppIfConditionAnn c = PrettySection (zeroPos, "if") [ppCondition c]

-- | @since 2.0.0.2
writePackageDescription :: FilePath -> PackageDescription -> IO ()
writePackageDescription fpath pkg = writeUTF8File fpath (showPackageDescription pkg)

-- TODO: make this use section syntax
-- add equivalent for GenericPackageDescription

-- | @since 2.0.0.2
showPackageDescription :: PackageDescription -> String
showPackageDescription = showGenericPackageDescription . pdToGpd

pdToGpd :: PackageDescription -> GenericPackageDescription
pdToGpd pd =
  GenericPackageDescription
    { packageDescription = pd
    , gpdScannedVersion = Nothing
    , genPackageFlags = []
    , condLibrary = mkCondTree <$> library pd
    , condSubLibraries = mkCondTreeL <$> subLibraries pd
    , condForeignLibs = mkCondTree' foreignLibName <$> foreignLibs pd
    , condExecutables = mkCondTree' exeName <$> executables pd
    , condTestSuites = mkCondTree' testName <$> testSuites pd
    , condBenchmarks = mkCondTree' benchmarkName <$> benchmarks pd
    }
  where
    -- We set CondTree's [Dependency] to an empty list, as it
    -- is not pretty printed anyway.
    mkCondTree x = CondNode x []
    mkCondTreeL l = (fromMaybe (mkUnqualComponentName "") (libraryNameString (libName l)), CondNode l [])

    mkCondTree'
      :: (a -> UnqualComponentName)
      -> a
      -> (UnqualComponentName, CondTree ConfVar a)
    mkCondTree' f x = (f x, CondNode x [])

-------------------------------------------------------------------------------
-- Internal libs
-------------------------------------------------------------------------------

-- See Note [Dependencies on sublibraries] in Distribution.PackageDescription.Parsec
--
preProcessInternalDeps :: CabalSpecVersion -> GenericPackageDescription -> GenericPackageDescription
preProcessInternalDeps specVer gpd
  | specVer >= CabalSpecV3_4 = gpd
  | otherwise = transformAllBuildInfos transformBI transformSBI gpd
  where
    transformBI :: BuildInfo -> BuildInfo
    transformBI =
      over L.targetBuildDepends (concatMap transformD)
        . over L.mixins (map transformM)

    transformSBI :: SetupBuildInfo -> SetupBuildInfo
    transformSBI = over L.setupDepends (concatMap transformD)

    transformD :: Dependency -> [Dependency]
    transformD (Dependency pn vr ln)
      | pn == thisPn && specVer < CabalSpecV3_0 =
          if LMainLibName `NES.member` ln
            then Dependency thisPn vr mainLibSet : sublibs
            else sublibs
      where
        sublibs =
          [ Dependency (unqualComponentNameToPackageName uqn) vr mainLibSet
          | LSubLibName uqn <- NES.toList ln
          ]
    transformD d = [d]

    -- Always perform transformation for mixins as syntax was only introduced in 3.4
    -- This guard is unnecessary as no transformations take place when cabalSpec >= CabalSpecV3_4 but
    -- it more clearly signifies the intent. (See the specVer >= CabalSpecV3_4 line above).
    transformM :: Mixin -> Mixin
    transformM (Mixin pn (LSubLibName uqn) inc)
      | pn == thisPn && specVer < CabalSpecV3_4 =
          mkMixin (unqualComponentNameToPackageName uqn) LMainLibName inc
    transformM m = m

    thisPn :: PackageName
    thisPn = pkgName (package (packageDescription gpd))

-------------------------------------------------------------------------------
-- HookedBuildInfo
-------------------------------------------------------------------------------

-- | @since 2.0.0.2
writeHookedBuildInfo :: FilePath -> HookedBuildInfo -> IO ()
writeHookedBuildInfo fpath =
  writeFileAtomic fpath
    . BS.Char8.pack
    . showHookedBuildInfo

-- | @since 2.0.0.2
showHookedBuildInfo :: HookedBuildInfo -> String
showHookedBuildInfo (mb_lib_bi, ex_bis) =
  showFields (const NoComment) $
    maybe mempty (prettyFieldGrammar cabalSpecLatest buildInfoFieldGrammar) mb_lib_bi
      ++ [ PrettySection "executable:" [pretty name] $
          prettyFieldGrammar cabalSpecLatest buildInfoFieldGrammar bi
         | (name, bi) <- ex_bis
         ]
