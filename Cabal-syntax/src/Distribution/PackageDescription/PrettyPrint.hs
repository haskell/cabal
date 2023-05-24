{-# LANGUAGE OverloadedStrings #-}

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
import Distribution.FieldGrammar (PrettyFieldGrammar', prettyFieldGrammar)
import Distribution.Fields.Pretty
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

-- | Writes a .cabal file from a generic package description
writeGenericPackageDescription :: FilePath -> GenericPackageDescription -> IO ()
writeGenericPackageDescription fpath pkg = writeUTF8File fpath (showGenericPackageDescription pkg)

-- | Writes a generic package description to a string
showGenericPackageDescription :: GenericPackageDescription -> String
showGenericPackageDescription gpd = showFields (const NoComment) $ ppGenericPackageDescription v gpd
  where
    v = specVersion $ packageDescription gpd

-- | Convert a generic package description to 'PrettyField's.
ppGenericPackageDescription :: CabalSpecVersion -> GenericPackageDescription -> [PrettyField ()]
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

ppPackageDescription :: CabalSpecVersion -> PackageDescription -> [PrettyField ()]
ppPackageDescription v pd =
  prettyFieldGrammar v packageDescriptionFieldGrammar pd
    ++ ppSourceRepos v (sourceRepos pd)

ppSourceRepos :: CabalSpecVersion -> [SourceRepo] -> [PrettyField ()]
ppSourceRepos = map . ppSourceRepo

ppSourceRepo :: CabalSpecVersion -> SourceRepo -> PrettyField ()
ppSourceRepo v repo =
  PrettySection () "source-repository" [pretty kind] $
    prettyFieldGrammar v (sourceRepoFieldGrammar kind) repo
  where
    kind = repoKind repo

ppSetupBInfo :: CabalSpecVersion -> Maybe SetupBuildInfo -> [PrettyField ()]
ppSetupBInfo _ Nothing = mempty
ppSetupBInfo v (Just sbi)
  | defaultSetupDepends sbi = mempty
  | otherwise =
      pure $
        PrettySection () "custom-setup" [] $
          prettyFieldGrammar v (setupBInfoFieldGrammar False) sbi

ppGenPackageFlags :: CabalSpecVersion -> [PackageFlag] -> [PrettyField ()]
ppGenPackageFlags = map . ppFlag

ppFlag :: CabalSpecVersion -> PackageFlag -> PrettyField ()
ppFlag v flag@(MkPackageFlag name _ _ _) =
  PrettySection () "flag" [ppFlagName name] $
    prettyFieldGrammar v (flagFieldGrammar name) flag

ppCondTree2 :: CabalSpecVersion -> PrettyFieldGrammar' s -> CondTree ConfVar [Dependency] s -> [PrettyField ()]
ppCondTree2 v grammar = go
  where
    -- TODO: recognise elif opportunities
    go (CondNode it _ ifs) =
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
      , PrettySection () "else" [] (go elseTree)
      ]

ppCondLibrary :: CabalSpecVersion -> Maybe (CondTree ConfVar [Dependency] Library) -> [PrettyField ()]
ppCondLibrary _ Nothing = mempty
ppCondLibrary v (Just condTree) =
  pure $
    PrettySection () "library" [] $
      ppCondTree2 v (libraryFieldGrammar LMainLibName) condTree

ppCondSubLibraries :: CabalSpecVersion -> [(UnqualComponentName, CondTree ConfVar [Dependency] Library)] -> [PrettyField ()]
ppCondSubLibraries v libs =
  [ PrettySection () "library" [pretty n] $
    ppCondTree2 v (libraryFieldGrammar $ LSubLibName n) condTree
  | (n, condTree) <- libs
  ]

ppCondForeignLibs :: CabalSpecVersion -> [(UnqualComponentName, CondTree ConfVar [Dependency] ForeignLib)] -> [PrettyField ()]
ppCondForeignLibs v flibs =
  [ PrettySection () "foreign-library" [pretty n] $
    ppCondTree2 v (foreignLibFieldGrammar n) condTree
  | (n, condTree) <- flibs
  ]

ppCondExecutables :: CabalSpecVersion -> [(UnqualComponentName, CondTree ConfVar [Dependency] Executable)] -> [PrettyField ()]
ppCondExecutables v exes =
  [ PrettySection () "executable" [pretty n] $
    ppCondTree2 v (executableFieldGrammar n) condTree
  | (n, condTree) <- exes
  ]

ppCondTestSuites :: CabalSpecVersion -> [(UnqualComponentName, CondTree ConfVar [Dependency] TestSuite)] -> [PrettyField ()]
ppCondTestSuites v suites =
  [ PrettySection () "test-suite" [pretty n] $
    ppCondTree2 v testSuiteFieldGrammar (fmap FG.unvalidateTestSuite condTree)
  | (n, condTree) <- suites
  ]

ppCondBenchmarks :: CabalSpecVersion -> [(UnqualComponentName, CondTree ConfVar [Dependency] Benchmark)] -> [PrettyField ()]
ppCondBenchmarks v suites =
  [ PrettySection () "benchmark" [pretty n] $
    ppCondTree2 v benchmarkFieldGrammar (fmap FG.unvalidateBenchmark condTree)
  | (n, condTree) <- suites
  ]

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

ppIfCondition :: Condition ConfVar -> [PrettyField ()] -> PrettyField ()
ppIfCondition c = PrettySection () "if" [ppCondition c]

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
    mkCondTree x = CondNode x [] []
    mkCondTreeL l = (fromMaybe (mkUnqualComponentName "") (libraryNameString (libName l)), CondNode l [] [])

    mkCondTree'
      :: (a -> UnqualComponentName)
      -> a
      -> (UnqualComponentName, CondTree ConfVar [Dependency] a)
    mkCondTree' f x = (f x, CondNode x [] [])

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
      | pn == thisPn =
          if LMainLibName `NES.member` ln
            then Dependency thisPn vr mainLibSet : sublibs
            else sublibs
      where
        sublibs =
          [ Dependency (unqualComponentNameToPackageName uqn) vr mainLibSet
          | LSubLibName uqn <- NES.toList ln
          ]
    transformD d = [d]

    transformM :: Mixin -> Mixin
    transformM (Mixin pn (LSubLibName uqn) inc)
      | pn == thisPn =
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
      ++ [ PrettySection () "executable:" [pretty name] $
          prettyFieldGrammar cabalSpecLatest buildInfoFieldGrammar bi
         | (name, bi) <- ex_bis
         ]
