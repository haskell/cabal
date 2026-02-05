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
  , showGenericPackageDescription'
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

import Distribution.Types.Annotation

import Distribution.CabalSpecVersion
import Distribution.Compat.Lens
import Distribution.FieldGrammar (PrettyFieldGrammar', prettierFieldGrammar, prettyFieldGrammar)
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

import Distribution.Parsec.Position

import qualified Distribution.PackageDescription.FieldGrammar as FG
import qualified Distribution.Types.BuildInfo.Lens as L
import qualified Distribution.Types.SetupBuildInfo.Lens as L

import Debug.Pretty.Simple
import Text.PrettyPrint (Doc, char, hsep, parens, text)

import qualified Data.ByteString.Lazy.Char8 as BS.Char8
import qualified Distribution.Compat.NonEmptySet as NES

-- | Writes a .cabal file from a generic package description
writeGenericPackageDescription :: FilePath -> GenericPackageDescription -> IO ()
writeGenericPackageDescription fpath pkg = writeUTF8File fpath (showGenericPackageDescription pkg)

-- | Writes a generic package description to a string
showGenericPackageDescription :: GenericPackageDescription -> String
showGenericPackageDescription = flip showGenericPackageDescription' mempty

showGenericPackageDescription' :: GenericPackageDescription -> TriviaTree -> String
showGenericPackageDescription' gpd t =
  -- TODO(leana8959): we can later use the "showFields" mechanism to inject the comments stored from the comment PR
  showFields (const NoComment) $
    ppGenericPackageDescription' v t gpd
  where
    v = specVersion $ packageDescription gpd

ppGenericPackageDescription :: CabalSpecVersion -> GenericPackageDescription -> [PrettyField Trivia]
ppGenericPackageDescription = flip ppGenericPackageDescription' mempty

-- | Convert a generic package description to 'PrettyField's.
ppGenericPackageDescription' :: CabalSpecVersion -> TriviaTree -> GenericPackageDescription -> [PrettyField Trivia]
ppGenericPackageDescription' v t gpd0 =
  concat
    [ ppPackageDescription v t (packageDescription gpd)
    , ppSetupBInfo v t (setupBuildInfo (packageDescription gpd))
    , ppGenPackageFlags v t (genPackageFlags gpd)
    , ppCondLibrary v t (condLibrary gpd)
    , ppCondSubLibraries v t (condSubLibraries gpd)
    , ppCondForeignLibs v t (condForeignLibs gpd)
    , ppCondExecutables v t (condExecutables gpd)
    , ppCondTestSuites v t (condTestSuites gpd)
    , ppCondBenchmarks v t (condBenchmarks gpd)
    ]
  where
    gpd = preProcessInternalDeps (specVersion (packageDescription gpd0)) gpd0

ppPackageDescription :: CabalSpecVersion -> TriviaTree -> PackageDescription -> [PrettyField Trivia]
ppPackageDescription v t pd =
  prettierFieldGrammar v t packageDescriptionFieldGrammar pd
    ++ ppSourceRepos v t (sourceRepos pd)

ppSourceRepos :: CabalSpecVersion -> TriviaTree -> [SourceRepo] -> [PrettyField Trivia]
ppSourceRepos v t = map (ppSourceRepo v t)

ppSourceRepo :: CabalSpecVersion -> TriviaTree -> SourceRepo -> PrettyField Trivia
ppSourceRepo v t repo =
  PrettySection mempty "source-repository" [pretty kind] $
    prettierFieldGrammar v t (sourceRepoFieldGrammar kind) repo
  where
    kind = repoKind repo

ppSetupBInfo :: CabalSpecVersion -> TriviaTree -> Maybe SetupBuildInfo -> [PrettyField Trivia]
ppSetupBInfo _ _ Nothing = mempty
ppSetupBInfo v t (Just sbi)
  | defaultSetupDepends sbi = mempty
  | otherwise =
      pure $
        PrettySection mempty "custom-setup" [] $
          prettierFieldGrammar v t (setupBInfoFieldGrammar False) sbi

ppGenPackageFlags :: CabalSpecVersion -> TriviaTree -> [PackageFlag] -> [PrettyField Trivia]
ppGenPackageFlags v t = map (ppFlag v t)

ppFlag :: CabalSpecVersion -> TriviaTree -> PackageFlag -> PrettyField Trivia
ppFlag v t flag@(MkPackageFlag name _ _ _) =
  PrettySection mempty "flag" [ppFlagName name] $
    prettierFieldGrammar v t (flagFieldGrammar name) flag

-- TODO(leana8959): deal with condtree
ppCondTree2 :: CabalSpecVersion -> TriviaTree -> PrettyFieldGrammar' s -> CondTree ConfVar [Dependency] s -> [PrettyField Trivia]
ppCondTree2 v t grammar = go
  where
    -- TODO: recognise elif opportunities
    go (CondNode it _ ifs) =
      prettierFieldGrammar v t grammar it
        ++ concatMap ppIf ifs

    ppIf (CondBranch c thenTree Nothing)
      --        | isEmpty thenDoc = mempty
      | otherwise = [ppIfCondition c thenDoc]
      where
        thenDoc = go thenTree
    ppIf (CondBranch c thenTree (Just elseTree)) =
      -- See #6193
      [ ppIfCondition c (go thenTree)
      , PrettySection mempty "else" [] (go elseTree)
      ]

ppCondLibrary :: CabalSpecVersion -> TriviaTree -> Maybe (CondTree ConfVar [Dependency] Library) -> [PrettyField Trivia]
ppCondLibrary _ _ Nothing = mempty
ppCondLibrary v t (Just condTree) =
  pure $
    PrettySection mempty "library" [] $
      ppCondTree2 v t (libraryFieldGrammar LMainLibName) condTree

ppCondSubLibraries :: CabalSpecVersion -> TriviaTree -> [(UnqualComponentName, CondTree ConfVar [Dependency] Library)] -> [PrettyField Trivia]
ppCondSubLibraries v t libs =
  [ PrettySection mempty "library" [pretty n] $
    ppCondTree2 v t (libraryFieldGrammar $ LSubLibName n) condTree
  | (n, condTree) <- libs
  ]

ppCondForeignLibs :: CabalSpecVersion -> TriviaTree -> [(UnqualComponentName, CondTree ConfVar [Dependency] ForeignLib)] -> [PrettyField Trivia]
ppCondForeignLibs v t flibs =
  [ PrettySection mempty "foreign-library" [pretty n] $
    ppCondTree2 v t (foreignLibFieldGrammar n) condTree
  | (n, condTree) <- flibs
  ]

ppCondExecutables :: CabalSpecVersion -> TriviaTree -> [(UnqualComponentName, CondTree ConfVar [Dependency] Executable)] -> [PrettyField Trivia]
ppCondExecutables v t exes =
  [ PrettySection mempty "executable" [pretty n] $
    ppCondTree2 v t (executableFieldGrammar n) condTree
  | (n, condTree) <- exes
  ]

ppCondTestSuites :: CabalSpecVersion -> TriviaTree -> [(UnqualComponentName, CondTree ConfVar [Dependency] TestSuite)] -> [PrettyField Trivia]
ppCondTestSuites v t suites =
  [ PrettySection mempty "test-suite" [pretty n] $
    ppCondTree2 v t testSuiteFieldGrammar (fmap FG.unvalidateTestSuite condTree)
  | (n, condTree) <- suites
  ]

ppCondBenchmarks :: CabalSpecVersion -> TriviaTree -> [(UnqualComponentName, CondTree ConfVar [Dependency] Benchmark)] -> [PrettyField Trivia]
ppCondBenchmarks v t suites =
  [ PrettySection mempty "benchmark" [pretty n] $
    ppCondTree2 v t benchmarkFieldGrammar (fmap FG.unvalidateBenchmark condTree)
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

ppIfCondition :: Condition ConfVar -> [PrettyField Trivia] -> PrettyField Trivia
ppIfCondition c = PrettySection mempty "if" [ppCondition c]

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
    -- This guard is uncessary as no transformations take place when cabalSpec >= CabalSpecV3_4 but
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
      ++ [ PrettySection mempty "executable:" [pretty name] $
          prettyFieldGrammar cabalSpecLatest buildInfoFieldGrammar bi
         | (name, bi) <- ex_bis
         ]
