{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.PackageDescription.PackageSourceDescriptionPrettyPrint
-- License     :  BSD3
--
-- Maintainer  : cabal-devel@haskell.org
-- Stability   : provisional
-- Portability : portable
--
-- Pretty printing for cabal files
--
-----------------------------------------------------------------------------

module Distribution.PackageDescription.PackageSourceDescriptionPrettyPrint (
    -- * Generic package descriptions
    writePackageSourceDescription,
    showPackageSourceDescription,
    ppPackageSourceDescription,

     -- ** Supplementary build information
     writeHookedBuildInfo,
     showHookedBuildInfo,
) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Types.CondTree
import Distribution.Types.Dependency
import Distribution.Types.ForeignLib          (ForeignLib)
import Distribution.Types.LibraryName
import Distribution.Types.UnqualComponentName

import Distribution.CabalSpecVersion
import Distribution.Fields.Pretty
import Distribution.PackageDescription
       (PackageDescription, SourceRepo, SetupBuildInfo, Flag(..), FlagName, ConfVar(..),
        Library, Executable, TestSuite, Benchmark, Condition(..), HookedBuildInfo,
        specVersion, setupBuildInfo, sourceRepos, repoKind, defaultSetupDepends, unFlagName)
import Distribution.Pretty
import Distribution.Simple.Utils
import Distribution.Types.PackageSourceDescription
       (PackageSourceDescription, packageDescription, genPackageFlags, condCommonStanzas,
        condLibrary, condSubLibraries, condForeignLibs, condExecutables, condTestSuites,
        condBenchmarks)
import Distribution.Types.CommonStanza (CommonStanza)
import Distribution.Types.Version (versionNumbers)

import Distribution.FieldGrammar
       (PrettyFieldGrammar', prettyFieldGrammar)
import Distribution.PackageDescription.FieldGrammar
       (benchmarkFieldGrammar, buildInfoFieldGrammar, commonStanzaFieldGrammar, executableFieldGrammar, flagFieldGrammar,
       foreignLibFieldGrammar, libraryFieldGrammar, packageDescriptionFieldGrammar,
       setupBInfoFieldGrammar, sourceRepoFieldGrammar, testSuiteFieldGrammar)

import qualified Distribution.PackageDescription.FieldGrammar as FG

import Text.PrettyPrint (Doc, char, hsep, parens, text, (<+>))

import qualified Data.ByteString.Lazy.Char8 as BS.Char8

-- | Writes a .cabal file from a generic package description
writePackageSourceDescription :: FilePath -> PackageSourceDescription -> IO ()
writePackageSourceDescription fpath pkg = writeUTF8File fpath (showPackageSourceDescription pkg)

-- | Writes a generic package description to a string
showPackageSourceDescription :: PackageSourceDescription -> String
showPackageSourceDescription gpd = showFields (const []) $ ppPackageSourceDescription v gpd
  where
    v = cabalSpecFromVersionDigits
      $ versionNumbers
      $ specVersion
      $ packageDescription gpd

-- | Convert a generic package description to 'PrettyField's.
ppPackageSourceDescription :: CabalSpecVersion -> PackageSourceDescription -> [PrettyField ()]
ppPackageSourceDescription v gpd = concat
    [ ppPackageDescription v (packageDescription gpd)
    , ppSetupBInfo v (setupBuildInfo (packageDescription gpd))
    , ppGenPackageFlags v (genPackageFlags gpd)
    , ppCommonStanzas v (condCommonStanzas gpd)
    , ppCondLibrary v (condLibrary gpd)
    , ppCondSubLibraries v (condSubLibraries gpd)
    , ppCondForeignLibs v (condForeignLibs gpd)
    , ppCondExecutables v (condExecutables gpd)
    , ppCondTestSuites v (condTestSuites gpd)
    , ppCondBenchmarks v (condBenchmarks gpd)
    ]

ppPackageDescription :: CabalSpecVersion -> PackageDescription -> [PrettyField ()]
ppPackageDescription v pd =
    prettyFieldGrammar v packageDescriptionFieldGrammar pd
    ++ ppSourceRepos v (sourceRepos pd)

ppSourceRepos :: CabalSpecVersion -> [SourceRepo] -> [PrettyField ()]
ppSourceRepos = map . ppSourceRepo

ppSourceRepo :: CabalSpecVersion -> SourceRepo -> PrettyField ()
ppSourceRepo v repo = PrettySection () "source-repository" [pretty kind] $
    prettyFieldGrammar v (sourceRepoFieldGrammar kind) repo
  where
    kind = repoKind repo

ppSetupBInfo :: CabalSpecVersion -> Maybe SetupBuildInfo -> [PrettyField ()]
ppSetupBInfo _ Nothing = mempty
ppSetupBInfo v (Just sbi)
    | defaultSetupDepends sbi = mempty
    | otherwise = pure $ PrettySection () "custom-setup" [] $
        prettyFieldGrammar v (setupBInfoFieldGrammar False) sbi

ppGenPackageFlags :: CabalSpecVersion -> [Flag] -> [PrettyField ()]
ppGenPackageFlags = map . ppFlag

ppFlag :: CabalSpecVersion -> Flag -> PrettyField ()
ppFlag v flag@(MkFlag name _ _ _)  = PrettySection () "flag" [ppFlagName name] $
    prettyFieldGrammar v (flagFieldGrammar name) flag

ppCondTree2 :: CabalSpecVersion -> PrettyFieldGrammar' s -> CondTree ConfVar [Dependency] s -> [PrettyField ()]
ppCondTree2 v grammar = go
  where
    -- TODO: recognise elif opportunities
    go (CondNode it _ ifs) =
        prettyFieldGrammar v grammar it ++
        concatMap ppIf ifs

    ppIf (CondBranch c thenTree Nothing)
--        | isEmpty thenDoc = mempty
        | otherwise       = [ppIfCondition c thenDoc]
      where
        thenDoc = go thenTree

    ppIf (CondBranch c thenTree (Just elseTree)) =
      -- See #6193
      [ ppIfCondition c (go thenTree)
      , PrettySection () "else" [] (go elseTree)
      ]

ppCommonStanzas :: CabalSpecVersion -> [(UnqualComponentName, CondTree ConfVar [Dependency] CommonStanza)] -> [PrettyField ()]
ppCommonStanzas v commonStanzas =
    [ PrettySection () "common" [pretty n]
    $ ppCondTree2 v (commonStanzaFieldGrammar n) condTree
    | (n, condTree) <- commonStanzas
    ]

ppCondLibrary :: CabalSpecVersion -> Maybe (CondTree ConfVar [Dependency] Library) -> [PrettyField ()]
ppCondLibrary _ Nothing = mempty
ppCondLibrary v (Just condTree) = pure $ PrettySection () "library" [] $
    ppCondTree2 v (libraryFieldGrammar LMainLibName) condTree

ppCondSubLibraries :: CabalSpecVersion -> [(UnqualComponentName, CondTree ConfVar [Dependency] Library)] -> [PrettyField ()]
ppCondSubLibraries v libs =
    [ PrettySection () "library" [pretty n]
    $ ppCondTree2 v (libraryFieldGrammar $ LSubLibName n) condTree
    | (n, condTree) <- libs
    ]

ppCondForeignLibs :: CabalSpecVersion -> [(UnqualComponentName, CondTree ConfVar [Dependency] ForeignLib)] -> [PrettyField ()]
ppCondForeignLibs v flibs =
    [ PrettySection () "foreign-library" [pretty n]
    $ ppCondTree2 v (foreignLibFieldGrammar n) condTree
    | (n, condTree) <- flibs
    ]

ppCondExecutables :: CabalSpecVersion -> [(UnqualComponentName, CondTree ConfVar [Dependency] Executable)] -> [PrettyField ()]
ppCondExecutables v exes =
    [ PrettySection () "executable" [pretty n]
    $ ppCondTree2 v (executableFieldGrammar n) condTree
    | (n, condTree) <- exes
    ]

ppCondTestSuites :: CabalSpecVersion -> [(UnqualComponentName, CondTree ConfVar [Dependency] TestSuite)] -> [PrettyField ()]
ppCondTestSuites v suites =
    [ PrettySection () "test-suite" [pretty n]
    $ ppCondTree2 v testSuiteFieldGrammar (fmap FG.unvalidateTestSuite condTree)
    | (n, condTree) <- suites
    ]

ppCondBenchmarks :: CabalSpecVersion -> [(UnqualComponentName, CondTree ConfVar [Dependency] Benchmark)] -> [PrettyField ()]
ppCondBenchmarks v suites =
    [ PrettySection () "benchmark" [pretty n]
    $ ppCondTree2 v benchmarkFieldGrammar (fmap FG.unvalidateBenchmark condTree)
    | (n, condTree) <- suites
    ]

ppCondition :: Condition ConfVar -> Doc
ppCondition (Var x)                      = ppConfVar x
ppCondition (Lit b)                      = text (show b)
ppCondition (CNot c)                     = char '!' <<>> (ppCondition c)
ppCondition (COr c1 c2)                  = parens (hsep [ppCondition c1, text "||"
                                                         <+> ppCondition c2])
ppCondition (CAnd c1 c2)                 = parens (hsep [ppCondition c1, text "&&"
                                                         <+> ppCondition c2])
ppConfVar :: ConfVar -> Doc
ppConfVar (OS os)                        = text "os"   <<>> parens (pretty os)
ppConfVar (Arch arch)                    = text "arch" <<>> parens (pretty arch)
ppConfVar (Flag name)                    = text "flag" <<>> parens (ppFlagName name)
ppConfVar (Impl c v)                     = text "impl" <<>> parens (pretty c <+> pretty v)

ppFlagName :: FlagName -> Doc
ppFlagName                               = text . unFlagName

ppIfCondition :: Condition ConfVar -> [PrettyField ()] -> PrettyField ()
ppIfCondition c = PrettySection () "if" [ppCondition c]


-- | @since 2.0.0.2
writeHookedBuildInfo :: FilePath -> HookedBuildInfo -> IO ()
writeHookedBuildInfo fpath = writeFileAtomic fpath . BS.Char8.pack
                             . showHookedBuildInfo

-- | @since 2.0.0.2
showHookedBuildInfo :: HookedBuildInfo -> String
showHookedBuildInfo (mb_lib_bi, ex_bis) = showFields (const []) $
    maybe mempty (prettyFieldGrammar cabalSpecLatest buildInfoFieldGrammar) mb_lib_bi ++
    [ PrettySection () "executable:" [pretty name]
    $ prettyFieldGrammar cabalSpecLatest buildInfoFieldGrammar bi
    | (name, bi) <- ex_bis
    ]
