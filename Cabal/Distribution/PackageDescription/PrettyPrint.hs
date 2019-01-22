{-# LANGUAGE OverloadedStrings #-}
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
--
-----------------------------------------------------------------------------

module Distribution.PackageDescription.PrettyPrint (
    -- * Generic package descriptions
    writeGenericPackageDescription,
    showGenericPackageDescription,
    ppGenericPackageDescription,

    -- * Package descriptions
     writePackageDescription,
     showPackageDescription,

     -- ** Supplementary build information
     writeHookedBuildInfo,
     showHookedBuildInfo,
) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Types.CondTree
import Distribution.Types.Dependency
import Distribution.Types.ForeignLib          (ForeignLib (foreignLibName))
import Distribution.Types.LibraryName
import Distribution.Types.UnqualComponentName

import Distribution.PackageDescription
import Distribution.Pretty
import Distribution.Fields.Pretty
import Distribution.Simple.Utils

import Distribution.FieldGrammar                    (PrettyFieldGrammar', prettyFieldGrammar)
import Distribution.PackageDescription.FieldGrammar
       (benchmarkFieldGrammar, buildInfoFieldGrammar, executableFieldGrammar, flagFieldGrammar,
       foreignLibFieldGrammar, libraryFieldGrammar, packageDescriptionFieldGrammar,
       setupBInfoFieldGrammar, sourceRepoFieldGrammar, testSuiteFieldGrammar)

import qualified Distribution.PackageDescription.FieldGrammar as FG

import Text.PrettyPrint (Doc, char, hsep, parens, text, (<+>))

import qualified Data.ByteString.Lazy.Char8 as BS.Char8

-- | Writes a .cabal file from a generic package description
writeGenericPackageDescription :: FilePath -> GenericPackageDescription -> NoCallStackIO ()
writeGenericPackageDescription fpath pkg = writeUTF8File fpath (showGenericPackageDescription pkg)

-- | Writes a generic package description to a string
showGenericPackageDescription :: GenericPackageDescription -> String
showGenericPackageDescription = showFields . ppGenericPackageDescription

-- | Convert a generic package description to 'PrettyField's.
ppGenericPackageDescription :: GenericPackageDescription -> [PrettyField]
ppGenericPackageDescription gpd = concat
    [ ppPackageDescription (packageDescription gpd)
    , ppSetupBInfo (setupBuildInfo (packageDescription gpd))
    , ppGenPackageFlags (genPackageFlags gpd)
    , ppCondLibrary (condLibrary gpd)
    , ppCondSubLibraries (condSubLibraries gpd)
    , ppCondForeignLibs (condForeignLibs gpd)
    , ppCondExecutables (condExecutables gpd)
    , ppCondTestSuites (condTestSuites gpd)
    , ppCondBenchmarks (condBenchmarks gpd)
    ]

ppPackageDescription :: PackageDescription -> [PrettyField]
ppPackageDescription pd =
    prettyFieldGrammar packageDescriptionFieldGrammar pd
    ++ ppSourceRepos (sourceRepos pd)

ppSourceRepos :: [SourceRepo] -> [PrettyField]
ppSourceRepos = map ppSourceRepo

ppSourceRepo :: SourceRepo -> PrettyField
ppSourceRepo repo = PrettySection "source-repository" [pretty kind] $
    prettyFieldGrammar (sourceRepoFieldGrammar kind) repo
  where
    kind = repoKind repo

ppSetupBInfo :: Maybe SetupBuildInfo -> [PrettyField]
ppSetupBInfo Nothing = mempty
ppSetupBInfo (Just sbi)
    | defaultSetupDepends sbi = mempty
    | otherwise = pure $ PrettySection "custom-setup" [] $
        prettyFieldGrammar (setupBInfoFieldGrammar False) sbi

ppGenPackageFlags :: [Flag] -> [PrettyField]
ppGenPackageFlags = map ppFlag

ppFlag :: Flag -> PrettyField
ppFlag flag@(MkFlag name _ _ _)  = PrettySection "flag" [ppFlagName name] $
    prettyFieldGrammar (flagFieldGrammar name) flag

ppCondTree2 :: PrettyFieldGrammar' s -> CondTree ConfVar [Dependency] s -> [PrettyField]
ppCondTree2 grammar = go
  where
    -- TODO: recognise elif opportunities
    go (CondNode it _ ifs) =
        prettyFieldGrammar grammar it ++
        concatMap ppIf ifs

    ppIf (CondBranch c thenTree Nothing)
--        | isEmpty thenDoc = mempty
        | otherwise       = [ppIfCondition c thenDoc]
      where
        thenDoc = go thenTree

    ppIf (CondBranch c thenTree (Just elseTree)) =
          case (False, False) of
 --       case (isEmpty thenDoc, isEmpty elseDoc) of
              (True,  True)  -> mempty
              (False, True)  -> [ ppIfCondition c thenDoc ]
              (True,  False) -> [ ppIfCondition (cNot c) elseDoc ]
              (False, False) -> [ ppIfCondition c thenDoc
                                , PrettySection "else" [] elseDoc
                                ]
      where
        thenDoc = go thenTree
        elseDoc = go elseTree

ppCondLibrary :: Maybe (CondTree ConfVar [Dependency] Library) -> [PrettyField]
ppCondLibrary Nothing = mempty
ppCondLibrary (Just condTree) = pure $ PrettySection "library" [] $
    ppCondTree2 (libraryFieldGrammar LMainLibName) condTree

ppCondSubLibraries :: [(UnqualComponentName, CondTree ConfVar [Dependency] Library)] -> [PrettyField]
ppCondSubLibraries libs =
    [ PrettySection "library" [pretty n]
    $ ppCondTree2 (libraryFieldGrammar $ LSubLibName n) condTree
    | (n, condTree) <- libs
    ]

ppCondForeignLibs :: [(UnqualComponentName, CondTree ConfVar [Dependency] ForeignLib)] -> [PrettyField]
ppCondForeignLibs flibs =
    [ PrettySection "foreign-library" [pretty n]
    $ ppCondTree2 (foreignLibFieldGrammar n) condTree
    | (n, condTree) <- flibs
    ]

ppCondExecutables :: [(UnqualComponentName, CondTree ConfVar [Dependency] Executable)] -> [PrettyField]
ppCondExecutables exes =
    [ PrettySection "executable" [pretty n]
    $ ppCondTree2 (executableFieldGrammar n) condTree
    | (n, condTree) <- exes
    ]

ppCondTestSuites :: [(UnqualComponentName, CondTree ConfVar [Dependency] TestSuite)] -> [PrettyField]
ppCondTestSuites suites =
    [ PrettySection "test-suite" [pretty n]
    $ ppCondTree2 testSuiteFieldGrammar (fmap FG.unvalidateTestSuite condTree)
    | (n, condTree) <- suites
    ]

ppCondBenchmarks :: [(UnqualComponentName, CondTree ConfVar [Dependency] Benchmark)] -> [PrettyField]
ppCondBenchmarks suites =
    [ PrettySection "benchmark" [pretty n]
    $ ppCondTree2 benchmarkFieldGrammar (fmap FG.unvalidateBenchmark condTree)
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

ppIfCondition :: (Condition ConfVar) -> [PrettyField] -> PrettyField
ppIfCondition c = PrettySection "if" [ppCondition c]

-- | @since 2.0.0.2
writePackageDescription :: FilePath -> PackageDescription -> NoCallStackIO ()
writePackageDescription fpath pkg = writeUTF8File fpath (showPackageDescription pkg)

--TODO: make this use section syntax
-- add equivalent for GenericPackageDescription

-- | @since 2.0.0.2
showPackageDescription :: PackageDescription -> String
showPackageDescription = showGenericPackageDescription . pdToGpd

pdToGpd :: PackageDescription -> GenericPackageDescription
pdToGpd pd = GenericPackageDescription
    { packageDescription = pd
    , genPackageFlags    = []
    , condLibrary        = mkCondTree <$> library pd
    , condSubLibraries   = mkCondTreeL <$> subLibraries pd
    , condForeignLibs    = mkCondTree' foreignLibName <$> foreignLibs pd
    , condExecutables    = mkCondTree' exeName <$> executables pd
    , condTestSuites     = mkCondTree' testName <$> testSuites pd
    , condBenchmarks     = mkCondTree' benchmarkName <$> benchmarks pd
    }
  where
    -- We set CondTree's [Dependency] to an empty list, as it
    -- is not pretty printed anyway.
    mkCondTree  x = CondNode x [] []
    mkCondTreeL l = (fromMaybe (mkUnqualComponentName "") (libraryNameString (libName l)), CondNode l [] [])

    mkCondTree'
        :: (a -> UnqualComponentName)
        -> a -> (UnqualComponentName, CondTree ConfVar [Dependency] a)
    mkCondTree' f x = (f x, CondNode x [] [])

-- | @since 2.0.0.2
writeHookedBuildInfo :: FilePath -> HookedBuildInfo -> NoCallStackIO ()
writeHookedBuildInfo fpath = writeFileAtomic fpath . BS.Char8.pack
                             . showHookedBuildInfo

-- | @since 2.0.0.2
showHookedBuildInfo :: HookedBuildInfo -> String
showHookedBuildInfo (mb_lib_bi, ex_bis) = showFields $
    maybe mempty (prettyFieldGrammar buildInfoFieldGrammar) mb_lib_bi ++
    [ PrettySection "executable:" [pretty name]
    $ prettyFieldGrammar buildInfoFieldGrammar bi
    | (name, bi) <- ex_bis
    ]
