-----------------------------------------------------------------------------
--
-- Module      :  Distribution.PackageDescription.PrettyPrint
-- Copyright   :  Jürgen Nicklisch-Franken 2010
-- License     :  BSD3
--
-- Maintainer  : cabal-devel@haskell.org
-- Stability   : provisional
-- Portability : portable
--
-- | Pretty printing for cabal files
--
-----------------------------------------------------------------------------

module Distribution.PackageDescription.PrettyPrint (
    writeGenericPackageDescription,
    showGenericPackageDescription,
) where

import Data.Monoid (Monoid(mempty))
import Distribution.PackageDescription
       ( Benchmark(..), BenchmarkInterface(..), benchmarkType
       , TestSuite(..), TestSuiteInterface(..), testType
       , SourceRepo(..),
        customFieldsBI, CondTree(..), Condition(..),
        FlagName(..), ConfVar(..), Executable(..), Library(..),
        Flag(..), PackageDescription(..),
        GenericPackageDescription(..))
import Text.PrettyPrint
       (hsep, comma, punctuate, fsep, parens, char, nest, empty,
        isEmpty, ($$), (<+>), colon, (<>), text, vcat, ($+$), Doc, render)
import Distribution.Simple.Utils (writeUTF8File)
import Distribution.ParseUtils (showFreeText, FieldDescr(..))
import Distribution.PackageDescription.Parse (pkgDescrFieldDescrs,binfoFieldDescrs,libFieldDescrs,
       sourceRepoFieldDescrs)
import Distribution.Package (Dependency(..))
import Distribution.Text (Text(..))
import Data.Maybe (isJust, fromJust, isNothing)

indentWith :: Int
indentWith = 4

-- | Recompile with false for regression testing
simplifiedPrinting :: Bool
simplifiedPrinting = False

-- | Writes a .cabal file from a generic package description
writeGenericPackageDescription :: FilePath -> GenericPackageDescription -> IO ()
writeGenericPackageDescription fpath pkg = writeUTF8File fpath (showGenericPackageDescription pkg)

-- | Writes a generic package description to a string
showGenericPackageDescription :: GenericPackageDescription -> String
showGenericPackageDescription            = render . ppGenericPackageDescription

ppGenericPackageDescription :: GenericPackageDescription -> Doc
ppGenericPackageDescription gpd          =
        ppPackageDescription (packageDescription gpd)
        $+$ ppGenPackageFlags (genPackageFlags gpd)
        $+$ ppLibrary (condLibrary gpd)
        $+$ ppExecutables (condExecutables gpd)
        $+$ ppTestSuites (condTestSuites gpd)
        $+$ ppBenchmarks (condBenchmarks gpd)

ppPackageDescription :: PackageDescription -> Doc
ppPackageDescription pd                  =      ppFields pkgDescrFieldDescrs pd
                                                $+$ ppCustomFields (customFieldsPD pd)
                                                $+$ ppSourceRepos (sourceRepos pd)

ppSourceRepos :: [SourceRepo] -> Doc
ppSourceRepos []                         = empty
ppSourceRepos (hd:tl)                    = ppSourceRepo hd $+$ ppSourceRepos tl

ppSourceRepo :: SourceRepo -> Doc
ppSourceRepo repo                        =
    emptyLine $ text "source-repository" <+> disp (repoKind repo) $+$
        (nest indentWith (ppFields sourceRepoFieldDescrs' repo))
  where
    sourceRepoFieldDescrs' = [fd | fd <- sourceRepoFieldDescrs, fieldName fd /= "kind"]

ppFields :: [FieldDescr a] -> a -> Doc
ppFields fields x                        =
    vcat [ ppField name (getter x)
                         | FieldDescr name getter _ <- fields]

ppField :: String -> Doc -> Doc
ppField name fielddoc | isEmpty fielddoc = empty
                      | otherwise        = text name <> colon <+> fielddoc

ppDiffFields :: [FieldDescr a] -> a -> a -> Doc
ppDiffFields fields x y                  =
    vcat [ ppField name (getter x)
                         | FieldDescr name getter _ <- fields,
                            render (getter x) /= render (getter y)]

ppCustomFields :: [(String,String)] -> Doc
ppCustomFields flds                      = vcat [ppCustomField f | f <- flds]

ppCustomField :: (String,String) -> Doc
ppCustomField (name,val)                 = text name <> colon <+> showFreeText val

ppGenPackageFlags :: [Flag] -> Doc
ppGenPackageFlags flds                   = vcat [ppFlag f | f <- flds]

ppFlag :: Flag -> Doc
ppFlag (MkFlag name desc dflt manual)    =
    emptyLine $ text "flag" <+> ppFlagName name $+$
            (nest indentWith ((if null desc
                                then empty
                                else  text "Description: " <+> showFreeText desc) $+$
                     (if dflt then empty else text "Default: False") $+$
                     (if manual then text "Manual: True" else empty)))

ppLibrary :: (Maybe (CondTree ConfVar [Dependency] Library)) -> Doc
ppLibrary Nothing                        = empty
ppLibrary (Just condTree)                =
    emptyLine $ text "library" $+$ nest indentWith (ppCondTree condTree Nothing ppLib)
  where
    ppLib lib Nothing     = ppFields libFieldDescrs lib
                            $$  ppCustomFields (customFieldsBI (libBuildInfo lib))
    ppLib lib (Just plib) = ppDiffFields libFieldDescrs lib plib
                            $$  ppCustomFields (customFieldsBI (libBuildInfo lib))

ppExecutables :: [(String, CondTree ConfVar [Dependency] Executable)] -> Doc
ppExecutables exes                       =
    vcat [emptyLine $ text ("executable " ++ n)
              $+$ nest indentWith (ppCondTree condTree Nothing ppExe)| (n,condTree) <- exes]
  where
    ppExe (Executable _ modulePath' buildInfo') Nothing =
        (if modulePath' == "" then empty else text "main-is:" <+> text modulePath')
            $+$ ppFields binfoFieldDescrs buildInfo'
            $+$  ppCustomFields (customFieldsBI buildInfo')
    ppExe (Executable _ modulePath' buildInfo')
            (Just (Executable _ modulePath2 buildInfo2)) =
            (if modulePath' == "" || modulePath' == modulePath2
                then empty else text "main-is:" <+> text modulePath')
            $+$ ppDiffFields binfoFieldDescrs buildInfo' buildInfo2
            $+$ ppCustomFields (customFieldsBI buildInfo')

ppTestSuites :: [(String, CondTree ConfVar [Dependency] TestSuite)] -> Doc
ppTestSuites suites =
    emptyLine $ vcat [     text ("test-suite " ++ n)
                       $+$ nest indentWith (ppCondTree condTree Nothing ppTestSuite)
                     | (n,condTree) <- suites]
  where
    ppTestSuite testsuite Nothing =
                maybe empty (\t -> text "type:"        <+> disp t)
                            maybeTestType
            $+$ maybe empty (\f -> text "main-is:"     <+> text f)
                            (testSuiteMainIs testsuite)
            $+$ maybe empty (\m -> text "test-module:" <+> disp m)
                            (testSuiteModule testsuite)
            $+$ ppFields binfoFieldDescrs (testBuildInfo testsuite)
            $+$ ppCustomFields (customFieldsBI (testBuildInfo testsuite))
      where
        maybeTestType | testInterface testsuite == mempty = Nothing
                      | otherwise = Just (testType testsuite)

    ppTestSuite (TestSuite _ _ buildInfo' _)
                    (Just (TestSuite _ _ buildInfo2 _)) =
            ppDiffFields binfoFieldDescrs buildInfo' buildInfo2
            $+$ ppCustomFields (customFieldsBI buildInfo')

    testSuiteMainIs test = case testInterface test of
      TestSuiteExeV10 _ f -> Just f
      _                   -> Nothing

    testSuiteModule test = case testInterface test of
      TestSuiteLibV09 _ m -> Just m
      _                   -> Nothing

ppBenchmarks :: [(String, CondTree ConfVar [Dependency] Benchmark)] -> Doc
ppBenchmarks suites =
    emptyLine $ vcat [     text ("benchmark " ++ n)
                       $+$ nest indentWith (ppCondTree condTree Nothing ppBenchmark)
                     | (n,condTree) <- suites]
  where
    ppBenchmark benchmark Nothing =
                maybe empty (\t -> text "type:"        <+> disp t)
                            maybeBenchmarkType
            $+$ maybe empty (\f -> text "main-is:"     <+> text f)
                            (benchmarkMainIs benchmark)
            $+$ ppFields binfoFieldDescrs (benchmarkBuildInfo benchmark)
            $+$ ppCustomFields (customFieldsBI (benchmarkBuildInfo benchmark))
      where
        maybeBenchmarkType | benchmarkInterface benchmark == mempty = Nothing
                           | otherwise = Just (benchmarkType benchmark)

    ppBenchmark (Benchmark _ _ buildInfo' _)
                    (Just (Benchmark _ _ buildInfo2 _)) =
            ppDiffFields binfoFieldDescrs buildInfo' buildInfo2
            $+$ ppCustomFields (customFieldsBI buildInfo')

    benchmarkMainIs benchmark = case benchmarkInterface benchmark of
      BenchmarkExeV10 _ f -> Just f
      _                   -> Nothing

ppCondition :: Condition ConfVar -> Doc
ppCondition (Var x)                      = ppConfVar x
ppCondition (Lit b)                      = text (show b)
ppCondition (CNot c)                     = char '!' <> (ppCondition c)
ppCondition (COr c1 c2)                  = parens (hsep [ppCondition c1, text "||"
                                                         <+> ppCondition c2])
ppCondition (CAnd c1 c2)                 = parens (hsep [ppCondition c1, text "&&"
                                                         <+> ppCondition c2])
ppConfVar :: ConfVar -> Doc
ppConfVar (OS os)                        = text "os"   <> parens (disp os)
ppConfVar (Arch arch)                    = text "arch" <> parens (disp arch)
ppConfVar (Flag name)                    = text "flag" <> parens (ppFlagName name)
ppConfVar (Impl c v)                     = text "impl" <> parens (disp c <+> disp v)

ppFlagName :: FlagName -> Doc
ppFlagName (FlagName name)               = text name

ppCondTree :: CondTree ConfVar [Dependency] a -> Maybe a -> (a -> Maybe a -> Doc) ->  Doc
ppCondTree ct@(CondNode it deps ifs) mbIt ppIt =
    let res = ppDeps deps
                $+$ (vcat $ map ppIf ifs)
                $+$ ppIt it mbIt
    in if isJust mbIt && isEmpty res
        then ppCondTree ct Nothing ppIt
        else res
  where
    ppIf (c,thenTree,mElseTree)          =
        ((emptyLine $ text "if" <+> ppCondition c) $$
          nest indentWith (ppCondTree thenTree
                    (if simplifiedPrinting then (Just it) else Nothing) ppIt))
        $+$ (if isNothing mElseTree
                then empty
                else text "else"
                    $$ nest indentWith (ppCondTree (fromJust mElseTree)
                        (if simplifiedPrinting then (Just it) else Nothing) ppIt))

ppDeps :: [Dependency] -> Doc
ppDeps []                                = empty
ppDeps deps                              =
    text "build-depends:" <+> fsep (punctuate comma (map disp deps))

emptyLine :: Doc -> Doc
emptyLine d                              = text " " $+$ d



