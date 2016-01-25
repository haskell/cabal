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
    writeGenericPackageDescription,
    showGenericPackageDescription,
) where

import Distribution.PackageDescription
import Distribution.Simple.Utils
import Distribution.ParseUtils
import Distribution.PackageDescription.Parse
import Distribution.Package
import Distribution.Text

import Data.Monoid as Mon (Monoid(mempty))
import Data.Maybe (isJust)
import Text.PrettyPrint
       (hsep, parens, char, nest, empty, isEmpty, ($$), (<+>),
        colon, (<>), text, vcat, ($+$), Doc, render)

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

-- TODO: this is a temporary hack. Ideally, fields containing default values
-- would be filtered out when the @FieldDescr a@ list is generated.
ppFieldsFiltered :: [(String, String)] -> [FieldDescr a] -> a -> Doc
ppFieldsFiltered removable fields x = ppFields (filter nondefault fields) x
  where
    nondefault (FieldDescr name getter _) =
        maybe True (render (getter x) /=) (lookup name removable)

binfoDefaults :: [(String, String)]
binfoDefaults = [("buildable", "True")]

libDefaults :: [(String, String)]
libDefaults = ("exposed", "True") : binfoDefaults

flagDefaults :: [(String, String)]
flagDefaults = [("default", "True"), ("manual", "False")]

ppDiffFields :: [FieldDescr a] -> a -> a -> Doc
ppDiffFields fields x y                  =
   vcat [ ppField name (getter x)
        | FieldDescr name getter _ <- fields
        , render (getter x) /= render (getter y)
        ]

ppCustomFields :: [(String,String)] -> Doc
ppCustomFields flds                      = vcat [ppCustomField f | f <- flds]

ppCustomField :: (String,String) -> Doc
ppCustomField (name,val)                 = text name <> colon <+> showFreeText val

ppGenPackageFlags :: [Flag] -> Doc
ppGenPackageFlags flds                   = vcat [ppFlag f | f <- flds]

ppFlag :: Flag -> Doc
ppFlag flag@(MkFlag name _ _ _)    =
    emptyLine $ text "flag" <+> ppFlagName name $+$ nest indentWith fields
  where
    fields = ppFieldsFiltered flagDefaults flagFieldDescrs flag

ppLibrary :: (Maybe (CondTree ConfVar [Dependency] Library)) -> Doc
ppLibrary Nothing                        = empty
ppLibrary (Just condTree)                =
    emptyLine $ text "library" $+$ nest indentWith (ppCondTree condTree Nothing ppLib)
  where
    ppLib lib Nothing     = ppFieldsFiltered libDefaults libFieldDescrs lib
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
            $+$ ppFieldsFiltered binfoDefaults binfoFieldDescrs buildInfo'
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
            $+$ ppFieldsFiltered binfoDefaults binfoFieldDescrs (testBuildInfo testsuite)
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
            $+$ ppFieldsFiltered binfoDefaults binfoFieldDescrs (benchmarkBuildInfo benchmark)
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
ppCondTree ct@(CondNode it _ ifs) mbIt ppIt =
    let res = (vcat $ map ppIf ifs)
                $+$ ppIt it mbIt
    in if isJust mbIt && isEmpty res
        then ppCondTree ct Nothing ppIt
        else res
  where
    -- TODO: this ends up printing trailing spaces when combined with nest.
    ppIf (c, thenTree, Just elseTree) = ppIfElse it ppIt c thenTree elseTree
    ppIf (c, thenTree, Nothing)       = ppIf' it ppIt c thenTree

ppIfCondition :: (Condition ConfVar) -> Doc
ppIfCondition c = (emptyLine $ text "if" <+> ppCondition c)

ppIf' :: a -> (a -> Maybe a -> Doc)
           -> Condition ConfVar
           -> CondTree ConfVar [Dependency] a
           -> Doc
ppIf' it ppIt c thenTree =
  if isEmpty thenDoc
     then Mon.mempty
     else ppIfCondition c $$ nest indentWith thenDoc
  where thenDoc = ppCondTree thenTree (if simplifiedPrinting then (Just it) else Nothing) ppIt

ppIfElse :: a -> (a -> Maybe a -> Doc)
              -> Condition ConfVar
              -> CondTree ConfVar [Dependency] a
              -> CondTree ConfVar [Dependency] a
              -> Doc
ppIfElse it ppIt c thenTree elseTree =
  case (isEmpty thenDoc, isEmpty elseDoc) of
    (True,  True)  -> Mon.mempty
    (False, True)  -> ppIfCondition c $$ nest indentWith thenDoc
    (True,  False) -> ppIfCondition (cNot c) $$ nest indentWith elseDoc
    (False, False) -> (ppIfCondition c $$ nest indentWith thenDoc)
                      $+$ (text "else" $$ nest indentWith elseDoc)
  where thenDoc = ppCondTree thenTree (if simplifiedPrinting then (Just it) else Nothing) ppIt
        elseDoc = ppCondTree elseTree (if simplifiedPrinting then (Just it) else Nothing) ppIt

emptyLine :: Doc -> Doc
emptyLine d                              = text "" $+$ d

