{-# LANGUAGE ExistentialQuantification, NamedFieldPuns #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.ParseUtils
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Parsing utilities.
-----------------------------------------------------------------------------

module Distribution.Client.ParseUtils (
    parseFields,
    ppFields,
    ppSection,

    SectionDescr(..),
    showConfig,
    parseConfig,
    liftFields,
    liftSection,
    filterFields,
    mapFieldNames,
    commandOptionsToFields,
  )
       where

import Distribution.ParseUtils
         ( FieldDescr(..), ParseResult(..), warning, LineNo, lineNo
         , Field(..), liftField, readFieldsFlat )
import qualified Distribution.ParseUtils as ParseUtils
         ( Field(..) )
import Distribution.Simple.Command
         ( OptionField, viewAsFieldDescr )

import Data.Monoid (Monoid(..))
import Control.Monad    ( foldM )
import Text.PrettyPrint ( (<>), (<+>), ($+$) )
import qualified Data.Map as Map
import qualified Text.PrettyPrint as Disp
         ( Doc, text, colon, vcat, empty, isEmpty, nest )

parseFields :: [FieldDescr a] -> a -> [ParseUtils.Field] -> ParseResult a
parseFields fields = foldM setField
  where
    fieldMap = Map.fromList
      [ (name, f) | f@(FieldDescr name _ _) <- fields ]
    setField accum (ParseUtils.F line name value) =
      case Map.lookup name fieldMap of
        Just (FieldDescr _ _ set) -> set line value accum
        Nothing -> do
          warning $ "Unrecognized field " ++ name ++ " on line " ++ show line
          return accum
    setField accum f = do
      warning $ "Unrecognized stanza on line " ++ show (lineNo f)
      return accum

-- | This is a customised version of the functions from Distribution.ParseUtils
-- that also optionally print default values for empty fields as comments.
--
ppFields :: [FieldDescr a] -> (Maybe a) -> a -> Disp.Doc
ppFields fields def cur = Disp.vcat [ ppField name (fmap getter def) (getter cur)
                                    | FieldDescr name getter _ <- fields]

ppField :: String -> (Maybe Disp.Doc) -> Disp.Doc -> Disp.Doc
ppField name mdef cur
  | Disp.isEmpty cur = maybe Disp.empty
                       (\def -> Disp.text "--" <+> Disp.text name
                                <> Disp.colon <+> def) mdef
  | otherwise        = Disp.text name <> Disp.colon <+> cur

ppSection :: String -> String -> [FieldDescr a] -> (Maybe a) -> a -> Disp.Doc
ppSection name arg fields def cur
  | Disp.isEmpty fieldsDoc = Disp.empty
  | otherwise              = Disp.text name <+> argDoc
                             $+$ (Disp.nest 2 fieldsDoc)
  where
    fieldsDoc = ppFields fields def cur
    argDoc | arg == "" = Disp.empty
           | otherwise = Disp.text arg

liftFields :: (b -> a)
           -> (a -> b -> b)
           -> [FieldDescr a]
           -> [FieldDescr b]
liftFields get set = map (liftField get set)


filterFields :: [String] -> [FieldDescr a] -> [FieldDescr a]
filterFields includeFields = filter ((`elem` includeFields) . fieldName)

mapFieldNames :: (String -> String) -> [FieldDescr a] -> [FieldDescr a]
mapFieldNames mangleName =
    map (\descr -> descr { fieldName = mangleName (fieldName descr) })

commandOptionsToFields :: [OptionField a] -> [FieldDescr a]
commandOptionsToFields = map viewAsFieldDescr


data SectionDescr a = forall b. Monoid b => SectionDescr {
       sectionName        :: String,
       sectionFields      :: [FieldDescr b],
       sectionSubsections :: [SectionDescr b],
       sectionGet         :: a -> [(String, b)],
       sectionSet         :: LineNo -> String -> b -> a -> ParseResult a
     }

liftSection :: (b -> a)
            -> (a -> b -> b)
            -> SectionDescr a
            -> SectionDescr b
liftSection get' set' (SectionDescr name fields sections get set) =
    let sectionGet' = get . get'
        sectionSet' lineno param x y = do x' <- set lineno param x (get' y)
                                          return (set' x' y)

     in SectionDescr name fields sections sectionGet' sectionSet'

parseConfig :: Monoid a => [FieldDescr a] -> [SectionDescr a]
            -> String -> ParseResult a
parseConfig fields sections str =
    accumFieldsAndSections fields sections =<< readFieldsFlat str


accumFieldsAndSections :: Monoid a => [FieldDescr a] -> [SectionDescr a]
              -> [Field] -> ParseResult a
accumFieldsAndSections fieldDescrs sectionDescrs =
    foldM accumField mempty
  where
    fieldMap   = Map.fromList [ (fieldName   f, f) | f <- fieldDescrs   ]
    sectionMap = Map.fromList [ (sectionName s, s) | s <- sectionDescrs ]

    accumField a (F line name value) =
      case Map.lookup name fieldMap of
        Just (FieldDescr _ _ set) -> set line value a
        Nothing -> do
          warning $ "Unrecognized field '" ++ name
                 ++ "' on line " ++ show line
          return a

    accumField a (Section line name param fields) =
      case Map.lookup name sectionMap of
        Just (SectionDescr _ fieldDescrs' sectionDescrs' _ set) -> do
          b <- accumFieldsAndSections fieldDescrs' sectionDescrs' fields
          set line param b a
        Nothing -> do
          warning $ "Unrecognized section '" ++ name
                 ++ "' on line " ++ show line
          return a

    accumField _ (IfBlock {}) = error "accumFieldsAndSections: impossible"

showConfig :: [FieldDescr a] -> [SectionDescr a] -> a -> Disp.Doc
showConfig fields sections val =
      ppFields fields Nothing val
        $+$
      Disp.vcat
      [ Disp.text "" $+$ sectionDoc
      | SectionDescr {
          sectionName, sectionGet,
          sectionFields, sectionSubsections
        } <- sections
      , (param, x) <- sectionGet val
      , let sectionDoc = ppSection' sectionName param sectionFields sectionSubsections x
      , not (Disp.isEmpty sectionDoc) ]

ppSection' :: String -> String -> [FieldDescr a] -> [SectionDescr a] -> a -> Disp.Doc
ppSection' name arg fields sections cur
  | Disp.isEmpty fieldsDoc = Disp.empty
  | otherwise              = Disp.text name <+> argDoc
                             $+$ (Disp.nest 2 fieldsDoc)
  where
    fieldsDoc = showConfig fields sections cur
    argDoc | arg == "" = Disp.empty
           | otherwise = Disp.text arg

