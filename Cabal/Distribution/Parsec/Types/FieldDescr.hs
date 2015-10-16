{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Parsec.Types.FieldDescr
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
module Distribution.Parsec.Types.FieldDescr (
    -- * Field name
    FieldName,
    -- * Field description
    FieldDescr (..),
    liftField,
    simpleField,
    boolField,
    optsField,
    listField,
    listFieldWithSep,
    commaListField,
    commaListFieldWithSep,
    spaceListField,
    -- ** Pretty printing
    ppFields,
    ppField,
    -- * Unknown fields
    UnknownFieldParser,
    warnUnrec,
    ignoreUnrec,
    ) where

import           Distribution.Compat.Prelude      hiding (get)
import           Prelude ()

import qualified Data.ByteString                  as BS
import           Data.Ord                         (comparing)
import           Text.PrettyPrint
                 (Doc, colon, comma, fsep, hsep, isEmpty, nest, punctuate,
                 text, vcat, ($+$), (<+>))

import qualified Distribution.Compat.Parsec       as P
import           Distribution.Compiler            (CompilerFlavor)
import           Distribution.Parsec.Class
import           Distribution.Parsec.Types.Common
import           Distribution.PrettyUtils

type FieldName = BS.ByteString

-------------------------------------------------------------------------------
-- Unrecoginsed fields
-------------------------------------------------------------------------------

-- | How to handle unknown fields.
type UnknownFieldParser a = FieldName -> String -> a -> Maybe a

-- | A default unrecognized field parser which simply returns Nothing,
--   i.e. ignores all unrecognized fields, so warnings will be generated.
warnUnrec :: UnknownFieldParser a
warnUnrec _ _ _ = Nothing

-- | A default unrecognized field parser which silently (i.e. no
--   warnings will be generated) ignores unrecognized fields, by
--   returning the structure being built unmodified.
ignoreUnrec :: UnknownFieldParser a
ignoreUnrec _ _ = Just

-------------------------------------------------------------------------------
-- Field description
-------------------------------------------------------------------------------

data FieldDescr a = FieldDescr
    { fieldName   :: FieldName
    , fieldPretty :: a -> Doc
    , fieldParser :: a -> FieldParser a
    }

liftField :: (b -> a) -> (a -> b -> b) -> FieldDescr a -> FieldDescr b
liftField get set fd = FieldDescr
    (fieldName fd)
    (fieldPretty fd . get)
    (\b -> flip set b <$> fieldParser fd (get b))

simpleField
    :: FieldName       -- ^ fieldname
    -> (a -> Doc)      -- ^ show
    -> FieldParser a   -- ^ parser
    -> (b -> a)        -- ^ getter
    -> (a -> b -> b)   -- ^ setter
    -> FieldDescr b
simpleField name pretty parse get set = FieldDescr
    name
    (pretty . get)
    (\a -> flip set a <$> parse)

boolField
    :: FieldName
    -> (b -> Bool)
    -> (Bool -> b -> b)
    -> FieldDescr b
boolField name get set = liftField get set (FieldDescr name showF parseF)
  where
    showF = text . show
    parseF _ = P.munch1 isAlpha >>= postprocess
    postprocess str
        |  str == "True"  = pure True
        |  str == "False" = pure False
        | lstr == "true"  = parsecWarning PWTBoolCase caseWarning *> pure True
        | lstr == "false" = parsecWarning PWTBoolCase caseWarning *> pure False
        | otherwise       = fail $ "Not a boolena: " ++ str
      where
        lstr = map toLower str
        caseWarning =
            "The " ++ show name ++ " field is case sensitive, use 'True' or 'False'."

optsField
    :: FieldName
    -> CompilerFlavor
    -> (b -> [(CompilerFlavor,[String])])
    -> ([(CompilerFlavor,[String])] -> b -> b)
    -> FieldDescr b
optsField name flavor get set = liftField
    (fromMaybe [] . lookup flavor . get)
    (\opts b -> set (reorder (update flavor opts (get b))) b)
    $ field name showF (many $ parsecToken' <* P.munch isSpace)
  where
    update _ opts l | all null opts = l  --empty opts as if no opts
    update f opts [] = [(f,opts)]
    update f opts ((f',opts'):rest)
        | f == f'   = (f, opts' ++ opts) : rest
        | otherwise = (f',opts') : update f opts rest
    reorder = sortBy (comparing fst)
    showF   = hsep . map text

listField
    :: FieldName
    -> (a -> Doc)
    -> FieldParser a
    -> (b -> [a])
    -> ([a] -> b -> b)
    -> FieldDescr b
listField = listFieldWithSep fsep

listFieldWithSep
    :: Separator
    -> FieldName
    -> (a -> Doc)
    -> FieldParser a
    -> (b -> [a])
    -> ([a] -> b -> b)
    -> FieldDescr b
listFieldWithSep separator name showF parseF get set =
    liftField get set' $
        field name showF' (parsecOptCommaList parseF)
  where
    set' xs b = set (get b ++ xs) b
    showF'    = separator . map showF


commaListField
    :: FieldName -> (a -> Doc) -> FieldParser a
    -> (b -> [a]) -> ([a] -> b -> b) -> FieldDescr b
commaListField = commaListFieldWithSep fsep

commaListFieldWithSep
    :: Separator -> FieldName  -> (a -> Doc) -> FieldParser a
    -> (b -> [a]) -> ([a] -> b -> b) -> FieldDescr b
commaListFieldWithSep separator name showF parseF get set =
    liftField get set' $
        field name showF' (parsecCommaList parseF)
   where
     set' xs b = set (get b ++ xs) b
     showF'    = separator . punctuate comma . map showF

spaceListField
    :: FieldName -> (a -> Doc) -> FieldParser a
    -> (b -> [a]) -> ([a] -> b -> b) -> FieldDescr b
spaceListField name showF parseF get set = liftField get set' $
    field name showF' (many $ parseF <* P.spaces)
  where
    set' xs b = set (get b ++ xs) b
    showF'    = fsep . map showF

-- Overriding field
field :: FieldName -> (a -> Doc) -> FieldParser a -> FieldDescr a
field name showF parseF =
    FieldDescr name showF (const parseF)

-------------------------------------------------------------------------------
-- Pretty printing
-------------------------------------------------------------------------------

ppFields :: [FieldDescr a] -> a -> Doc
ppFields fields x =
   vcat [ ppField name (getter x) | FieldDescr name getter _ <- fields ]

ppField :: FieldName -> Doc -> Doc
ppField name fielddoc
   | isEmpty fielddoc         = mempty
   | name `elem` nestedFields = text namestr <<>> colon $+$ nest indentWith fielddoc
   | otherwise                = text namestr <<>> colon <+> fielddoc
   where
      namestr = show name -- TODO: do this
      nestedFields :: [BS.ByteString]
      nestedFields =
         [ "description"
         , "build-depends"
         , "data-files"
         , "extra-source-files"
         , "extra-tmp-files"
         , "exposed-modules"
         , "c-sources"
         , "js-sources"
         , "extra-libraries"
         , "includes"
         , "install-includes"
         , "other-modules"
         , "depends"
         ]

-- | Handle deprecated fields
--
-- *TODO:* use Parsec
{-
deprecField :: Field Position -> Parsec (Field Position)
deprecField = undefined  {-
-}

 (Field (Name pos fld) val) = do
  fld' <- case lookup fld deprecatedFields of
            Nothing -> return fld
            Just newName -> do
              warning $ "The field " ++ show fld
                      ++ " is deprecated, please use " ++ show newName
              return newName
  return (Field (Name pos fld') val)
deprecField _ = cabalBug "'deprecField' called on a non-field"
-}
