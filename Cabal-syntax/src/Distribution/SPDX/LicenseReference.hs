{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Distribution.SPDX.LicenseReference
  ( LicenseRef
  , licenseRef
  , licenseDocumentRef
  , mkLicenseRef
  , mkLicenseRef'
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Parsec
import Distribution.Pretty
import Distribution.Utils.Generic (isAsciiAlphaNum)

import qualified Distribution.Compat.CharParsing as P
import qualified Text.PrettyPrint as Disp

-- | A user defined license reference denoted by @LicenseRef-[idstring]@ (for a license not on the SPDX License List);
data LicenseRef = LicenseRef
  { _lrDocument :: !(Maybe String)
  , _lrLicense :: !String
  }
  deriving (Show, Read, Eq, Ord, Typeable, Data, Generic)

-- | License reference.
licenseRef :: LicenseRef -> String
licenseRef = _lrLicense

-- | Document reference.
licenseDocumentRef :: LicenseRef -> Maybe String
licenseDocumentRef = _lrDocument

instance Binary LicenseRef
instance Structured LicenseRef

instance NFData LicenseRef where
  rnf (LicenseRef d l) = rnf d `seq` rnf l

instance Pretty LicenseRef where
  pretty (LicenseRef Nothing l) = Disp.text "LicenseRef-" <<>> Disp.text l
  pretty (LicenseRef (Just d) l) =
    Disp.text "DocumentRef-" <<>> Disp.text d <<>> Disp.char ':' <<>> Disp.text "LicenseRef-" <<>> Disp.text l

instance Parsec LicenseRef where
  parsec = name <|> doc
    where
      name = do
        _ <- P.string "LicenseRef-"
        n <- some $ P.satisfy $ \c -> isAsciiAlphaNum c || c == '-' || c == '.'
        pure (LicenseRef Nothing n)

      doc = do
        _ <- P.string "DocumentRef-"
        d <- some $ P.satisfy $ \c -> isAsciiAlphaNum c || c == '-' || c == '.'
        _ <- P.char ':'
        _ <- P.string "LicenseRef-"
        n <- some $ P.satisfy $ \c -> isAsciiAlphaNum c || c == '-' || c == '.'
        pure (LicenseRef (Just d) n)

-- | Create 'LicenseRef' from optional document ref and name.
mkLicenseRef :: Maybe String -> String -> Maybe LicenseRef
mkLicenseRef d l = do
  d' <- traverse checkIdString d
  l' <- checkIdString l
  pure (LicenseRef d' l')
  where
    checkIdString s
      | all (\c -> isAsciiAlphaNum c || c == '-' || c == '.') s = Just s
      | otherwise = Nothing

-- | Like 'mkLicenseRef' but convert invalid characters into @-@.
mkLicenseRef' :: Maybe String -> String -> LicenseRef
mkLicenseRef' d l = LicenseRef (fmap f d) (f l)
  where
    f = map g
    g c
      | isAsciiAlphaNum c || c == '-' || c == '.' = c
      | otherwise = '-'
