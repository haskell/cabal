{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Distribution.SPDX.License
  ( License (..)
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Parsec
import Distribution.Pretty
import Distribution.SPDX.LicenseExpression

import qualified Distribution.Compat.CharParsing as P
import qualified Text.PrettyPrint as Disp

-- | Declared license.
-- See [section 3.15 of SPDX Specification 2.1](https://spdx.org/spdx-specification-21-web-version#h.1hmsyys)
--
-- /Note:/ the NOASSERTION case is omitted.
--
-- Old 'License' can be migrated using following rules:
--
-- * @AllRightsReserved@ and @UnspecifiedLicense@ to 'NONE'.
--   No license specified which legally defaults to /All Rights Reserved/.
--   The package may not be legally modified or redistributed by anyone but
--   the rightsholder.
--
-- * @OtherLicense@ can be converted to 'LicenseRef' pointing to the file
--   in the package.
--
-- * @UnknownLicense@ i.e. other licenses of the form @name-x.y@, should be
--   covered by SPDX license list, otherwise use 'LicenseRef'.
--
-- * @PublicDomain@ isn't covered. Consider using CC0.
--   See <https://wiki.spdx.org/view/Legal_Team/Decisions/Dealing_with_Public_Domain_within_SPDX_Files>
--   for more information.
data License
  = -- | if the package contains no license information whatsoever; or
    NONE
  | -- | A valid SPDX License Expression as defined in Appendix IV.
    License LicenseExpression
  deriving (Show, Read, Eq, Ord, Typeable, Data, Generic)

instance Binary License
instance Structured License

instance NFData License where
  rnf NONE = ()
  rnf (License l) = rnf l

instance Pretty License where
  pretty NONE = Disp.text "NONE"
  pretty (License l) = pretty l

-- |
-- >>> eitherParsec "BSD-3-Clause AND MIT" :: Either String License
-- Right (License (EAnd (ELicense (ELicenseId BSD_3_Clause) Nothing) (ELicense (ELicenseId MIT) Nothing)))
--
-- >>> eitherParsec "NONE" :: Either String License
-- Right NONE
instance Parsec License where
  parsec = NONE <$ P.try (P.string "NONE") <|> License <$> parsec
