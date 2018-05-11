{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
module Distribution.SPDX.LicenseId (
    LicenseId (..),
    licenseId,
    licenseName,
    licenseIsOsiApproved,
    mkLicenseId,
    -- * Helpers
    licenseIdMigrationMessage,
    ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Pretty
import Distribution.Parsec.Class
import Distribution.Utils.Generic (isAsciiAlphaNum)

import qualified Distribution.Compat.Map.Strict as Map
import qualified Distribution.Compat.CharParsing as P
import qualified Text.PrettyPrint as Disp

-------------------------------------------------------------------------------
-- LicenseId
-------------------------------------------------------------------------------

-- | SPDX License identifier
data LicenseId
{{{ licenseIds }}}
  deriving (Eq, Ord, Enum, Bounded, Show, Read, Typeable, Data, Generic)

instance Binary LicenseId

instance Pretty LicenseId where
    pretty = Disp.text . licenseId

-- |
-- >>> eitherParsec "BSD-3-Clause" :: Either String LicenseId
-- Right BSD_3_Clause
--
-- >>> eitherParsec "BSD3" :: Either String LicenseId
-- Left "...Unknown SPDX license identifier: 'BSD3' Do you mean BSD-3-Clause?"
--
instance Parsec LicenseId where
    parsec = do
        n <- some $ P.satisfy $ \c -> isAsciiAlphaNum c || c == '-' || c == '.'
        maybe (fail $ "Unknown SPDX license identifier: '" ++  n ++ "' " ++ licenseIdMigrationMessage n) return $ mkLicenseId n

instance NFData LicenseId where
    rnf l = l `seq` ()

-- | Help message for migrating from non-SDPX license identifiers.
--
-- Old 'License' is almost SDPX, except for 'BSD2', 'BSD3'. This function
-- suggests SPDX variant:
--
-- >>> licenseIdMigrationMessage "BSD3"
-- "Do you mean BSD-3-Clause?"
--
-- Also 'OtherLicense', 'AllRightsReserved', and 'PublicDomain' aren't
-- valid SPDX identifiers
--
-- >>> traverse_ (print . licenseIdMigrationMessage) [ "OtherLicense", "AllRightsReserved", "PublicDomain" ]
-- "SPDX license list contains plenty of licenses. See https://spdx.org/licenses/. Also they can be combined into complex expressions with AND and OR."
-- "You can use NONE as a value of license field."
-- "Public Domain is a complex matter. See https://wiki.spdx.org/view/Legal_Team/Decisions/Dealing_with_Public_Domain_within_SPDX_Files. Consider using a proper license."
--
-- SPDX License list version 3.0 introduced "-only" and "-or-later" variants for GNU family of licenses.
-- See <https://spdx.org/news/news/2018/01/license-list-30-released>
-- >>> licenseIdMigrationMessage "GPL-2.0"
-- "SDPX license list 3.0 deprecated suffixless variants of GNU family of licenses. Use GPL-2.0-only or GPL-2.0-or-later."
--
-- For other common licenses their old license format coincides with the SPDX identifiers:
--
-- >>> traverse eitherParsec ["GPL-2.0-only", "GPL-3.0-only", "LGPL-2.1-only", "MIT", "ISC", "MPL-2.0", "Apache-2.0"] :: Either String [LicenseId]
-- Right [GPL_2_0_only,GPL_3_0_only,LGPL_2_1_only,MIT,ISC,MPL_2_0,Apache_2_0]
--
licenseIdMigrationMessage :: String -> String
licenseIdMigrationMessage = go where
    go l | gnuVariant l    = "SDPX license list 3.0 deprecated suffixless variants of GNU family of licenses. Use " ++ l ++ "-only or " ++ l ++ "-or-later."
    go "BSD3"              = "Do you mean BSD-3-Clause?"
    go "BSD2"              = "Do you mean BSD-2-Clause?"
    go "AllRightsReserved" = "You can use NONE as a value of license field."
    go "OtherLicense"      = "SPDX license list contains plenty of licenses. See https://spdx.org/licenses/. Also they can be combined into complex expressions with AND and OR."
    go "PublicDomain"      = "Public Domain is a complex matter. See https://wiki.spdx.org/view/Legal_Team/Decisions/Dealing_with_Public_Domain_within_SPDX_Files. Consider using a proper license."

    -- otherwise, we don't know
    go _ = ""

    gnuVariant = flip elem ["GPL-2.0", "GPL-3.0", "LGPL-2.1", "LGPL-3.0", "AGPL-3.0" ]

-------------------------------------------------------------------------------
-- License Data
-------------------------------------------------------------------------------

-- | License SPDX identifier, e.g. @"BSD-3-Clause"@.
licenseId :: LicenseId -> String
{{#licenses}}
licenseId {{licenseCon}} = {{{licenseId}}}
{{/licenses}}

-- | License name, e.g. @"GNU General Public License v2.0 only"@
licenseName :: LicenseId -> String
{{#licenses}}
licenseName {{licenseCon}} = {{{licenseName}}}
{{/licenses}}

-- | Whether the license is approved by Open Source Initiative (OSI).
--
-- See <https://opensource.org/licenses/alphabetical>.
licenseIsOsiApproved :: LicenseId -> Bool
{{#licenses}}
licenseIsOsiApproved {{licenseCon}} = {{#isOsiApproved}}True{{/isOsiApproved}}{{^isOsiApproved}}False{{/isOsiApproved}}
{{/licenses}}

-------------------------------------------------------------------------------
-- Creation
-------------------------------------------------------------------------------

-- | Create a 'LicenseId' from a 'String'.
mkLicenseId :: String -> Maybe LicenseId
mkLicenseId s = Map.lookup s stringLookup

stringLookup :: Map String LicenseId
stringLookup = Map.fromList $ map (\i -> (licenseId i, i)) $ [minBound .. maxBound]
