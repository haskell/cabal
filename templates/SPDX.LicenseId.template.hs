{- FOURMOLU_DISABLE -}
{-# LANGUAGE DeriveDataTypeable #-}
module Distribution.SPDX.LicenseId (
    LicenseId (..),
    licenseId,
    licenseName,
    licenseIsOsiApproved,
    licenseIsFsfLibre,
    mkLicenseId,
    licenseIdList,
    -- * Helpers
    licenseIdMigrationMessage,
    ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Compat.Lens (set)
import Distribution.Pretty
import Distribution.Parsec
import Distribution.Utils.Generic (isAsciiAlphaNum)
import Distribution.Utils.Structured (Structured (..), nominalStructure, typeVersion)
import Distribution.SPDX.LicenseListVersion

import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.Map.Strict as Map
import qualified Distribution.Compat.CharParsing as P
import qualified Text.PrettyPrint as Disp

-------------------------------------------------------------------------------
-- LicenseId
-------------------------------------------------------------------------------

-- | SPDX License identifiers list v3.16
data LicenseId
{{ licenseIds }}
  deriving (Eq, Ord, Enum, Bounded, Show, Read, Typeable, Data)

instance Binary LicenseId where
    -- Word16 is encoded in big endianess
    -- https://github.com/kolmodin/binary/blob/master/src/Data/Binary/Class.hs#L220-LL227
    put = Binary.putWord16be . fromIntegral . fromEnum
    get = do
        i <- Binary.getWord16be
        if i > fromIntegral (fromEnum (maxBound :: LicenseId))
        then fail "Too large LicenseId tag"
        else return (toEnum (fromIntegral i))

-- note: remember to bump version each time the definition changes
instance Structured LicenseId where
    structure p = set typeVersion 306 $ nominalStructure p

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
        v <- askCabalSpecVersion
        maybe (fail $ "Unknown SPDX license identifier: '" ++  n ++ "' " ++ licenseIdMigrationMessage n) return $
            mkLicenseId (cabalSpecVersionToSPDXListVersion v) n

instance NFData LicenseId where
    rnf l = l `seq` ()

-- | Help message for migrating from non-SPDX license identifiers.
--
-- Old 'License' is almost SPDX, except for 'BSD2', 'BSD3'. This function
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
-- "SPDX license list 3.0 deprecated suffixless variants of GNU family of licenses. Use GPL-2.0-only or GPL-2.0-or-later."
--
-- For other common licenses their old license format coincides with the SPDX identifiers:
--
-- >>> traverse eitherParsec ["GPL-2.0-only", "GPL-3.0-only", "LGPL-2.1-only", "MIT", "ISC", "MPL-2.0", "Apache-2.0"] :: Either String [LicenseId]
-- Right [GPL_2_0_only,GPL_3_0_only,LGPL_2_1_only,MIT,ISC,MPL_2_0,Apache_2_0]
--
licenseIdMigrationMessage :: String -> String
licenseIdMigrationMessage = go where
    go l | gnuVariant l    = "SPDX license list 3.0 deprecated suffixless variants of GNU family of licenses. Use " ++ l ++ "-only or " ++ l ++ "-or-later."
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
{% for l in licenses %}
licenseId {{l.constructor}} = {{l.id}}
{% endfor %}

-- | License name, e.g. @"GNU General Public License v2.0 only"@
licenseName :: LicenseId -> String
{% for l in licenses %}
licenseName {{l.constructor}} = {{l.name}}
{% endfor %}

-- | Whether the license is approved by Open Source Initiative (OSI).
--
-- See <https://opensource.org/licenses/alphabetical>.
licenseIsOsiApproved :: LicenseId -> Bool
{% for l in licenses %}
{% if l.isOsiApproved %}
licenseIsOsiApproved {{l.constructor}} = True
{% endif %}
{% endfor %}
licenseIsOsiApproved _ = False

-- | Whether the license is considered libre by Free Software Foundation (FSF).
--
-- See <https://www.gnu.org/licenses/license-list.en.html>
--
-- @since 3.4.0.0
--
licenseIsFsfLibre :: LicenseId -> Bool
{% for l in licenses %}
{% if l.isFsfLibre %}
licenseIsFsfLibre {{l.constructor}} = True
{% endif %}
{% endfor %}
licenseIsFsfLibre _ = False

-------------------------------------------------------------------------------
-- Creation
-------------------------------------------------------------------------------

licenseIdList :: LicenseListVersion -> [LicenseId]
licenseIdList LicenseListVersion_3_0 =
{{licenseList_perv.v_3_0}}
    ++ bulkOfLicenses
licenseIdList LicenseListVersion_3_2 =
{{licenseList_perv.v_3_2}}
    ++ bulkOfLicenses
licenseIdList LicenseListVersion_3_6 =
{{licenseList_perv.v_3_6}}
    ++ bulkOfLicenses
licenseIdList LicenseListVersion_3_9 =
{{licenseList_perv.v_3_9}}
    ++ bulkOfLicenses
licenseIdList LicenseListVersion_3_10 =
{{licenseList_perv.v_3_10}}
    ++ bulkOfLicenses
licenseIdList LicenseListVersion_3_16 =
{{licenseList_perv.v_3_16}}
    ++ bulkOfLicenses

-- | Create a 'LicenseId' from a 'String'.
mkLicenseId :: LicenseListVersion -> String -> Maybe LicenseId
mkLicenseId LicenseListVersion_3_0  s = Map.lookup s stringLookup_3_0
mkLicenseId LicenseListVersion_3_2  s = Map.lookup s stringLookup_3_2
mkLicenseId LicenseListVersion_3_6  s = Map.lookup s stringLookup_3_6
mkLicenseId LicenseListVersion_3_9  s = Map.lookup s stringLookup_3_9
mkLicenseId LicenseListVersion_3_10 s = Map.lookup s stringLookup_3_10
mkLicenseId LicenseListVersion_3_16 s = Map.lookup s stringLookup_3_16

stringLookup_3_0 :: Map String LicenseId
stringLookup_3_0 = Map.fromList $ map (\i -> (licenseId i, i)) $
    licenseIdList LicenseListVersion_3_0

stringLookup_3_2 :: Map String LicenseId
stringLookup_3_2 = Map.fromList $ map (\i -> (licenseId i, i)) $
    licenseIdList LicenseListVersion_3_2

stringLookup_3_6 :: Map String LicenseId
stringLookup_3_6 = Map.fromList $ map (\i -> (licenseId i, i)) $
    licenseIdList LicenseListVersion_3_6

stringLookup_3_9 :: Map String LicenseId
stringLookup_3_9 = Map.fromList $ map (\i -> (licenseId i, i)) $
    licenseIdList LicenseListVersion_3_9

stringLookup_3_10 :: Map String LicenseId
stringLookup_3_10 = Map.fromList $ map (\i -> (licenseId i, i)) $
    licenseIdList LicenseListVersion_3_10

stringLookup_3_16 :: Map String LicenseId
stringLookup_3_16 = Map.fromList $ map (\i -> (licenseId i, i)) $
    licenseIdList LicenseListVersion_3_16

--  | Licenses in all SPDX License lists
bulkOfLicenses :: [LicenseId]
bulkOfLicenses =
{{licenseList_all}}
