{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
module Distribution.SPDX.LicenseId (
    LicenseId (..),
    licenseId,
    licenseName,
    licenseIsOsiApproved,
    mkLicenseId,
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

instance Parsec LicenseId where
    parsec = do
        n <- some $ P.satisfy $ \c -> isAsciiAlphaNum c || c == '-' || c == '.'
        maybe (fail $ "Unknown SPDX license identifier: " ++ n) return $ mkLicenseId n

instance NFData LicenseId where
    rnf l = l `seq` ()

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
