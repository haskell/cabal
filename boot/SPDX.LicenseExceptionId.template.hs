{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
module Distribution.SPDX.LicenseExceptionId (
    LicenseExceptionId (..),
    licenseExceptionId,
    licenseExceptionName,
    mkLicenseExceptionId,
    ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Pretty
import Distribution.Parsec.Class
import Distribution.Utils.Generic (isAsciiAlphaNum)

import qualified Distribution.Compat.Map.Strict as Map
import qualified Distribution.Compat.Parsec as P
import qualified Text.PrettyPrint as Disp

-------------------------------------------------------------------------------
-- LicenseExceptionId
-------------------------------------------------------------------------------

-- | SPDX License identifier
data LicenseExceptionId
{{{ licenseIds }}}
  deriving (Eq, Ord, Enum, Bounded, Show, Read, Typeable, Data, Generic)

instance Binary LicenseExceptionId

instance Pretty LicenseExceptionId where
    pretty = Disp.text . licenseExceptionId

instance Parsec LicenseExceptionId where
    parsec = do
        n <- some $ P.satisfy $ \c -> isAsciiAlphaNum c || c == '-' || c == '.'
        maybe (fail $ "Unknown SPDX license exception identifier: " ++ n) return $ mkLicenseExceptionId n

instance NFData LicenseExceptionId where
    rnf l = l `seq` ()

-------------------------------------------------------------------------------
-- License Data
-------------------------------------------------------------------------------

-- | License SPDX identifier, e.g. @"BSD-3-Clause"@.
licenseExceptionId :: LicenseExceptionId -> String
{{#licenses}}
licenseExceptionId {{licenseCon}} = {{{licenseId}}}
{{/licenses}}

-- | License name, e.g. @"GNU General Public License v2.0 only"@
licenseExceptionName :: LicenseExceptionId -> String
{{#licenses}}
licenseExceptionName {{licenseCon}} = {{{licenseName}}}
{{/licenses}}

-------------------------------------------------------------------------------
-- Creation
-------------------------------------------------------------------------------

-- | Create a 'LicenseExceptionId' from a 'String'.
mkLicenseExceptionId :: String -> Maybe LicenseExceptionId
mkLicenseExceptionId s = Map.lookup s stringLookup

stringLookup :: Map String LicenseExceptionId
stringLookup = Map.fromList $ map (\i -> (licenseExceptionId i, i)) $ [minBound .. maxBound]
