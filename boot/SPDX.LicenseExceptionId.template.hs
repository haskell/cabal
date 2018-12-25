{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
module Distribution.SPDX.LicenseExceptionId (
    LicenseExceptionId (..),
    licenseExceptionId,
    licenseExceptionName,
    mkLicenseExceptionId,
    licenseExceptionIdList,
    ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Pretty
import Distribution.Parsec
import Distribution.Utils.Generic (isAsciiAlphaNum)
import Distribution.SPDX.LicenseListVersion

import qualified Data.Map.Strict as Map
import qualified Distribution.Compat.CharParsing as P
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
        v <- askCabalSpecVersion
        maybe (fail $ "Unknown SPDX license exception identifier: " ++ n) return $
            mkLicenseExceptionId (cabalSpecVersionToSPDXListVersion v) n

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

licenseExceptionIdList :: LicenseListVersion -> [LicenseExceptionId]
licenseExceptionIdList LicenseListVersion_3_0 =
{{{licenseList_3_0}}}
    ++ bulkOfLicenses
licenseExceptionIdList LicenseListVersion_3_2 =
{{{licenseList_3_2}}}
    ++ bulkOfLicenses
licenseExceptionIdList LicenseListVersion_3_3 =
{{{licenseList_3_3}}}
    ++ bulkOfLicenses

-- | Create a 'LicenseExceptionId' from a 'String'.
mkLicenseExceptionId :: LicenseListVersion -> String -> Maybe LicenseExceptionId
mkLicenseExceptionId LicenseListVersion_3_0 s = Map.lookup s stringLookup_3_0
mkLicenseExceptionId LicenseListVersion_3_2 s = Map.lookup s stringLookup_3_2
mkLicenseExceptionId LicenseListVersion_3_3 s = Map.lookup s stringLookup_3_3

stringLookup_3_0 :: Map String LicenseExceptionId
stringLookup_3_0 = Map.fromList $ map (\i -> (licenseExceptionId i, i)) $
    licenseExceptionIdList LicenseListVersion_3_0

stringLookup_3_2 :: Map String LicenseExceptionId
stringLookup_3_2 = Map.fromList $ map (\i -> (licenseExceptionId i, i)) $
    licenseExceptionIdList LicenseListVersion_3_3

stringLookup_3_3 :: Map String LicenseExceptionId
stringLookup_3_3 = Map.fromList $ map (\i -> (licenseExceptionId i, i)) $
    licenseExceptionIdList LicenseListVersion_3_3

--  | License exceptions in all SPDX License lists
bulkOfLicenses :: [LicenseExceptionId]
bulkOfLicenses =
{{{licenseList_all}}}
