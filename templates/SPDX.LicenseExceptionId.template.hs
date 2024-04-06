{- FOURMOLU_DISABLE -}
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
-- LicenseExceptionId
-------------------------------------------------------------------------------

-- | SPDX License Exceptions identifiers list v3.23
data LicenseExceptionId
{{ licenseIds }}
  deriving (Eq, Ord, Enum, Bounded, Show, Read, Typeable, Data, Generic)

instance Binary LicenseExceptionId where
    put = Binary.putWord8 . fromIntegral . fromEnum
    get = do
        i <- Binary.getWord8
        if i > fromIntegral (fromEnum (maxBound :: LicenseExceptionId))
        then fail "Too large LicenseExceptionId tag"
        else return (toEnum (fromIntegral i))

-- note: remember to bump version each time the definition changes
instance Structured LicenseExceptionId where
    structure p = set typeVersion 306 $ nominalStructure p

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
{% for l in licenses %}
licenseExceptionId {{l.constructor}} = {{l.id}}
{% endfor %}

-- | License name, e.g. @"GNU General Public License v2.0 only"@
licenseExceptionName :: LicenseExceptionId -> String
{% for l in licenses %}
licenseExceptionName {{l.constructor}} = {{l.name}}
{% endfor %}

-------------------------------------------------------------------------------
-- Creation
-------------------------------------------------------------------------------

licenseExceptionIdList :: LicenseListVersion -> [LicenseExceptionId]
licenseExceptionIdList LicenseListVersion_3_0 =
{{licenseList_perv.v_3_0}}
    ++ bulkOfLicenses
licenseExceptionIdList LicenseListVersion_3_2 =
{{licenseList_perv.v_3_2}}
    ++ bulkOfLicenses
licenseExceptionIdList LicenseListVersion_3_6 =
{{licenseList_perv.v_3_6}}
    ++ bulkOfLicenses
licenseExceptionIdList LicenseListVersion_3_9 =
{{licenseList_perv.v_3_9}}
    ++ bulkOfLicenses
licenseExceptionIdList LicenseListVersion_3_10 =
{{licenseList_perv.v_3_10}}
    ++ bulkOfLicenses
licenseExceptionIdList LicenseListVersion_3_16 =
{{licenseList_perv.v_3_16}}
    ++ bulkOfLicenses
licenseExceptionIdList LicenseListVersion_3_23 =
{{licenseList_perv.v_3_23}}
    ++ bulkOfLicenses

-- | Create a 'LicenseExceptionId' from a 'String'.
mkLicenseExceptionId :: LicenseListVersion -> String -> Maybe LicenseExceptionId
mkLicenseExceptionId LicenseListVersion_3_0  s = Map.lookup s stringLookup_3_0
mkLicenseExceptionId LicenseListVersion_3_2  s = Map.lookup s stringLookup_3_2
mkLicenseExceptionId LicenseListVersion_3_6  s = Map.lookup s stringLookup_3_6
mkLicenseExceptionId LicenseListVersion_3_9  s = Map.lookup s stringLookup_3_9
mkLicenseExceptionId LicenseListVersion_3_10 s = Map.lookup s stringLookup_3_10
mkLicenseExceptionId LicenseListVersion_3_16 s = Map.lookup s stringLookup_3_16
mkLicenseExceptionId LicenseListVersion_3_23 s = Map.lookup s stringLookup_3_23

stringLookup_3_0 :: Map String LicenseExceptionId
stringLookup_3_0 = Map.fromList $ map (\i -> (licenseExceptionId i, i)) $
    licenseExceptionIdList LicenseListVersion_3_0

stringLookup_3_2 :: Map String LicenseExceptionId
stringLookup_3_2 = Map.fromList $ map (\i -> (licenseExceptionId i, i)) $
    licenseExceptionIdList LicenseListVersion_3_2

stringLookup_3_6 :: Map String LicenseExceptionId
stringLookup_3_6 = Map.fromList $ map (\i -> (licenseExceptionId i, i)) $
    licenseExceptionIdList LicenseListVersion_3_6

stringLookup_3_9 :: Map String LicenseExceptionId
stringLookup_3_9 = Map.fromList $ map (\i -> (licenseExceptionId i, i)) $
    licenseExceptionIdList LicenseListVersion_3_9

stringLookup_3_10 :: Map String LicenseExceptionId
stringLookup_3_10 = Map.fromList $ map (\i -> (licenseExceptionId i, i)) $
    licenseExceptionIdList LicenseListVersion_3_10

stringLookup_3_16 :: Map String LicenseExceptionId
stringLookup_3_16 = Map.fromList $ map (\i -> (licenseExceptionId i, i)) $
    licenseExceptionIdList LicenseListVersion_3_16

stringLookup_3_23 :: Map String LicenseExceptionId
stringLookup_3_23 = Map.fromList $ map (\i -> (licenseExceptionId i, i)) $
    licenseExceptionIdList LicenseListVersion_3_23

--  | License exceptions in all SPDX License lists
bulkOfLicenses :: [LicenseExceptionId]
bulkOfLicenses =
{{licenseList_all}}
