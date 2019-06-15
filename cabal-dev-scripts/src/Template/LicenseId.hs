module Template.LicenseId (instantiate, Cfg(..), CfgLicenses(..)) where

-- data types

data Cfg = Cfg
  { cfgLicenseIds :: !String
  , cfgLicenseList_3_0 :: !String
  , cfgLicenseList_3_2 :: !String
  , cfgLicenseList_3_5 :: !String
  , cfgLicenseList_all :: !String
  , cfgLicenses :: ![CfgLicenses]
  } deriving (Show)

data CfgLicenses = CfgLicenses
  { cfgLicensesCon :: !String
  , cfgLicensesId :: !String
  , cfgLicensesIsOsiApproved :: !Bool
  , cfgLicensesName :: !String
  } deriving (Show)

-- template instantiation

instantiate :: Cfg -> String
instantiate cfg = ($ []) $ id
  . showString "{-# LANGUAGE DeriveDataTypeable #-}\n"
  . showString "{-# LANGUAGE DeriveGeneric      #-}\n"
  . showString "module Distribution.SPDX.LicenseId (\n"
  . showString "    LicenseId (..),\n"
  . showString "    licenseId,\n"
  . showString "    licenseName,\n"
  . showString "    licenseIsOsiApproved,\n"
  . showString "    mkLicenseId,\n"
  . showString "    licenseIdList,\n"
  . showString "    -- * Helpers\n"
  . showString "    licenseIdMigrationMessage,\n"
  . showString "    ) where\n"
  . showString "\n"
  . showString "import Distribution.Compat.Prelude\n"
  . showString "import Prelude ()\n"
  . showString "\n"
  . showString "import Distribution.Pretty\n"
  . showString "import Distribution.Parsec\n"
  . showString "import Distribution.Utils.Generic (isAsciiAlphaNum)\n"
  . showString "import Distribution.SPDX.LicenseListVersion\n"
  . showString "\n"
  . showString "import qualified Data.Binary.Get as Binary\n"
  . showString "import qualified Data.Binary.Put as Binary\n"
  . showString "import qualified Data.Map.Strict as Map\n"
  . showString "import qualified Distribution.Compat.CharParsing as P\n"
  . showString "import qualified Text.PrettyPrint as Disp\n"
  . showString "\n"
  . showString "-------------------------------------------------------------------------------\n"
  . showString "-- LicenseId\n"
  . showString "-------------------------------------------------------------------------------\n"
  . showString "\n"
  . showString "-- | SPDX License identifier\n"
  . showString "data LicenseId\n"
  . showString (cfgLicenseIds $ cfg)
  . showString "\n"
  . showString "  deriving (Eq, Ord, Enum, Bounded, Show, Read, Typeable, Data, Generic)\n"
  . showString "\n"
  . showString "instance Binary LicenseId where\n"
  . showString "    -- Word16 is encoded in big endianess\n"
  . showString "    -- https://github.com/kolmodin/binary/blob/master/src/Data/Binary/Class.hs#L220-LL227\n"
  . showString "    put = Binary.putWord16be . fromIntegral . fromEnum\n"
  . showString "    get = do\n"
  . showString "        i <- Binary.getWord16be\n"
  . showString "        if i > fromIntegral (fromEnum (maxBound :: LicenseId))\n"
  . showString "        then fail \"Too large LicenseId tag\"\n"
  . showString "        else return (toEnum (fromIntegral i))\n"
  . showString "\n"
  . showString "instance Pretty LicenseId where\n"
  . showString "    pretty = Disp.text . licenseId\n"
  . showString "\n"
  . showString "-- |\n"
  . showString "-- >>> eitherParsec \"BSD-3-Clause\" :: Either String LicenseId\n"
  . showString "-- Right BSD_3_Clause\n"
  . showString "--\n"
  . showString "-- >>> eitherParsec \"BSD3\" :: Either String LicenseId\n"
  . showString "-- Left \"...Unknown SPDX license identifier: 'BSD3' Do you mean BSD-3-Clause?\"\n"
  . showString "--\n"
  . showString "instance Parsec LicenseId where\n"
  . showString "    parsec = do\n"
  . showString "        n <- some $ P.satisfy $ \\c -> isAsciiAlphaNum c || c == '-' || c == '.'\n"
  . showString "        v <- askCabalSpecVersion\n"
  . showString "        maybe (fail $ \"Unknown SPDX license identifier: '\" ++  n ++ \"' \" ++ licenseIdMigrationMessage n) return $\n"
  . showString "            mkLicenseId (cabalSpecVersionToSPDXListVersion v) n\n"
  . showString "\n"
  . showString "instance NFData LicenseId where\n"
  . showString "    rnf l = l `seq` ()\n"
  . showString "\n"
  . showString "-- | Help message for migrating from non-SPDX license identifiers.\n"
  . showString "--\n"
  . showString "-- Old 'License' is almost SPDX, except for 'BSD2', 'BSD3'. This function\n"
  . showString "-- suggests SPDX variant:\n"
  . showString "--\n"
  . showString "-- >>> licenseIdMigrationMessage \"BSD3\"\n"
  . showString "-- \"Do you mean BSD-3-Clause?\"\n"
  . showString "--\n"
  . showString "-- Also 'OtherLicense', 'AllRightsReserved', and 'PublicDomain' aren't\n"
  . showString "-- valid SPDX identifiers\n"
  . showString "--\n"
  . showString "-- >>> traverse_ (print . licenseIdMigrationMessage) [ \"OtherLicense\", \"AllRightsReserved\", \"PublicDomain\" ]\n"
  . showString "-- \"SPDX license list contains plenty of licenses. See https://spdx.org/licenses/. Also they can be combined into complex expressions with AND and OR.\"\n"
  . showString "-- \"You can use NONE as a value of license field.\"\n"
  . showString "-- \"Public Domain is a complex matter. See https://wiki.spdx.org/view/Legal_Team/Decisions/Dealing_with_Public_Domain_within_SPDX_Files. Consider using a proper license.\"\n"
  . showString "--\n"
  . showString "-- SPDX License list version 3.0 introduced \"-only\" and \"-or-later\" variants for GNU family of licenses.\n"
  . showString "-- See <https://spdx.org/news/news/2018/01/license-list-30-released>\n"
  . showString "-- >>> licenseIdMigrationMessage \"GPL-2.0\"\n"
  . showString "-- \"SPDX license list 3.0 deprecated suffixless variants of GNU family of licenses. Use GPL-2.0-only or GPL-2.0-or-later.\"\n"
  . showString "--\n"
  . showString "-- For other common licenses their old license format coincides with the SPDX identifiers:\n"
  . showString "--\n"
  . showString "-- >>> traverse eitherParsec [\"GPL-2.0-only\", \"GPL-3.0-only\", \"LGPL-2.1-only\", \"MIT\", \"ISC\", \"MPL-2.0\", \"Apache-2.0\"] :: Either String [LicenseId]\n"
  . showString "-- Right [GPL_2_0_only,GPL_3_0_only,LGPL_2_1_only,MIT,ISC,MPL_2_0,Apache_2_0]\n"
  . showString "--\n"
  . showString "licenseIdMigrationMessage :: String -> String\n"
  . showString "licenseIdMigrationMessage = go where\n"
  . showString "    go l | gnuVariant l    = \"SPDX license list 3.0 deprecated suffixless variants of GNU family of licenses. Use \" ++ l ++ \"-only or \" ++ l ++ \"-or-later.\"\n"
  . showString "    go \"BSD3\"              = \"Do you mean BSD-3-Clause?\"\n"
  . showString "    go \"BSD2\"              = \"Do you mean BSD-2-Clause?\"\n"
  . showString "    go \"AllRightsReserved\" = \"You can use NONE as a value of license field.\"\n"
  . showString "    go \"OtherLicense\"      = \"SPDX license list contains plenty of licenses. See https://spdx.org/licenses/. Also they can be combined into complex expressions with AND and OR.\"\n"
  . showString "    go \"PublicDomain\"      = \"Public Domain is a complex matter. See https://wiki.spdx.org/view/Legal_Team/Decisions/Dealing_with_Public_Domain_within_SPDX_Files. Consider using a proper license.\"\n"
  . showString "\n"
  . showString "    -- otherwise, we don't know\n"
  . showString "    go _ = \"\"\n"
  . showString "\n"
  . showString "    gnuVariant = flip elem [\"GPL-2.0\", \"GPL-3.0\", \"LGPL-2.1\", \"LGPL-3.0\", \"AGPL-3.0\" ]\n"
  . showString "\n"
  . showString "-------------------------------------------------------------------------------\n"
  . showString "-- License Data\n"
  . showString "-------------------------------------------------------------------------------\n"
  . showString "\n"
  . showString "-- | License SPDX identifier, e.g. @\"BSD-3-Clause\"@.\n"
  . showString "licenseId :: LicenseId -> String\n"
  . _forImpl (cfgLicenses $ cfg) (\license -> id
    . showString "licenseId "
    . showString (cfgLicensesCon $ license)
    . showString " = "
    . showString (cfgLicensesId $ license)
    . showString "\n"
    )
  . showString "\n"
  . showString "-- | License name, e.g. @\"GNU General Public License v2.0 only\"@\n"
  . showString "licenseName :: LicenseId -> String\n"
  . _forImpl (cfgLicenses $ cfg) (\license -> id
    . showString "licenseName "
    . showString (cfgLicensesCon $ license)
    . showString " = "
    . showString (cfgLicensesName $ license)
    . showString "\n"
    )
  . showString "\n"
  . showString "-- | Whether the license is approved by Open Source Initiative (OSI).\n"
  . showString "--\n"
  . showString "-- See <https://opensource.org/licenses/alphabetical>.\n"
  . showString "licenseIsOsiApproved :: LicenseId -> Bool\n"
  . _forImpl (cfgLicenses $ cfg) (\license -> id
    . showString "licenseIsOsiApproved "
    . showString (cfgLicensesCon $ license)
    . showString " = "
    . ( _ifImpl (cfgLicensesIsOsiApproved $ license) $ id
      . showString "True"
      )
    . ( _ifImpl (not $ cfgLicensesIsOsiApproved $ license) $ id
      . showString "False"
      )
    . showString "\n"
    )
  . showString "\n"
  . showString "-------------------------------------------------------------------------------\n"
  . showString "-- Creation\n"
  . showString "-------------------------------------------------------------------------------\n"
  . showString "\n"
  . showString "licenseIdList :: LicenseListVersion -> [LicenseId]\n"
  . showString "licenseIdList LicenseListVersion_3_0 =\n"
  . showString (cfgLicenseList_3_0 $ cfg)
  . showString "\n"
  . showString "    ++ bulkOfLicenses\n"
  . showString "licenseIdList LicenseListVersion_3_2 =\n"
  . showString (cfgLicenseList_3_2 $ cfg)
  . showString "\n"
  . showString "    ++ bulkOfLicenses\n"
  . showString "licenseIdList LicenseListVersion_3_5 =\n"
  . showString (cfgLicenseList_3_5 $ cfg)
  . showString "\n"
  . showString "    ++ bulkOfLicenses\n"
  . showString "\n"
  . showString "-- | Create a 'LicenseId' from a 'String'.\n"
  . showString "mkLicenseId :: LicenseListVersion -> String -> Maybe LicenseId\n"
  . showString "mkLicenseId LicenseListVersion_3_0 s = Map.lookup s stringLookup_3_0\n"
  . showString "mkLicenseId LicenseListVersion_3_2 s = Map.lookup s stringLookup_3_2\n"
  . showString "mkLicenseId LicenseListVersion_3_5 s = Map.lookup s stringLookup_3_5\n"
  . showString "\n"
  . showString "stringLookup_3_0 :: Map String LicenseId\n"
  . showString "stringLookup_3_0 = Map.fromList $ map (\\i -> (licenseId i, i)) $\n"
  . showString "    licenseIdList LicenseListVersion_3_0\n"
  . showString "\n"
  . showString "stringLookup_3_2 :: Map String LicenseId\n"
  . showString "stringLookup_3_2 = Map.fromList $ map (\\i -> (licenseId i, i)) $\n"
  . showString "    licenseIdList LicenseListVersion_3_2\n"
  . showString "\n"
  . showString "stringLookup_3_5 :: Map String LicenseId\n"
  . showString "stringLookup_3_5 = Map.fromList $ map (\\i -> (licenseId i, i)) $\n"
  . showString "    licenseIdList LicenseListVersion_3_5\n"
  . showString "\n"
  . showString "--  | Licenses in all SPDX License lists\n"
  . showString "bulkOfLicenses :: [LicenseId]\n"
  . showString "bulkOfLicenses =\n"
  . showString (cfgLicenseList_all $ cfg)
  . showString "\n"

-- 'prelude'

_ifImpl :: Bool -> ShowS -> ShowS
_ifImpl True  s = s
_ifImpl False _ = id

_forImpl :: [a] -> (a -> ShowS) -> ShowS
_forImpl xs f = foldr (\x s -> f x . s) id xs

