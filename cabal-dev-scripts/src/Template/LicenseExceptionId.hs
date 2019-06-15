module Template.LicenseExceptionId (instantiate, Cfg(..), CfgLicenses(..)) where

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
  , cfgLicensesName :: !String
  } deriving (Show)

-- template instantiation

instantiate :: Cfg -> String
instantiate cfg = ($ []) $ id
  . showString "{-# LANGUAGE DeriveDataTypeable #-}\n"
  . showString "{-# LANGUAGE DeriveGeneric      #-}\n"
  . showString "module Distribution.SPDX.LicenseExceptionId (\n"
  . showString "    LicenseExceptionId (..),\n"
  . showString "    licenseExceptionId,\n"
  . showString "    licenseExceptionName,\n"
  . showString "    mkLicenseExceptionId,\n"
  . showString "    licenseExceptionIdList,\n"
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
  . showString "-- LicenseExceptionId\n"
  . showString "-------------------------------------------------------------------------------\n"
  . showString "\n"
  . showString "-- | SPDX License identifier\n"
  . showString "data LicenseExceptionId\n"
  . showString (cfgLicenseIds $ cfg)
  . showString "\n"
  . showString "  deriving (Eq, Ord, Enum, Bounded, Show, Read, Typeable, Data, Generic)\n"
  . showString "\n"
  . showString "instance Binary LicenseExceptionId where\n"
  . showString "    put = Binary.putWord8 . fromIntegral . fromEnum\n"
  . showString "    get = do\n"
  . showString "        i <- Binary.getWord8\n"
  . showString "        if i > fromIntegral (fromEnum (maxBound :: LicenseExceptionId))\n"
  . showString "        then fail \"Too large LicenseExceptionId tag\"\n"
  . showString "        else return (toEnum (fromIntegral i))\n"
  . showString "\n"
  . showString "instance Pretty LicenseExceptionId where\n"
  . showString "    pretty = Disp.text . licenseExceptionId\n"
  . showString "\n"
  . showString "instance Parsec LicenseExceptionId where\n"
  . showString "    parsec = do\n"
  . showString "        n <- some $ P.satisfy $ \\c -> isAsciiAlphaNum c || c == '-' || c == '.'\n"
  . showString "        v <- askCabalSpecVersion\n"
  . showString "        maybe (fail $ \"Unknown SPDX license exception identifier: \" ++ n) return $\n"
  . showString "            mkLicenseExceptionId (cabalSpecVersionToSPDXListVersion v) n\n"
  . showString "\n"
  . showString "instance NFData LicenseExceptionId where\n"
  . showString "    rnf l = l `seq` ()\n"
  . showString "\n"
  . showString "-------------------------------------------------------------------------------\n"
  . showString "-- License Data\n"
  . showString "-------------------------------------------------------------------------------\n"
  . showString "\n"
  . showString "-- | License SPDX identifier, e.g. @\"BSD-3-Clause\"@.\n"
  . showString "licenseExceptionId :: LicenseExceptionId -> String\n"
  . _forImpl (cfgLicenses $ cfg) (\l -> id
    . showString "licenseExceptionId "
    . showString (cfgLicensesCon $ l)
    . showString " = "
    . showString (cfgLicensesId $ l)
    . showString "\n"
    )
  . showString "\n"
  . showString "-- | License name, e.g. @\"GNU General Public License v2.0 only\"@\n"
  . showString "licenseExceptionName :: LicenseExceptionId -> String\n"
  . _forImpl (cfgLicenses $ cfg) (\l -> id
    . showString "licenseExceptionName "
    . showString (cfgLicensesCon $ l)
    . showString " = "
    . showString (cfgLicensesName $ l)
    . showString "\n"
    )
  . showString "\n"
  . showString "-------------------------------------------------------------------------------\n"
  . showString "-- Creation\n"
  . showString "-------------------------------------------------------------------------------\n"
  . showString "\n"
  . showString "licenseExceptionIdList :: LicenseListVersion -> [LicenseExceptionId]\n"
  . showString "licenseExceptionIdList LicenseListVersion_3_0 =\n"
  . showString (cfgLicenseList_3_0 $ cfg)
  . showString "\n"
  . showString "    ++ bulkOfLicenses\n"
  . showString "licenseExceptionIdList LicenseListVersion_3_2 =\n"
  . showString (cfgLicenseList_3_2 $ cfg)
  . showString "\n"
  . showString "    ++ bulkOfLicenses\n"
  . showString "licenseExceptionIdList LicenseListVersion_3_5 =\n"
  . showString (cfgLicenseList_3_5 $ cfg)
  . showString "\n"
  . showString "    ++ bulkOfLicenses\n"
  . showString "\n"
  . showString "-- | Create a 'LicenseExceptionId' from a 'String'.\n"
  . showString "mkLicenseExceptionId :: LicenseListVersion -> String -> Maybe LicenseExceptionId\n"
  . showString "mkLicenseExceptionId LicenseListVersion_3_0 s = Map.lookup s stringLookup_3_0\n"
  . showString "mkLicenseExceptionId LicenseListVersion_3_2 s = Map.lookup s stringLookup_3_2\n"
  . showString "mkLicenseExceptionId LicenseListVersion_3_5 s = Map.lookup s stringLookup_3_5\n"
  . showString "\n"
  . showString "stringLookup_3_0 :: Map String LicenseExceptionId\n"
  . showString "stringLookup_3_0 = Map.fromList $ map (\\i -> (licenseExceptionId i, i)) $\n"
  . showString "    licenseExceptionIdList LicenseListVersion_3_0\n"
  . showString "\n"
  . showString "stringLookup_3_2 :: Map String LicenseExceptionId\n"
  . showString "stringLookup_3_2 = Map.fromList $ map (\\i -> (licenseExceptionId i, i)) $\n"
  . showString "    licenseExceptionIdList LicenseListVersion_3_2\n"
  . showString "\n"
  . showString "stringLookup_3_5 :: Map String LicenseExceptionId\n"
  . showString "stringLookup_3_5 = Map.fromList $ map (\\i -> (licenseExceptionId i, i)) $\n"
  . showString "    licenseExceptionIdList LicenseListVersion_3_5\n"
  . showString "\n"
  . showString "--  | License exceptions in all SPDX License lists\n"
  . showString "bulkOfLicenses :: [LicenseExceptionId]\n"
  . showString "bulkOfLicenses =\n"
  . showString (cfgLicenseList_all $ cfg)
  . showString "\n"

-- 'prelude'

_ifImpl :: Bool -> ShowS -> ShowS
_ifImpl True  s = s
_ifImpl False _ = id

_forImpl :: [a] -> (a -> ShowS) -> ShowS
_forImpl xs f = foldr (\x s -> f x . s) id xs

