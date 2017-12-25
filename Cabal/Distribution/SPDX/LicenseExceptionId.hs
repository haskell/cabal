-- This file is generated. See Makefile's spdx rule
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
import qualified Distribution.Compat.CharParsing as P
import qualified Text.PrettyPrint as Disp

-------------------------------------------------------------------------------
-- LicenseExceptionId
-------------------------------------------------------------------------------

-- | SPDX License identifier
data LicenseExceptionId
    = DS389_exception -- ^ @389-exception@, 389 Directory Server Exception
    | Autoconf_exception_2_0 -- ^ @Autoconf-exception-2.0@, Autoconf exception 2.0
    | Autoconf_exception_3_0 -- ^ @Autoconf-exception-3.0@, Autoconf exception 3.0
    | Bison_exception_2_2 -- ^ @Bison-exception-2.2@, Bison exception 2.2
    | Classpath_exception_2_0 -- ^ @Classpath-exception-2.0@, Classpath exception 2.0
    | CLISP_exception_2_0 -- ^ @CLISP-exception-2.0@, CLISP exception 2.0 
    | DigiRule_FOSS_exception -- ^ @DigiRule-FOSS-exception@, DigiRule FOSS License Exception
    | ECos_exception_2_0 -- ^ @eCos-exception-2.0@, eCos exception 2.0
    | Fawkes_Runtime_exception -- ^ @Fawkes-Runtime-exception@, Fawkes Runtime Exception 
    | FLTK_exception -- ^ @FLTK-exception@, FLTK exception
    | Font_exception_2_0 -- ^ @Font-exception-2.0@, Font exception 2.0
    | Freertos_exception_2_0 -- ^ @freertos-exception-2.0@, FreeRTOS Exception 2.0
    | GCC_exception_2_0 -- ^ @GCC-exception-2.0@, GCC Runtime Library exception 2.0
    | GCC_exception_3_1 -- ^ @GCC-exception-3.1@, GCC Runtime Library exception 3.1
    | Gnu_javamail_exception -- ^ @gnu-javamail-exception@, GNU JavaMail exception
    | I2p_gpl_java_exception -- ^ @i2p-gpl-java-exception@, i2p GPL+Java Exception
    | Libtool_exception -- ^ @Libtool-exception@, Libtool Exception
    | LZMA_exception -- ^ @LZMA-exception@, LZMA exception
    | Mif_exception -- ^ @mif-exception@, Macros and Inline Functions Exception
    | Nokia_Qt_exception_1_1 -- ^ @Nokia-Qt-exception-1.1@, Nokia Qt LGPL exception 1.1
    | OCCT_exception_1_0 -- ^ @OCCT-exception-1.0@, Open CASCADE Exception 1.0
    | Openvpn_openssl_exception -- ^ @openvpn-openssl-exception@, OpenVPN OpenSSL Exception
    | Qwt_exception_1_0 -- ^ @Qwt-exception-1.0@, Qwt exception 1.0
    | U_boot_exception_2_0 -- ^ @u-boot-exception-2.0@, U-Boot exception 2.0
    | WxWindows_exception_3_1 -- ^ @WxWindows-exception-3.1@, WxWindows Library Exception 3.1
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
licenseExceptionId DS389_exception = "389-exception"
licenseExceptionId Autoconf_exception_2_0 = "Autoconf-exception-2.0"
licenseExceptionId Autoconf_exception_3_0 = "Autoconf-exception-3.0"
licenseExceptionId Bison_exception_2_2 = "Bison-exception-2.2"
licenseExceptionId Classpath_exception_2_0 = "Classpath-exception-2.0"
licenseExceptionId CLISP_exception_2_0 = "CLISP-exception-2.0"
licenseExceptionId DigiRule_FOSS_exception = "DigiRule-FOSS-exception"
licenseExceptionId ECos_exception_2_0 = "eCos-exception-2.0"
licenseExceptionId Fawkes_Runtime_exception = "Fawkes-Runtime-exception"
licenseExceptionId FLTK_exception = "FLTK-exception"
licenseExceptionId Font_exception_2_0 = "Font-exception-2.0"
licenseExceptionId Freertos_exception_2_0 = "freertos-exception-2.0"
licenseExceptionId GCC_exception_2_0 = "GCC-exception-2.0"
licenseExceptionId GCC_exception_3_1 = "GCC-exception-3.1"
licenseExceptionId Gnu_javamail_exception = "gnu-javamail-exception"
licenseExceptionId I2p_gpl_java_exception = "i2p-gpl-java-exception"
licenseExceptionId Libtool_exception = "Libtool-exception"
licenseExceptionId LZMA_exception = "LZMA-exception"
licenseExceptionId Mif_exception = "mif-exception"
licenseExceptionId Nokia_Qt_exception_1_1 = "Nokia-Qt-exception-1.1"
licenseExceptionId OCCT_exception_1_0 = "OCCT-exception-1.0"
licenseExceptionId Openvpn_openssl_exception = "openvpn-openssl-exception"
licenseExceptionId Qwt_exception_1_0 = "Qwt-exception-1.0"
licenseExceptionId U_boot_exception_2_0 = "u-boot-exception-2.0"
licenseExceptionId WxWindows_exception_3_1 = "WxWindows-exception-3.1"

-- | License name, e.g. @"GNU General Public License v2.0 only"@
licenseExceptionName :: LicenseExceptionId -> String
licenseExceptionName DS389_exception = "389 Directory Server Exception"
licenseExceptionName Autoconf_exception_2_0 = "Autoconf exception 2.0"
licenseExceptionName Autoconf_exception_3_0 = "Autoconf exception 3.0"
licenseExceptionName Bison_exception_2_2 = "Bison exception 2.2"
licenseExceptionName Classpath_exception_2_0 = "Classpath exception 2.0"
licenseExceptionName CLISP_exception_2_0 = "CLISP exception 2.0 "
licenseExceptionName DigiRule_FOSS_exception = "DigiRule FOSS License Exception"
licenseExceptionName ECos_exception_2_0 = "eCos exception 2.0"
licenseExceptionName Fawkes_Runtime_exception = "Fawkes Runtime Exception "
licenseExceptionName FLTK_exception = "FLTK exception"
licenseExceptionName Font_exception_2_0 = "Font exception 2.0"
licenseExceptionName Freertos_exception_2_0 = "FreeRTOS Exception 2.0"
licenseExceptionName GCC_exception_2_0 = "GCC Runtime Library exception 2.0"
licenseExceptionName GCC_exception_3_1 = "GCC Runtime Library exception 3.1"
licenseExceptionName Gnu_javamail_exception = "GNU JavaMail exception"
licenseExceptionName I2p_gpl_java_exception = "i2p GPL+Java Exception"
licenseExceptionName Libtool_exception = "Libtool Exception"
licenseExceptionName LZMA_exception = "LZMA exception"
licenseExceptionName Mif_exception = "Macros and Inline Functions Exception"
licenseExceptionName Nokia_Qt_exception_1_1 = "Nokia Qt LGPL exception 1.1"
licenseExceptionName OCCT_exception_1_0 = "Open CASCADE Exception 1.0"
licenseExceptionName Openvpn_openssl_exception = "OpenVPN OpenSSL Exception"
licenseExceptionName Qwt_exception_1_0 = "Qwt exception 1.0"
licenseExceptionName U_boot_exception_2_0 = "U-Boot exception 2.0"
licenseExceptionName WxWindows_exception_3_1 = "WxWindows Library Exception 3.1"

-------------------------------------------------------------------------------
-- Creation
-------------------------------------------------------------------------------

-- | Create a 'LicenseExceptionId' from a 'String'.
mkLicenseExceptionId :: String -> Maybe LicenseExceptionId
mkLicenseExceptionId s = Map.lookup s stringLookup

stringLookup :: Map String LicenseExceptionId
stringLookup = Map.fromList $ map (\i -> (licenseExceptionId i, i)) $ [minBound .. maxBound]
