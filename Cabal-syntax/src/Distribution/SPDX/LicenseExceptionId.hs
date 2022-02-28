-- This file is generated. See Makefile's spdx rule
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

-- | SPDX License identifier
data LicenseExceptionId
    = DS389_exception -- ^ @389-exception@, 389 Directory Server Exception
    | Autoconf_exception_2_0 -- ^ @Autoconf-exception-2.0@, Autoconf exception 2.0
    | Autoconf_exception_3_0 -- ^ @Autoconf-exception-3.0@, Autoconf exception 3.0
    | Bison_exception_2_2 -- ^ @Bison-exception-2.2@, Bison exception 2.2
    | Bootloader_exception -- ^ @Bootloader-exception@, Bootloader Distribution Exception
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
    | GPL_3_0_linking_exception -- ^ @GPL-3.0-linking-exception@, GPL-3.0 Linking Exception, SPDX License List 3.9, SPDX License List 3.10
    | GPL_3_0_linking_source_exception -- ^ @GPL-3.0-linking-source-exception@, GPL-3.0 Linking Exception (with Corresponding Source), SPDX License List 3.9, SPDX License List 3.10
    | GPL_CC_1_0 -- ^ @GPL-CC-1.0@, GPL Cooperation Commitment 1.0, SPDX License List 3.6, SPDX License List 3.9, SPDX License List 3.10
    | I2p_gpl_java_exception -- ^ @i2p-gpl-java-exception@, i2p GPL+Java Exception
    | LGPL_3_0_linking_exception -- ^ @LGPL-3.0-linking-exception@, LGPL-3.0 Linking Exception, SPDX License List 3.9, SPDX License List 3.10
    | Libtool_exception -- ^ @Libtool-exception@, Libtool Exception
    | Linux_syscall_note -- ^ @Linux-syscall-note@, Linux Syscall Note
    | LLVM_exception -- ^ @LLVM-exception@, LLVM Exception, SPDX License List 3.2, SPDX License List 3.6, SPDX License List 3.9, SPDX License List 3.10
    | LZMA_exception -- ^ @LZMA-exception@, LZMA exception
    | Mif_exception -- ^ @mif-exception@, Macros and Inline Functions Exception
    | Nokia_Qt_exception_1_1 -- ^ @Nokia-Qt-exception-1.1@, Nokia Qt LGPL exception 1.1, SPDX License List 3.0, SPDX License List 3.2
    | OCaml_LGPL_linking_exception -- ^ @OCaml-LGPL-linking-exception@, OCaml LGPL Linking Exception, SPDX License List 3.6, SPDX License List 3.9, SPDX License List 3.10
    | OCCT_exception_1_0 -- ^ @OCCT-exception-1.0@, Open CASCADE Exception 1.0
    | OpenJDK_assembly_exception_1_0 -- ^ @OpenJDK-assembly-exception-1.0@, OpenJDK Assembly exception 1.0, SPDX License List 3.2, SPDX License List 3.6, SPDX License List 3.9, SPDX License List 3.10
    | Openvpn_openssl_exception -- ^ @openvpn-openssl-exception@, OpenVPN OpenSSL Exception
    | PS_or_PDF_font_exception_20170817 -- ^ @PS-or-PDF-font-exception-20170817@, PS/PDF font exception (2017-08-17), SPDX License List 3.2, SPDX License List 3.6, SPDX License List 3.9, SPDX License List 3.10
    | Qt_GPL_exception_1_0 -- ^ @Qt-GPL-exception-1.0@, Qt GPL exception 1.0, SPDX License List 3.2, SPDX License List 3.6, SPDX License List 3.9, SPDX License List 3.10
    | Qt_LGPL_exception_1_1 -- ^ @Qt-LGPL-exception-1.1@, Qt LGPL exception 1.1, SPDX License List 3.2, SPDX License List 3.6, SPDX License List 3.9, SPDX License List 3.10
    | Qwt_exception_1_0 -- ^ @Qwt-exception-1.0@, Qwt exception 1.0
    | SHL_2_0 -- ^ @SHL-2.0@, Solderpad Hardware License v2.0, SPDX License List 3.9, SPDX License List 3.10
    | SHL_2_1 -- ^ @SHL-2.1@, Solderpad Hardware License v2.1, SPDX License List 3.9, SPDX License List 3.10
    | Swift_exception -- ^ @Swift-exception@, Swift Exception, SPDX License List 3.6, SPDX License List 3.9, SPDX License List 3.10
    | U_boot_exception_2_0 -- ^ @u-boot-exception-2.0@, U-Boot exception 2.0
    | Universal_FOSS_exception_1_0 -- ^ @Universal-FOSS-exception-1.0@, Universal FOSS Exception, Version 1.0, SPDX License List 3.6, SPDX License List 3.9, SPDX License List 3.10
    | WxWindows_exception_3_1 -- ^ @WxWindows-exception-3.1@, WxWindows Library Exception 3.1
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
licenseExceptionId DS389_exception = "389-exception"
licenseExceptionId Autoconf_exception_2_0 = "Autoconf-exception-2.0"
licenseExceptionId Autoconf_exception_3_0 = "Autoconf-exception-3.0"
licenseExceptionId Bison_exception_2_2 = "Bison-exception-2.2"
licenseExceptionId Bootloader_exception = "Bootloader-exception"
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
licenseExceptionId GPL_3_0_linking_exception = "GPL-3.0-linking-exception"
licenseExceptionId GPL_3_0_linking_source_exception = "GPL-3.0-linking-source-exception"
licenseExceptionId GPL_CC_1_0 = "GPL-CC-1.0"
licenseExceptionId I2p_gpl_java_exception = "i2p-gpl-java-exception"
licenseExceptionId LGPL_3_0_linking_exception = "LGPL-3.0-linking-exception"
licenseExceptionId Libtool_exception = "Libtool-exception"
licenseExceptionId Linux_syscall_note = "Linux-syscall-note"
licenseExceptionId LLVM_exception = "LLVM-exception"
licenseExceptionId LZMA_exception = "LZMA-exception"
licenseExceptionId Mif_exception = "mif-exception"
licenseExceptionId Nokia_Qt_exception_1_1 = "Nokia-Qt-exception-1.1"
licenseExceptionId OCaml_LGPL_linking_exception = "OCaml-LGPL-linking-exception"
licenseExceptionId OCCT_exception_1_0 = "OCCT-exception-1.0"
licenseExceptionId OpenJDK_assembly_exception_1_0 = "OpenJDK-assembly-exception-1.0"
licenseExceptionId Openvpn_openssl_exception = "openvpn-openssl-exception"
licenseExceptionId PS_or_PDF_font_exception_20170817 = "PS-or-PDF-font-exception-20170817"
licenseExceptionId Qt_GPL_exception_1_0 = "Qt-GPL-exception-1.0"
licenseExceptionId Qt_LGPL_exception_1_1 = "Qt-LGPL-exception-1.1"
licenseExceptionId Qwt_exception_1_0 = "Qwt-exception-1.0"
licenseExceptionId SHL_2_0 = "SHL-2.0"
licenseExceptionId SHL_2_1 = "SHL-2.1"
licenseExceptionId Swift_exception = "Swift-exception"
licenseExceptionId U_boot_exception_2_0 = "u-boot-exception-2.0"
licenseExceptionId Universal_FOSS_exception_1_0 = "Universal-FOSS-exception-1.0"
licenseExceptionId WxWindows_exception_3_1 = "WxWindows-exception-3.1"

-- | License name, e.g. @"GNU General Public License v2.0 only"@
licenseExceptionName :: LicenseExceptionId -> String
licenseExceptionName DS389_exception = "389 Directory Server Exception"
licenseExceptionName Autoconf_exception_2_0 = "Autoconf exception 2.0"
licenseExceptionName Autoconf_exception_3_0 = "Autoconf exception 3.0"
licenseExceptionName Bison_exception_2_2 = "Bison exception 2.2"
licenseExceptionName Bootloader_exception = "Bootloader Distribution Exception"
licenseExceptionName Classpath_exception_2_0 = "Classpath exception 2.0"
licenseExceptionName CLISP_exception_2_0 = "CLISP exception 2.0"
licenseExceptionName DigiRule_FOSS_exception = "DigiRule FOSS License Exception"
licenseExceptionName ECos_exception_2_0 = "eCos exception 2.0"
licenseExceptionName Fawkes_Runtime_exception = "Fawkes Runtime Exception"
licenseExceptionName FLTK_exception = "FLTK exception"
licenseExceptionName Font_exception_2_0 = "Font exception 2.0"
licenseExceptionName Freertos_exception_2_0 = "FreeRTOS Exception 2.0"
licenseExceptionName GCC_exception_2_0 = "GCC Runtime Library exception 2.0"
licenseExceptionName GCC_exception_3_1 = "GCC Runtime Library exception 3.1"
licenseExceptionName Gnu_javamail_exception = "GNU JavaMail exception"
licenseExceptionName GPL_3_0_linking_exception = "GPL-3.0 Linking Exception"
licenseExceptionName GPL_3_0_linking_source_exception = "GPL-3.0 Linking Exception (with Corresponding Source)"
licenseExceptionName GPL_CC_1_0 = "GPL Cooperation Commitment 1.0"
licenseExceptionName I2p_gpl_java_exception = "i2p GPL+Java Exception"
licenseExceptionName LGPL_3_0_linking_exception = "LGPL-3.0 Linking Exception"
licenseExceptionName Libtool_exception = "Libtool Exception"
licenseExceptionName Linux_syscall_note = "Linux Syscall Note"
licenseExceptionName LLVM_exception = "LLVM Exception"
licenseExceptionName LZMA_exception = "LZMA exception"
licenseExceptionName Mif_exception = "Macros and Inline Functions Exception"
licenseExceptionName Nokia_Qt_exception_1_1 = "Nokia Qt LGPL exception 1.1"
licenseExceptionName OCaml_LGPL_linking_exception = "OCaml LGPL Linking Exception"
licenseExceptionName OCCT_exception_1_0 = "Open CASCADE Exception 1.0"
licenseExceptionName OpenJDK_assembly_exception_1_0 = "OpenJDK Assembly exception 1.0"
licenseExceptionName Openvpn_openssl_exception = "OpenVPN OpenSSL Exception"
licenseExceptionName PS_or_PDF_font_exception_20170817 = "PS/PDF font exception (2017-08-17)"
licenseExceptionName Qt_GPL_exception_1_0 = "Qt GPL exception 1.0"
licenseExceptionName Qt_LGPL_exception_1_1 = "Qt LGPL exception 1.1"
licenseExceptionName Qwt_exception_1_0 = "Qwt exception 1.0"
licenseExceptionName SHL_2_0 = "Solderpad Hardware License v2.0"
licenseExceptionName SHL_2_1 = "Solderpad Hardware License v2.1"
licenseExceptionName Swift_exception = "Swift Exception"
licenseExceptionName U_boot_exception_2_0 = "U-Boot exception 2.0"
licenseExceptionName Universal_FOSS_exception_1_0 = "Universal FOSS Exception, Version 1.0"
licenseExceptionName WxWindows_exception_3_1 = "WxWindows Library Exception 3.1"

-------------------------------------------------------------------------------
-- Creation
-------------------------------------------------------------------------------

licenseExceptionIdList :: LicenseListVersion -> [LicenseExceptionId]
licenseExceptionIdList LicenseListVersion_3_0 =
    [ Nokia_Qt_exception_1_1
    ]
    ++ bulkOfLicenses
licenseExceptionIdList LicenseListVersion_3_2 =
    [ LLVM_exception
    , Nokia_Qt_exception_1_1
    , OpenJDK_assembly_exception_1_0
    , PS_or_PDF_font_exception_20170817
    , Qt_GPL_exception_1_0
    , Qt_LGPL_exception_1_1
    ]
    ++ bulkOfLicenses
licenseExceptionIdList LicenseListVersion_3_6 =
    [ GPL_CC_1_0
    , LLVM_exception
    , OCaml_LGPL_linking_exception
    , OpenJDK_assembly_exception_1_0
    , PS_or_PDF_font_exception_20170817
    , Qt_GPL_exception_1_0
    , Qt_LGPL_exception_1_1
    , Swift_exception
    , Universal_FOSS_exception_1_0
    ]
    ++ bulkOfLicenses
licenseExceptionIdList LicenseListVersion_3_9 =
    [ GPL_3_0_linking_exception
    , GPL_3_0_linking_source_exception
    , GPL_CC_1_0
    , LGPL_3_0_linking_exception
    , LLVM_exception
    , OCaml_LGPL_linking_exception
    , OpenJDK_assembly_exception_1_0
    , PS_or_PDF_font_exception_20170817
    , Qt_GPL_exception_1_0
    , Qt_LGPL_exception_1_1
    , SHL_2_0
    , SHL_2_1
    , Swift_exception
    , Universal_FOSS_exception_1_0
    ]
    ++ bulkOfLicenses
licenseExceptionIdList LicenseListVersion_3_10 =
    [ GPL_3_0_linking_exception
    , GPL_3_0_linking_source_exception
    , GPL_CC_1_0
    , LGPL_3_0_linking_exception
    , LLVM_exception
    , OCaml_LGPL_linking_exception
    , OpenJDK_assembly_exception_1_0
    , PS_or_PDF_font_exception_20170817
    , Qt_GPL_exception_1_0
    , Qt_LGPL_exception_1_1
    , SHL_2_0
    , SHL_2_1
    , Swift_exception
    , Universal_FOSS_exception_1_0
    ]
    ++ bulkOfLicenses

-- | Create a 'LicenseExceptionId' from a 'String'.
mkLicenseExceptionId :: LicenseListVersion -> String -> Maybe LicenseExceptionId
mkLicenseExceptionId LicenseListVersion_3_0  s = Map.lookup s stringLookup_3_0
mkLicenseExceptionId LicenseListVersion_3_2  s = Map.lookup s stringLookup_3_2
mkLicenseExceptionId LicenseListVersion_3_6  s = Map.lookup s stringLookup_3_6
mkLicenseExceptionId LicenseListVersion_3_9  s = Map.lookup s stringLookup_3_9
mkLicenseExceptionId LicenseListVersion_3_10 s = Map.lookup s stringLookup_3_10

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

--  | License exceptions in all SPDX License lists
bulkOfLicenses :: [LicenseExceptionId]
bulkOfLicenses =
    [ DS389_exception
    , Autoconf_exception_2_0
    , Autoconf_exception_3_0
    , Bison_exception_2_2
    , Bootloader_exception
    , Classpath_exception_2_0
    , CLISP_exception_2_0
    , DigiRule_FOSS_exception
    , ECos_exception_2_0
    , Fawkes_Runtime_exception
    , FLTK_exception
    , Font_exception_2_0
    , Freertos_exception_2_0
    , GCC_exception_2_0
    , GCC_exception_3_1
    , Gnu_javamail_exception
    , I2p_gpl_java_exception
    , Libtool_exception
    , Linux_syscall_note
    , LZMA_exception
    , Mif_exception
    , OCCT_exception_1_0
    , Openvpn_openssl_exception
    , Qwt_exception_1_0
    , U_boot_exception_2_0
    , WxWindows_exception_3_1
    ]
