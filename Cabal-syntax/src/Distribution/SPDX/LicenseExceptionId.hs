-- This file is generated. See Makefile's spdx rule
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
    = DS389_exception -- ^ @389-exception@, 389 Directory Server Exception
    | Asterisk_exception -- ^ @Asterisk-exception@, Asterisk exception, SPDX License List 3.23
    | Autoconf_exception_2_0 -- ^ @Autoconf-exception-2.0@, Autoconf exception 2.0
    | Autoconf_exception_3_0 -- ^ @Autoconf-exception-3.0@, Autoconf exception 3.0
    | Autoconf_exception_generic_3_0 -- ^ @Autoconf-exception-generic-3.0@, Autoconf generic exception for GPL-3.0, SPDX License List 3.23
    | Autoconf_exception_generic -- ^ @Autoconf-exception-generic@, Autoconf generic exception, SPDX License List 3.23
    | Autoconf_exception_macro -- ^ @Autoconf-exception-macro@, Autoconf macro exception, SPDX License List 3.23
    | Bison_exception_1_24 -- ^ @Bison-exception-1.24@, Bison exception 1.24, SPDX License List 3.23
    | Bison_exception_2_2 -- ^ @Bison-exception-2.2@, Bison exception 2.2
    | Bootloader_exception -- ^ @Bootloader-exception@, Bootloader Distribution Exception
    | Classpath_exception_2_0 -- ^ @Classpath-exception-2.0@, Classpath exception 2.0
    | CLISP_exception_2_0 -- ^ @CLISP-exception-2.0@, CLISP exception 2.0
    | Cryptsetup_OpenSSL_exception -- ^ @cryptsetup-OpenSSL-exception@, cryptsetup OpenSSL exception, SPDX License List 3.23
    | DigiRule_FOSS_exception -- ^ @DigiRule-FOSS-exception@, DigiRule FOSS License Exception
    | ECos_exception_2_0 -- ^ @eCos-exception-2.0@, eCos exception 2.0
    | Fawkes_Runtime_exception -- ^ @Fawkes-Runtime-exception@, Fawkes Runtime Exception
    | FLTK_exception -- ^ @FLTK-exception@, FLTK exception
    | Fmt_exception -- ^ @fmt-exception@, fmt exception, SPDX License List 3.23
    | Font_exception_2_0 -- ^ @Font-exception-2.0@, Font exception 2.0
    | Freertos_exception_2_0 -- ^ @freertos-exception-2.0@, FreeRTOS Exception 2.0
    | GCC_exception_2_0_note -- ^ @GCC-exception-2.0-note@, GCC    Runtime Library exception 2.0 - note variant, SPDX License List 3.23
    | GCC_exception_2_0 -- ^ @GCC-exception-2.0@, GCC Runtime Library exception 2.0
    | GCC_exception_3_1 -- ^ @GCC-exception-3.1@, GCC Runtime Library exception 3.1
    | Gmsh_exception -- ^ @Gmsh-exception@, Gmsh exception>, SPDX License List 3.23
    | GNAT_exception -- ^ @GNAT-exception@, GNAT exception, SPDX License List 3.23
    | GNOME_examples_exception -- ^ @GNOME-examples-exception@, GNOME examples exception, SPDX License List 3.23
    | GNU_compiler_exception -- ^ @GNU-compiler-exception@, GNU Compiler Exception, SPDX License List 3.23
    | Gnu_javamail_exception -- ^ @gnu-javamail-exception@, GNU JavaMail exception
    | GPL_3_0_interface_exception -- ^ @GPL-3.0-interface-exception@, GPL-3.0 Interface Exception, SPDX License List 3.23
    | GPL_3_0_linking_exception -- ^ @GPL-3.0-linking-exception@, GPL-3.0 Linking Exception, SPDX License List 3.9, SPDX License List 3.10, SPDX License List 3.16, SPDX License List 3.23
    | GPL_3_0_linking_source_exception -- ^ @GPL-3.0-linking-source-exception@, GPL-3.0 Linking Exception (with Corresponding Source), SPDX License List 3.9, SPDX License List 3.10, SPDX License List 3.16, SPDX License List 3.23
    | GPL_CC_1_0 -- ^ @GPL-CC-1.0@, GPL Cooperation Commitment 1.0, SPDX License List 3.6, SPDX License List 3.9, SPDX License List 3.10, SPDX License List 3.16, SPDX License List 3.23
    | GStreamer_exception_2005 -- ^ @GStreamer-exception-2005@, GStreamer Exception (2005), SPDX License List 3.23
    | GStreamer_exception_2008 -- ^ @GStreamer-exception-2008@, GStreamer Exception (2008), SPDX License List 3.23
    | I2p_gpl_java_exception -- ^ @i2p-gpl-java-exception@, i2p GPL+Java Exception
    | KiCad_libraries_exception -- ^ @KiCad-libraries-exception@, KiCad Libraries Exception, SPDX License List 3.23
    | LGPL_3_0_linking_exception -- ^ @LGPL-3.0-linking-exception@, LGPL-3.0 Linking Exception, SPDX License List 3.9, SPDX License List 3.10, SPDX License List 3.16, SPDX License List 3.23
    | Libpri_OpenH323_exception -- ^ @libpri-OpenH323-exception@, libpri OpenH323 exception, SPDX License List 3.23
    | Libtool_exception -- ^ @Libtool-exception@, Libtool Exception
    | Linux_syscall_note -- ^ @Linux-syscall-note@, Linux Syscall Note
    | LLGPL -- ^ @LLGPL@, LLGPL Preamble, SPDX License List 3.23
    | LLVM_exception -- ^ @LLVM-exception@, LLVM Exception, SPDX License List 3.2, SPDX License List 3.6, SPDX License List 3.9, SPDX License List 3.10, SPDX License List 3.16, SPDX License List 3.23
    | LZMA_exception -- ^ @LZMA-exception@, LZMA exception
    | Mif_exception -- ^ @mif-exception@, Macros and Inline Functions Exception
    | Nokia_Qt_exception_1_1 -- ^ @Nokia-Qt-exception-1.1@, Nokia Qt LGPL exception 1.1, SPDX License List 3.0, SPDX License List 3.2
    | OCaml_LGPL_linking_exception -- ^ @OCaml-LGPL-linking-exception@, OCaml LGPL Linking Exception, SPDX License List 3.6, SPDX License List 3.9, SPDX License List 3.10, SPDX License List 3.16, SPDX License List 3.23
    | OCCT_exception_1_0 -- ^ @OCCT-exception-1.0@, Open CASCADE Exception 1.0
    | OpenJDK_assembly_exception_1_0 -- ^ @OpenJDK-assembly-exception-1.0@, OpenJDK Assembly exception 1.0, SPDX License List 3.2, SPDX License List 3.6, SPDX License List 3.9, SPDX License List 3.10, SPDX License List 3.16, SPDX License List 3.23
    | Openvpn_openssl_exception -- ^ @openvpn-openssl-exception@, OpenVPN OpenSSL Exception
    | PS_or_PDF_font_exception_20170817 -- ^ @PS-or-PDF-font-exception-20170817@, PS/PDF font exception (2017-08-17), SPDX License List 3.2, SPDX License List 3.6, SPDX License List 3.9, SPDX License List 3.10, SPDX License List 3.16, SPDX License List 3.23
    | QPL_1_0_INRIA_2004_exception -- ^ @QPL-1.0-INRIA-2004-exception@, INRIA QPL 1.0 2004 variant exception, SPDX License List 3.23
    | Qt_GPL_exception_1_0 -- ^ @Qt-GPL-exception-1.0@, Qt GPL exception 1.0, SPDX License List 3.2, SPDX License List 3.6, SPDX License List 3.9, SPDX License List 3.10, SPDX License List 3.16, SPDX License List 3.23
    | Qt_LGPL_exception_1_1 -- ^ @Qt-LGPL-exception-1.1@, Qt LGPL exception 1.1, SPDX License List 3.2, SPDX License List 3.6, SPDX License List 3.9, SPDX License List 3.10, SPDX License List 3.16, SPDX License List 3.23
    | Qwt_exception_1_0 -- ^ @Qwt-exception-1.0@, Qwt exception 1.0
    | SANE_exception -- ^ @SANE-exception@, SANE Exception, SPDX License List 3.23
    | SHL_2_0 -- ^ @SHL-2.0@, Solderpad Hardware License v2.0, SPDX License List 3.9, SPDX License List 3.10, SPDX License List 3.16, SPDX License List 3.23
    | SHL_2_1 -- ^ @SHL-2.1@, Solderpad Hardware License v2.1, SPDX License List 3.9, SPDX License List 3.10, SPDX License List 3.16, SPDX License List 3.23
    | Stunnel_exception -- ^ @stunnel-exception@, stunnel Exception, SPDX License List 3.23
    | SWI_exception -- ^ @SWI-exception@, SWI exception, SPDX License List 3.23
    | Swift_exception -- ^ @Swift-exception@, Swift Exception, SPDX License List 3.6, SPDX License List 3.9, SPDX License List 3.10, SPDX License List 3.16, SPDX License List 3.23
    | Texinfo_exception -- ^ @Texinfo-exception@, Texinfo exception, SPDX License List 3.23
    | U_boot_exception_2_0 -- ^ @u-boot-exception-2.0@, U-Boot exception 2.0
    | UBDL_exception -- ^ @UBDL-exception@, Unmodified Binary Distribution exception, SPDX License List 3.23
    | Universal_FOSS_exception_1_0 -- ^ @Universal-FOSS-exception-1.0@, Universal FOSS Exception, Version 1.0, SPDX License List 3.6, SPDX License List 3.9, SPDX License List 3.10, SPDX License List 3.16, SPDX License List 3.23
    | Vsftpd_openssl_exception -- ^ @vsftpd-openssl-exception@, vsftpd OpenSSL exception, SPDX License List 3.23
    | WxWindows_exception_3_1 -- ^ @WxWindows-exception-3.1@, WxWindows Library Exception 3.1
    | X11vnc_openssl_exception -- ^ @x11vnc-openssl-exception@, x11vnc OpenSSL Exception, SPDX License List 3.23
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
licenseExceptionId Asterisk_exception = "Asterisk-exception"
licenseExceptionId Autoconf_exception_2_0 = "Autoconf-exception-2.0"
licenseExceptionId Autoconf_exception_3_0 = "Autoconf-exception-3.0"
licenseExceptionId Autoconf_exception_generic_3_0 = "Autoconf-exception-generic-3.0"
licenseExceptionId Autoconf_exception_generic = "Autoconf-exception-generic"
licenseExceptionId Autoconf_exception_macro = "Autoconf-exception-macro"
licenseExceptionId Bison_exception_1_24 = "Bison-exception-1.24"
licenseExceptionId Bison_exception_2_2 = "Bison-exception-2.2"
licenseExceptionId Bootloader_exception = "Bootloader-exception"
licenseExceptionId Classpath_exception_2_0 = "Classpath-exception-2.0"
licenseExceptionId CLISP_exception_2_0 = "CLISP-exception-2.0"
licenseExceptionId Cryptsetup_OpenSSL_exception = "cryptsetup-OpenSSL-exception"
licenseExceptionId DigiRule_FOSS_exception = "DigiRule-FOSS-exception"
licenseExceptionId ECos_exception_2_0 = "eCos-exception-2.0"
licenseExceptionId Fawkes_Runtime_exception = "Fawkes-Runtime-exception"
licenseExceptionId FLTK_exception = "FLTK-exception"
licenseExceptionId Fmt_exception = "fmt-exception"
licenseExceptionId Font_exception_2_0 = "Font-exception-2.0"
licenseExceptionId Freertos_exception_2_0 = "freertos-exception-2.0"
licenseExceptionId GCC_exception_2_0_note = "GCC-exception-2.0-note"
licenseExceptionId GCC_exception_2_0 = "GCC-exception-2.0"
licenseExceptionId GCC_exception_3_1 = "GCC-exception-3.1"
licenseExceptionId Gmsh_exception = "Gmsh-exception"
licenseExceptionId GNAT_exception = "GNAT-exception"
licenseExceptionId GNOME_examples_exception = "GNOME-examples-exception"
licenseExceptionId GNU_compiler_exception = "GNU-compiler-exception"
licenseExceptionId Gnu_javamail_exception = "gnu-javamail-exception"
licenseExceptionId GPL_3_0_interface_exception = "GPL-3.0-interface-exception"
licenseExceptionId GPL_3_0_linking_exception = "GPL-3.0-linking-exception"
licenseExceptionId GPL_3_0_linking_source_exception = "GPL-3.0-linking-source-exception"
licenseExceptionId GPL_CC_1_0 = "GPL-CC-1.0"
licenseExceptionId GStreamer_exception_2005 = "GStreamer-exception-2005"
licenseExceptionId GStreamer_exception_2008 = "GStreamer-exception-2008"
licenseExceptionId I2p_gpl_java_exception = "i2p-gpl-java-exception"
licenseExceptionId KiCad_libraries_exception = "KiCad-libraries-exception"
licenseExceptionId LGPL_3_0_linking_exception = "LGPL-3.0-linking-exception"
licenseExceptionId Libpri_OpenH323_exception = "libpri-OpenH323-exception"
licenseExceptionId Libtool_exception = "Libtool-exception"
licenseExceptionId Linux_syscall_note = "Linux-syscall-note"
licenseExceptionId LLGPL = "LLGPL"
licenseExceptionId LLVM_exception = "LLVM-exception"
licenseExceptionId LZMA_exception = "LZMA-exception"
licenseExceptionId Mif_exception = "mif-exception"
licenseExceptionId Nokia_Qt_exception_1_1 = "Nokia-Qt-exception-1.1"
licenseExceptionId OCaml_LGPL_linking_exception = "OCaml-LGPL-linking-exception"
licenseExceptionId OCCT_exception_1_0 = "OCCT-exception-1.0"
licenseExceptionId OpenJDK_assembly_exception_1_0 = "OpenJDK-assembly-exception-1.0"
licenseExceptionId Openvpn_openssl_exception = "openvpn-openssl-exception"
licenseExceptionId PS_or_PDF_font_exception_20170817 = "PS-or-PDF-font-exception-20170817"
licenseExceptionId QPL_1_0_INRIA_2004_exception = "QPL-1.0-INRIA-2004-exception"
licenseExceptionId Qt_GPL_exception_1_0 = "Qt-GPL-exception-1.0"
licenseExceptionId Qt_LGPL_exception_1_1 = "Qt-LGPL-exception-1.1"
licenseExceptionId Qwt_exception_1_0 = "Qwt-exception-1.0"
licenseExceptionId SANE_exception = "SANE-exception"
licenseExceptionId SHL_2_0 = "SHL-2.0"
licenseExceptionId SHL_2_1 = "SHL-2.1"
licenseExceptionId Stunnel_exception = "stunnel-exception"
licenseExceptionId SWI_exception = "SWI-exception"
licenseExceptionId Swift_exception = "Swift-exception"
licenseExceptionId Texinfo_exception = "Texinfo-exception"
licenseExceptionId U_boot_exception_2_0 = "u-boot-exception-2.0"
licenseExceptionId UBDL_exception = "UBDL-exception"
licenseExceptionId Universal_FOSS_exception_1_0 = "Universal-FOSS-exception-1.0"
licenseExceptionId Vsftpd_openssl_exception = "vsftpd-openssl-exception"
licenseExceptionId WxWindows_exception_3_1 = "WxWindows-exception-3.1"
licenseExceptionId X11vnc_openssl_exception = "x11vnc-openssl-exception"

-- | License name, e.g. @"GNU General Public License v2.0 only"@
licenseExceptionName :: LicenseExceptionId -> String
licenseExceptionName DS389_exception = "389 Directory Server Exception"
licenseExceptionName Asterisk_exception = "Asterisk exception"
licenseExceptionName Autoconf_exception_2_0 = "Autoconf exception 2.0"
licenseExceptionName Autoconf_exception_3_0 = "Autoconf exception 3.0"
licenseExceptionName Autoconf_exception_generic_3_0 = "Autoconf generic exception for GPL-3.0"
licenseExceptionName Autoconf_exception_generic = "Autoconf generic exception"
licenseExceptionName Autoconf_exception_macro = "Autoconf macro exception"
licenseExceptionName Bison_exception_1_24 = "Bison exception 1.24"
licenseExceptionName Bison_exception_2_2 = "Bison exception 2.2"
licenseExceptionName Bootloader_exception = "Bootloader Distribution Exception"
licenseExceptionName Classpath_exception_2_0 = "Classpath exception 2.0"
licenseExceptionName CLISP_exception_2_0 = "CLISP exception 2.0"
licenseExceptionName Cryptsetup_OpenSSL_exception = "cryptsetup OpenSSL exception"
licenseExceptionName DigiRule_FOSS_exception = "DigiRule FOSS License Exception"
licenseExceptionName ECos_exception_2_0 = "eCos exception 2.0"
licenseExceptionName Fawkes_Runtime_exception = "Fawkes Runtime Exception"
licenseExceptionName FLTK_exception = "FLTK exception"
licenseExceptionName Fmt_exception = "fmt exception"
licenseExceptionName Font_exception_2_0 = "Font exception 2.0"
licenseExceptionName Freertos_exception_2_0 = "FreeRTOS Exception 2.0"
licenseExceptionName GCC_exception_2_0_note = "GCC    Runtime Library exception 2.0 - note variant"
licenseExceptionName GCC_exception_2_0 = "GCC Runtime Library exception 2.0"
licenseExceptionName GCC_exception_3_1 = "GCC Runtime Library exception 3.1"
licenseExceptionName Gmsh_exception = "Gmsh exception>"
licenseExceptionName GNAT_exception = "GNAT exception"
licenseExceptionName GNOME_examples_exception = "GNOME examples exception"
licenseExceptionName GNU_compiler_exception = "GNU Compiler Exception"
licenseExceptionName Gnu_javamail_exception = "GNU JavaMail exception"
licenseExceptionName GPL_3_0_interface_exception = "GPL-3.0 Interface Exception"
licenseExceptionName GPL_3_0_linking_exception = "GPL-3.0 Linking Exception"
licenseExceptionName GPL_3_0_linking_source_exception = "GPL-3.0 Linking Exception (with Corresponding Source)"
licenseExceptionName GPL_CC_1_0 = "GPL Cooperation Commitment 1.0"
licenseExceptionName GStreamer_exception_2005 = "GStreamer Exception (2005)"
licenseExceptionName GStreamer_exception_2008 = "GStreamer Exception (2008)"
licenseExceptionName I2p_gpl_java_exception = "i2p GPL+Java Exception"
licenseExceptionName KiCad_libraries_exception = "KiCad Libraries Exception"
licenseExceptionName LGPL_3_0_linking_exception = "LGPL-3.0 Linking Exception"
licenseExceptionName Libpri_OpenH323_exception = "libpri OpenH323 exception"
licenseExceptionName Libtool_exception = "Libtool Exception"
licenseExceptionName Linux_syscall_note = "Linux Syscall Note"
licenseExceptionName LLGPL = "LLGPL Preamble"
licenseExceptionName LLVM_exception = "LLVM Exception"
licenseExceptionName LZMA_exception = "LZMA exception"
licenseExceptionName Mif_exception = "Macros and Inline Functions Exception"
licenseExceptionName Nokia_Qt_exception_1_1 = "Nokia Qt LGPL exception 1.1"
licenseExceptionName OCaml_LGPL_linking_exception = "OCaml LGPL Linking Exception"
licenseExceptionName OCCT_exception_1_0 = "Open CASCADE Exception 1.0"
licenseExceptionName OpenJDK_assembly_exception_1_0 = "OpenJDK Assembly exception 1.0"
licenseExceptionName Openvpn_openssl_exception = "OpenVPN OpenSSL Exception"
licenseExceptionName PS_or_PDF_font_exception_20170817 = "PS/PDF font exception (2017-08-17)"
licenseExceptionName QPL_1_0_INRIA_2004_exception = "INRIA QPL 1.0 2004 variant exception"
licenseExceptionName Qt_GPL_exception_1_0 = "Qt GPL exception 1.0"
licenseExceptionName Qt_LGPL_exception_1_1 = "Qt LGPL exception 1.1"
licenseExceptionName Qwt_exception_1_0 = "Qwt exception 1.0"
licenseExceptionName SANE_exception = "SANE Exception"
licenseExceptionName SHL_2_0 = "Solderpad Hardware License v2.0"
licenseExceptionName SHL_2_1 = "Solderpad Hardware License v2.1"
licenseExceptionName Stunnel_exception = "stunnel Exception"
licenseExceptionName SWI_exception = "SWI exception"
licenseExceptionName Swift_exception = "Swift Exception"
licenseExceptionName Texinfo_exception = "Texinfo exception"
licenseExceptionName U_boot_exception_2_0 = "U-Boot exception 2.0"
licenseExceptionName UBDL_exception = "Unmodified Binary Distribution exception"
licenseExceptionName Universal_FOSS_exception_1_0 = "Universal FOSS Exception, Version 1.0"
licenseExceptionName Vsftpd_openssl_exception = "vsftpd OpenSSL exception"
licenseExceptionName WxWindows_exception_3_1 = "WxWindows Library Exception 3.1"
licenseExceptionName X11vnc_openssl_exception = "x11vnc OpenSSL Exception"

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
licenseExceptionIdList LicenseListVersion_3_16 =
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
licenseExceptionIdList LicenseListVersion_3_23 =
    [ Asterisk_exception
    , Autoconf_exception_generic_3_0
    , Autoconf_exception_generic
    , Autoconf_exception_macro
    , Bison_exception_1_24
    , Cryptsetup_OpenSSL_exception
    , Fmt_exception
    , GCC_exception_2_0_note
    , Gmsh_exception
    , GNAT_exception
    , GNOME_examples_exception
    , GNU_compiler_exception
    , GPL_3_0_interface_exception
    , GPL_3_0_linking_exception
    , GPL_3_0_linking_source_exception
    , GPL_CC_1_0
    , GStreamer_exception_2005
    , GStreamer_exception_2008
    , KiCad_libraries_exception
    , LGPL_3_0_linking_exception
    , Libpri_OpenH323_exception
    , LLGPL
    , LLVM_exception
    , OCaml_LGPL_linking_exception
    , OpenJDK_assembly_exception_1_0
    , PS_or_PDF_font_exception_20170817
    , QPL_1_0_INRIA_2004_exception
    , Qt_GPL_exception_1_0
    , Qt_LGPL_exception_1_1
    , SANE_exception
    , SHL_2_0
    , SHL_2_1
    , Stunnel_exception
    , SWI_exception
    , Swift_exception
    , Texinfo_exception
    , UBDL_exception
    , Universal_FOSS_exception_1_0
    , Vsftpd_openssl_exception
    , X11vnc_openssl_exception
    ]
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
