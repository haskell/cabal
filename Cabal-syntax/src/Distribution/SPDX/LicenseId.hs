-- This file is generated. See Makefile's spdx rule
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

-- | SPDX License identifiers list v3.23
data LicenseId
    = NullBSD -- ^ @0BSD@, BSD Zero Clause License
    | AAL -- ^ @AAL@, Attribution Assurance License
    | Abstyles -- ^ @Abstyles@, Abstyles License
    | AdaCore_doc -- ^ @AdaCore-doc@, AdaCore Doc License, SPDX License List 3.23
    | Adobe_2006 -- ^ @Adobe-2006@, Adobe Systems Incorporated Source Code License Agreement
    | Adobe_Display_PostScript -- ^ @Adobe-Display-PostScript@, Adobe Display PostScript License, SPDX License List 3.23
    | Adobe_Glyph -- ^ @Adobe-Glyph@, Adobe Glyph List License
    | Adobe_Utopia -- ^ @Adobe-Utopia@, Adobe Utopia Font License, SPDX License List 3.23
    | ADSL -- ^ @ADSL@, Amazon Digital Services License
    | AFL_1_1 -- ^ @AFL-1.1@, Academic Free License v1.1
    | AFL_1_2 -- ^ @AFL-1.2@, Academic Free License v1.2
    | AFL_2_0 -- ^ @AFL-2.0@, Academic Free License v2.0
    | AFL_2_1 -- ^ @AFL-2.1@, Academic Free License v2.1
    | AFL_3_0 -- ^ @AFL-3.0@, Academic Free License v3.0
    | Afmparse -- ^ @Afmparse@, Afmparse License
    | AGPL_1_0 -- ^ @AGPL-1.0@, Affero General Public License v1.0, SPDX License List 3.0
    | AGPL_1_0_only -- ^ @AGPL-1.0-only@, Affero General Public License v1.0 only, SPDX License List 3.2, SPDX License List 3.6, SPDX License List 3.9, SPDX License List 3.10, SPDX License List 3.16, SPDX License List 3.23
    | AGPL_1_0_or_later -- ^ @AGPL-1.0-or-later@, Affero General Public License v1.0 or later, SPDX License List 3.2, SPDX License List 3.6, SPDX License List 3.9, SPDX License List 3.10, SPDX License List 3.16, SPDX License List 3.23
    | AGPL_3_0_only -- ^ @AGPL-3.0-only@, GNU Affero General Public License v3.0 only
    | AGPL_3_0_or_later -- ^ @AGPL-3.0-or-later@, GNU Affero General Public License v3.0 or later
    | Aladdin -- ^ @Aladdin@, Aladdin Free Public License
    | AMDPLPA -- ^ @AMDPLPA@, AMD's plpa_map.c License
    | AML_glslang -- ^ @AML-glslang@, AML glslang variant License, SPDX License List 3.23
    | AML -- ^ @AML@, Apple MIT License
    | AMPAS -- ^ @AMPAS@, Academy of Motion Picture Arts and Sciences BSD
    | ANTLR_PD_fallback -- ^ @ANTLR-PD-fallback@, ANTLR Software Rights Notice with license fallback, SPDX License List 3.16, SPDX License List 3.23
    | ANTLR_PD -- ^ @ANTLR-PD@, ANTLR Software Rights Notice
    | Apache_1_0 -- ^ @Apache-1.0@, Apache License 1.0
    | Apache_1_1 -- ^ @Apache-1.1@, Apache License 1.1
    | Apache_2_0 -- ^ @Apache-2.0@, Apache License 2.0
    | APAFML -- ^ @APAFML@, Adobe Postscript AFM License
    | APL_1_0 -- ^ @APL-1.0@, Adaptive Public License 1.0
    | App_s2p -- ^ @App-s2p@, App::s2p License, SPDX License List 3.16, SPDX License List 3.23
    | APSL_1_0 -- ^ @APSL-1.0@, Apple Public Source License 1.0
    | APSL_1_1 -- ^ @APSL-1.1@, Apple Public Source License 1.1
    | APSL_1_2 -- ^ @APSL-1.2@, Apple Public Source License 1.2
    | APSL_2_0 -- ^ @APSL-2.0@, Apple Public Source License 2.0
    | Arphic_1999 -- ^ @Arphic-1999@, Arphic Public License, SPDX License List 3.23
    | Artistic_1_0_cl8 -- ^ @Artistic-1.0-cl8@, Artistic License 1.0 w/clause 8
    | Artistic_1_0_Perl -- ^ @Artistic-1.0-Perl@, Artistic License 1.0 (Perl)
    | Artistic_1_0 -- ^ @Artistic-1.0@, Artistic License 1.0
    | Artistic_2_0 -- ^ @Artistic-2.0@, Artistic License 2.0
    | ASWF_Digital_Assets_1_0 -- ^ @ASWF-Digital-Assets-1.0@, ASWF Digital Assets License version 1.0, SPDX License List 3.23
    | ASWF_Digital_Assets_1_1 -- ^ @ASWF-Digital-Assets-1.1@, ASWF Digital Assets License 1.1, SPDX License List 3.23
    | Baekmuk -- ^ @Baekmuk@, Baekmuk License, SPDX License List 3.23
    | Bahyph -- ^ @Bahyph@, Bahyph License
    | Barr -- ^ @Barr@, Barr License
    | Bcrypt_Solar_Designer -- ^ @bcrypt-Solar-Designer@, bcrypt Solar Designer License, SPDX License List 3.23
    | Beerware -- ^ @Beerware@, Beerware License
    | Bitstream_Charter -- ^ @Bitstream-Charter@, Bitstream Charter Font License, SPDX License List 3.23
    | Bitstream_Vera -- ^ @Bitstream-Vera@, Bitstream Vera Font License, SPDX License List 3.23
    | BitTorrent_1_0 -- ^ @BitTorrent-1.0@, BitTorrent Open Source License v1.0
    | BitTorrent_1_1 -- ^ @BitTorrent-1.1@, BitTorrent Open Source License v1.1
    | Blessing -- ^ @blessing@, SQLite Blessing, SPDX License List 3.6, SPDX License List 3.9, SPDX License List 3.10, SPDX License List 3.16, SPDX License List 3.23
    | BlueOak_1_0_0 -- ^ @BlueOak-1.0.0@, Blue Oak Model License 1.0.0, SPDX License List 3.6, SPDX License List 3.9, SPDX License List 3.10, SPDX License List 3.16, SPDX License List 3.23
    | Boehm_GC -- ^ @Boehm-GC@, Boehm-Demers-Weiser GC License, SPDX License List 3.23
    | Borceux -- ^ @Borceux@, Borceux license
    | Brian_Gladman_2_Clause -- ^ @Brian-Gladman-2-Clause@, Brian Gladman 2-Clause License, SPDX License List 3.23
    | Brian_Gladman_3_Clause -- ^ @Brian-Gladman-3-Clause@, Brian Gladman 3-Clause License, SPDX License List 3.23
    | BSD_1_Clause -- ^ @BSD-1-Clause@, BSD 1-Clause License
    | BSD_2_Clause_FreeBSD -- ^ @BSD-2-Clause-FreeBSD@, BSD 2-Clause FreeBSD License, SPDX License List 3.0, SPDX License List 3.2, SPDX License List 3.6, SPDX License List 3.9
    | BSD_2_Clause_NetBSD -- ^ @BSD-2-Clause-NetBSD@, BSD 2-Clause NetBSD License, SPDX License List 3.0, SPDX License List 3.2, SPDX License List 3.6
    | BSD_2_Clause_Darwin -- ^ @BSD-2-Clause-Darwin@, BSD 2-Clause - Ian Darwin variant, SPDX License List 3.23
    | BSD_2_Clause_Patent -- ^ @BSD-2-Clause-Patent@, BSD-2-Clause Plus Patent License
    | BSD_2_Clause_Views -- ^ @BSD-2-Clause-Views@, BSD 2-Clause with views sentence, SPDX License List 3.10, SPDX License List 3.16, SPDX License List 3.23
    | BSD_2_Clause -- ^ @BSD-2-Clause@, BSD 2-Clause "Simplified" License
    | BSD_3_Clause_acpica -- ^ @BSD-3-Clause-acpica@, BSD 3-Clause acpica variant, SPDX License List 3.23
    | BSD_3_Clause_Attribution -- ^ @BSD-3-Clause-Attribution@, BSD with attribution
    | BSD_3_Clause_Clear -- ^ @BSD-3-Clause-Clear@, BSD 3-Clause Clear License
    | BSD_3_Clause_flex -- ^ @BSD-3-Clause-flex@, BSD 3-Clause Flex variant, SPDX License List 3.23
    | BSD_3_Clause_HP -- ^ @BSD-3-Clause-HP@, Hewlett-Packard BSD variant license, SPDX License List 3.23
    | BSD_3_Clause_LBNL -- ^ @BSD-3-Clause-LBNL@, Lawrence Berkeley National Labs BSD variant license
    | BSD_3_Clause_Modification -- ^ @BSD-3-Clause-Modification@, BSD 3-Clause Modification, SPDX License List 3.16, SPDX License List 3.23
    | BSD_3_Clause_No_Military_License -- ^ @BSD-3-Clause-No-Military-License@, BSD 3-Clause No Military License, SPDX License List 3.16, SPDX License List 3.23
    | BSD_3_Clause_No_Nuclear_License_2014 -- ^ @BSD-3-Clause-No-Nuclear-License-2014@, BSD 3-Clause No Nuclear License 2014
    | BSD_3_Clause_No_Nuclear_License -- ^ @BSD-3-Clause-No-Nuclear-License@, BSD 3-Clause No Nuclear License
    | BSD_3_Clause_No_Nuclear_Warranty -- ^ @BSD-3-Clause-No-Nuclear-Warranty@, BSD 3-Clause No Nuclear Warranty
    | BSD_3_Clause_Open_MPI -- ^ @BSD-3-Clause-Open-MPI@, BSD 3-Clause Open MPI variant, SPDX License List 3.6, SPDX License List 3.9, SPDX License List 3.10, SPDX License List 3.16, SPDX License List 3.23
    | BSD_3_Clause_Sun -- ^ @BSD-3-Clause-Sun@, BSD 3-Clause Sun Microsystems, SPDX License List 3.23
    | BSD_3_Clause -- ^ @BSD-3-Clause@, BSD 3-Clause "New" or "Revised" License
    | BSD_4_Clause_Shortened -- ^ @BSD-4-Clause-Shortened@, BSD 4 Clause Shortened, SPDX License List 3.16, SPDX License List 3.23
    | BSD_4_Clause_UC -- ^ @BSD-4-Clause-UC@, BSD-4-Clause (University of California-Specific)
    | BSD_4_Clause -- ^ @BSD-4-Clause@, BSD 4-Clause "Original" or "Old" License
    | BSD_4_3RENO -- ^ @BSD-4.3RENO@, BSD 4.3 RENO License, SPDX License List 3.23
    | BSD_4_3TAHOE -- ^ @BSD-4.3TAHOE@, BSD 4.3 TAHOE License, SPDX License List 3.23
    | BSD_Advertising_Acknowledgement -- ^ @BSD-Advertising-Acknowledgement@, BSD Advertising Acknowledgement License, SPDX License List 3.23
    | BSD_Attribution_HPND_disclaimer -- ^ @BSD-Attribution-HPND-disclaimer@, BSD with Attribution and HPND disclaimer, SPDX License List 3.23
    | BSD_Inferno_Nettverk -- ^ @BSD-Inferno-Nettverk@, BSD-Inferno-Nettverk, SPDX License List 3.23
    | BSD_Protection -- ^ @BSD-Protection@, BSD Protection License
    | BSD_Source_beginning_file -- ^ @BSD-Source-beginning-file@, BSD Source Code Attribution - beginning of file variant, SPDX License List 3.23
    | BSD_Source_Code -- ^ @BSD-Source-Code@, BSD Source Code Attribution
    | BSD_Systemics_W3Works -- ^ @BSD-Systemics-W3Works@, Systemics W3Works BSD variant license, SPDX License List 3.23
    | BSD_Systemics -- ^ @BSD-Systemics@, Systemics BSD variant license, SPDX License List 3.23
    | BSL_1_0 -- ^ @BSL-1.0@, Boost Software License 1.0
    | Bzip2_1_0_5 -- ^ @bzip2-1.0.5@, bzip2 and libbzip2 License v1.0.5, SPDX License List 3.0, SPDX License List 3.2, SPDX License List 3.6, SPDX License List 3.9, SPDX License List 3.10
    | BUSL_1_1 -- ^ @BUSL-1.1@, Business Source License 1.1, SPDX License List 3.16, SPDX License List 3.23
    | Bzip2_1_0_6 -- ^ @bzip2-1.0.6@, bzip2 and libbzip2 License v1.0.6
    | C_UDA_1_0 -- ^ @C-UDA-1.0@, Computational Use of Data Agreement v1.0, SPDX License List 3.16, SPDX License List 3.23
    | CAL_1_0_Combined_Work_Exception -- ^ @CAL-1.0-Combined-Work-Exception@, Cryptographic Autonomy License 1.0 (Combined Work Exception), SPDX License List 3.9, SPDX License List 3.10, SPDX License List 3.16, SPDX License List 3.23
    | CAL_1_0 -- ^ @CAL-1.0@, Cryptographic Autonomy License 1.0, SPDX License List 3.9, SPDX License List 3.10, SPDX License List 3.16, SPDX License List 3.23
    | Caldera_no_preamble -- ^ @Caldera-no-preamble@, Caldera License (without preamble), SPDX License List 3.23
    | Caldera -- ^ @Caldera@, Caldera License
    | CATOSL_1_1 -- ^ @CATOSL-1.1@, Computer Associates Trusted Open Source License 1.1
    | CC_BY_1_0 -- ^ @CC-BY-1.0@, Creative Commons Attribution 1.0 Generic
    | CC_BY_2_0 -- ^ @CC-BY-2.0@, Creative Commons Attribution 2.0 Generic
    | CC_BY_2_5_AU -- ^ @CC-BY-2.5-AU@, Creative Commons Attribution 2.5 Australia, SPDX License List 3.16, SPDX License List 3.23
    | CC_BY_2_5 -- ^ @CC-BY-2.5@, Creative Commons Attribution 2.5 Generic
    | CC_BY_3_0_AT -- ^ @CC-BY-3.0-AT@, Creative Commons Attribution 3.0 Austria, SPDX License List 3.10, SPDX License List 3.16, SPDX License List 3.23
    | CC_BY_3_0_AU -- ^ @CC-BY-3.0-AU@, Creative Commons Attribution 3.0 Australia, SPDX License List 3.23
    | CC_BY_3_0_DE -- ^ @CC-BY-3.0-DE@, Creative Commons Attribution 3.0 Germany, SPDX License List 3.16, SPDX License List 3.23
    | CC_BY_3_0_IGO -- ^ @CC-BY-3.0-IGO@, Creative Commons Attribution 3.0 IGO, SPDX License List 3.23
    | CC_BY_3_0_NL -- ^ @CC-BY-3.0-NL@, Creative Commons Attribution 3.0 Netherlands, SPDX License List 3.16, SPDX License List 3.23
    | CC_BY_3_0_US -- ^ @CC-BY-3.0-US@, Creative Commons Attribution 3.0 United States, SPDX License List 3.16, SPDX License List 3.23
    | CC_BY_3_0 -- ^ @CC-BY-3.0@, Creative Commons Attribution 3.0 Unported
    | CC_BY_4_0 -- ^ @CC-BY-4.0@, Creative Commons Attribution 4.0 International
    | CC_BY_NC_1_0 -- ^ @CC-BY-NC-1.0@, Creative Commons Attribution Non Commercial 1.0 Generic
    | CC_BY_NC_2_0 -- ^ @CC-BY-NC-2.0@, Creative Commons Attribution Non Commercial 2.0 Generic
    | CC_BY_NC_2_5 -- ^ @CC-BY-NC-2.5@, Creative Commons Attribution Non Commercial 2.5 Generic
    | CC_BY_NC_3_0_DE -- ^ @CC-BY-NC-3.0-DE@, Creative Commons Attribution Non Commercial 3.0 Germany, SPDX License List 3.16, SPDX License List 3.23
    | CC_BY_NC_3_0 -- ^ @CC-BY-NC-3.0@, Creative Commons Attribution Non Commercial 3.0 Unported
    | CC_BY_NC_4_0 -- ^ @CC-BY-NC-4.0@, Creative Commons Attribution Non Commercial 4.0 International
    | CC_BY_NC_ND_1_0 -- ^ @CC-BY-NC-ND-1.0@, Creative Commons Attribution Non Commercial No Derivatives 1.0 Generic
    | CC_BY_NC_ND_2_0 -- ^ @CC-BY-NC-ND-2.0@, Creative Commons Attribution Non Commercial No Derivatives 2.0 Generic
    | CC_BY_NC_ND_2_5 -- ^ @CC-BY-NC-ND-2.5@, Creative Commons Attribution Non Commercial No Derivatives 2.5 Generic
    | CC_BY_NC_ND_3_0_DE -- ^ @CC-BY-NC-ND-3.0-DE@, Creative Commons Attribution Non Commercial No Derivatives 3.0 Germany, SPDX License List 3.16, SPDX License List 3.23
    | CC_BY_NC_ND_3_0_IGO -- ^ @CC-BY-NC-ND-3.0-IGO@, Creative Commons Attribution Non Commercial No Derivatives 3.0 IGO, SPDX License List 3.10, SPDX License List 3.16, SPDX License List 3.23
    | CC_BY_NC_ND_3_0 -- ^ @CC-BY-NC-ND-3.0@, Creative Commons Attribution Non Commercial No Derivatives 3.0 Unported
    | CC_BY_NC_ND_4_0 -- ^ @CC-BY-NC-ND-4.0@, Creative Commons Attribution Non Commercial No Derivatives 4.0 International
    | CC_BY_NC_SA_1_0 -- ^ @CC-BY-NC-SA-1.0@, Creative Commons Attribution Non Commercial Share Alike 1.0 Generic
    | CC_BY_NC_SA_2_0_DE -- ^ @CC-BY-NC-SA-2.0-DE@, Creative Commons Attribution Non Commercial Share Alike 2.0 Germany, SPDX License List 3.23
    | CC_BY_NC_SA_2_0_FR -- ^ @CC-BY-NC-SA-2.0-FR@, Creative Commons Attribution-NonCommercial-ShareAlike 2.0 France, SPDX License List 3.16, SPDX License List 3.23
    | CC_BY_NC_SA_2_0_UK -- ^ @CC-BY-NC-SA-2.0-UK@, Creative Commons Attribution Non Commercial Share Alike 2.0 England and Wales, SPDX License List 3.16, SPDX License List 3.23
    | CC_BY_NC_SA_2_0 -- ^ @CC-BY-NC-SA-2.0@, Creative Commons Attribution Non Commercial Share Alike 2.0 Generic
    | CC_BY_NC_SA_2_5 -- ^ @CC-BY-NC-SA-2.5@, Creative Commons Attribution Non Commercial Share Alike 2.5 Generic
    | CC_BY_NC_SA_3_0_DE -- ^ @CC-BY-NC-SA-3.0-DE@, Creative Commons Attribution Non Commercial Share Alike 3.0 Germany, SPDX License List 3.16, SPDX License List 3.23
    | CC_BY_NC_SA_3_0_IGO -- ^ @CC-BY-NC-SA-3.0-IGO@, Creative Commons Attribution Non Commercial Share Alike 3.0 IGO, SPDX License List 3.16, SPDX License List 3.23
    | CC_BY_NC_SA_3_0 -- ^ @CC-BY-NC-SA-3.0@, Creative Commons Attribution Non Commercial Share Alike 3.0 Unported
    | CC_BY_NC_SA_4_0 -- ^ @CC-BY-NC-SA-4.0@, Creative Commons Attribution Non Commercial Share Alike 4.0 International
    | CC_BY_ND_1_0 -- ^ @CC-BY-ND-1.0@, Creative Commons Attribution No Derivatives 1.0 Generic
    | CC_BY_ND_2_0 -- ^ @CC-BY-ND-2.0@, Creative Commons Attribution No Derivatives 2.0 Generic
    | CC_BY_ND_2_5 -- ^ @CC-BY-ND-2.5@, Creative Commons Attribution No Derivatives 2.5 Generic
    | CC_BY_ND_3_0_DE -- ^ @CC-BY-ND-3.0-DE@, Creative Commons Attribution No Derivatives 3.0 Germany, SPDX License List 3.16, SPDX License List 3.23
    | CC_BY_ND_3_0 -- ^ @CC-BY-ND-3.0@, Creative Commons Attribution No Derivatives 3.0 Unported
    | CC_BY_ND_4_0 -- ^ @CC-BY-ND-4.0@, Creative Commons Attribution No Derivatives 4.0 International
    | CC_BY_SA_1_0 -- ^ @CC-BY-SA-1.0@, Creative Commons Attribution Share Alike 1.0 Generic
    | CC_BY_SA_2_0_UK -- ^ @CC-BY-SA-2.0-UK@, Creative Commons Attribution Share Alike 2.0 England and Wales, SPDX License List 3.16, SPDX License List 3.23
    | CC_BY_SA_2_0 -- ^ @CC-BY-SA-2.0@, Creative Commons Attribution Share Alike 2.0 Generic
    | CC_BY_SA_2_1_JP -- ^ @CC-BY-SA-2.1-JP@, Creative Commons Attribution Share Alike 2.1 Japan, SPDX License List 3.16, SPDX License List 3.23
    | CC_BY_SA_2_5 -- ^ @CC-BY-SA-2.5@, Creative Commons Attribution Share Alike 2.5 Generic
    | CC_BY_SA_3_0_AT -- ^ @CC-BY-SA-3.0-AT@, Creative Commons Attribution Share Alike 3.0 Austria, SPDX License List 3.10, SPDX License List 3.16, SPDX License List 3.23
    | CC_BY_SA_3_0_DE -- ^ @CC-BY-SA-3.0-DE@, Creative Commons Attribution Share Alike 3.0 Germany, SPDX License List 3.16, SPDX License List 3.23
    | CC_BY_SA_3_0_IGO -- ^ @CC-BY-SA-3.0-IGO@, Creative Commons Attribution-ShareAlike 3.0 IGO, SPDX License List 3.23
    | CC_BY_SA_3_0 -- ^ @CC-BY-SA-3.0@, Creative Commons Attribution Share Alike 3.0 Unported
    | CC_BY_SA_4_0 -- ^ @CC-BY-SA-4.0@, Creative Commons Attribution Share Alike 4.0 International
    | CC_PDDC -- ^ @CC-PDDC@, Creative Commons Public Domain Dedication and Certification, SPDX License List 3.6, SPDX License List 3.9, SPDX License List 3.10, SPDX License List 3.16, SPDX License List 3.23
    | CC0_1_0 -- ^ @CC0-1.0@, Creative Commons Zero v1.0 Universal
    | CDDL_1_0 -- ^ @CDDL-1.0@, Common Development and Distribution License 1.0
    | CDDL_1_1 -- ^ @CDDL-1.1@, Common Development and Distribution License 1.1
    | CDL_1_0 -- ^ @CDL-1.0@, Common Documentation License 1.0, SPDX License List 3.16, SPDX License List 3.23
    | CDLA_Permissive_1_0 -- ^ @CDLA-Permissive-1.0@, Community Data License Agreement Permissive 1.0
    | CDLA_Permissive_2_0 -- ^ @CDLA-Permissive-2.0@, Community Data License Agreement Permissive 2.0, SPDX License List 3.16, SPDX License List 3.23
    | CDLA_Sharing_1_0 -- ^ @CDLA-Sharing-1.0@, Community Data License Agreement Sharing 1.0
    | CECILL_1_0 -- ^ @CECILL-1.0@, CeCILL Free Software License Agreement v1.0
    | CECILL_1_1 -- ^ @CECILL-1.1@, CeCILL Free Software License Agreement v1.1
    | CECILL_2_0 -- ^ @CECILL-2.0@, CeCILL Free Software License Agreement v2.0
    | CECILL_2_1 -- ^ @CECILL-2.1@, CeCILL Free Software License Agreement v2.1
    | CECILL_B -- ^ @CECILL-B@, CeCILL-B Free Software License Agreement
    | CECILL_C -- ^ @CECILL-C@, CeCILL-C Free Software License Agreement
    | CERN_OHL_1_1 -- ^ @CERN-OHL-1.1@, CERN Open Hardware Licence v1.1, SPDX License List 3.6, SPDX License List 3.9, SPDX License List 3.10, SPDX License List 3.16, SPDX License List 3.23
    | CERN_OHL_1_2 -- ^ @CERN-OHL-1.2@, CERN Open Hardware Licence v1.2, SPDX License List 3.6, SPDX License List 3.9, SPDX License List 3.10, SPDX License List 3.16, SPDX License List 3.23
    | CERN_OHL_P_2_0 -- ^ @CERN-OHL-P-2.0@, CERN Open Hardware Licence Version 2 - Permissive, SPDX License List 3.9, SPDX License List 3.10, SPDX License List 3.16, SPDX License List 3.23
    | CERN_OHL_S_2_0 -- ^ @CERN-OHL-S-2.0@, CERN Open Hardware Licence Version 2 - Strongly Reciprocal, SPDX License List 3.9, SPDX License List 3.10, SPDX License List 3.16, SPDX License List 3.23
    | CERN_OHL_W_2_0 -- ^ @CERN-OHL-W-2.0@, CERN Open Hardware Licence Version 2 - Weakly Reciprocal, SPDX License List 3.9, SPDX License List 3.10, SPDX License List 3.16, SPDX License List 3.23
    | CFITSIO -- ^ @CFITSIO@, CFITSIO License, SPDX License List 3.23
    | Check_cvs -- ^ @check-cvs@, check-cvs License, SPDX License List 3.23
    | Checkmk -- ^ @checkmk@, Checkmk License, SPDX License List 3.23
    | ClArtistic -- ^ @ClArtistic@, Clarified Artistic License
    | Clips -- ^ @Clips@, Clips License, SPDX License List 3.23
    | CMU_Mach_nodoc -- ^ @CMU-Mach-nodoc@, CMU    Mach - no notices-in-documentation variant, SPDX License List 3.23
    | CMU_Mach -- ^ @CMU-Mach@, CMU Mach License, SPDX License List 3.23
    | CNRI_Jython -- ^ @CNRI-Jython@, CNRI Jython License
    | CNRI_Python_GPL_Compatible -- ^ @CNRI-Python-GPL-Compatible@, CNRI Python Open Source GPL Compatible License Agreement
    | CNRI_Python -- ^ @CNRI-Python@, CNRI Python License
    | COIL_1_0 -- ^ @COIL-1.0@, Copyfree Open Innovation License, SPDX License List 3.16, SPDX License List 3.23
    | Community_Spec_1_0 -- ^ @Community-Spec-1.0@, Community Specification License 1.0, SPDX License List 3.16, SPDX License List 3.23
    | Condor_1_1 -- ^ @Condor-1.1@, Condor Public License v1.1
    | Copyleft_next_0_3_0 -- ^ @copyleft-next-0.3.0@, copyleft-next 0.3.0, SPDX License List 3.6, SPDX License List 3.9, SPDX License List 3.10, SPDX License List 3.16, SPDX License List 3.23
    | Copyleft_next_0_3_1 -- ^ @copyleft-next-0.3.1@, copyleft-next 0.3.1, SPDX License List 3.6, SPDX License List 3.9, SPDX License List 3.10, SPDX License List 3.16, SPDX License List 3.23
    | Cornell_Lossless_JPEG -- ^ @Cornell-Lossless-JPEG@, Cornell Lossless JPEG License, SPDX License List 3.23
    | CPAL_1_0 -- ^ @CPAL-1.0@, Common Public Attribution License 1.0
    | CPL_1_0 -- ^ @CPL-1.0@, Common Public License 1.0
    | CPOL_1_02 -- ^ @CPOL-1.02@, Code Project Open License 1.02
    | Cronyx -- ^ @Cronyx@, Cronyx License, SPDX License List 3.23
    | Crossword -- ^ @Crossword@, Crossword License
    | CrystalStacker -- ^ @CrystalStacker@, CrystalStacker License
    | CUA_OPL_1_0 -- ^ @CUA-OPL-1.0@, CUA Office Public License v1.0
    | Cube -- ^ @Cube@, Cube License
    | Curl -- ^ @curl@, curl License
    | D_FSL_1_0 -- ^ @D-FSL-1.0@, Deutsche Freie Software Lizenz
    | DEC_3_Clause -- ^ @DEC-3-Clause@, DEC 3-Clause License, SPDX License List 3.23
    | Diffmark -- ^ @diffmark@, diffmark license
    | DL_DE_BY_2_0 -- ^ @DL-DE-BY-2.0@, Data licence Germany – attribution – version 2.0, SPDX License List 3.16, SPDX License List 3.23
    | DL_DE_ZERO_2_0 -- ^ @DL-DE-ZERO-2.0@, Data licence Germany – zero – version 2.0, SPDX License List 3.23
    | DOC -- ^ @DOC@, DOC License
    | Dotseqn -- ^ @Dotseqn@, Dotseqn License
    | DRL_1_0 -- ^ @DRL-1.0@, Detection Rule License 1.0, SPDX License List 3.16, SPDX License List 3.23
    | DRL_1_1 -- ^ @DRL-1.1@, Detection Rule License 1.1, SPDX License List 3.23
    | DSDP -- ^ @DSDP@, DSDP License
    | Dtoa -- ^ @dtoa@, David M. Gay dtoa License, SPDX License List 3.23
    | Dvipdfm -- ^ @dvipdfm@, dvipdfm License
    | ECL_1_0 -- ^ @ECL-1.0@, Educational Community License v1.0
    | ECL_2_0 -- ^ @ECL-2.0@, Educational Community License v2.0
    | EFL_1_0 -- ^ @EFL-1.0@, Eiffel Forum License v1.0
    | EFL_2_0 -- ^ @EFL-2.0@, Eiffel Forum License v2.0
    | EGenix -- ^ @eGenix@, eGenix.com Public License 1.1.0
    | Elastic_2_0 -- ^ @Elastic-2.0@, Elastic License 2.0, SPDX License List 3.16, SPDX License List 3.23
    | Entessa -- ^ @Entessa@, Entessa Public License v1.0
    | EPICS -- ^ @EPICS@, EPICS Open License, SPDX License List 3.10, SPDX License List 3.16, SPDX License List 3.23
    | EPL_1_0 -- ^ @EPL-1.0@, Eclipse Public License 1.0
    | EPL_2_0 -- ^ @EPL-2.0@, Eclipse Public License 2.0
    | ErlPL_1_1 -- ^ @ErlPL-1.1@, Erlang Public License v1.1
    | Etalab_2_0 -- ^ @etalab-2.0@, Etalab Open License 2.0, SPDX License List 3.9, SPDX License List 3.10, SPDX License List 3.16, SPDX License List 3.23
    | EUDatagrid -- ^ @EUDatagrid@, EU DataGrid Software License
    | EUPL_1_0 -- ^ @EUPL-1.0@, European Union Public License 1.0
    | EUPL_1_1 -- ^ @EUPL-1.1@, European Union Public License 1.1
    | EUPL_1_2 -- ^ @EUPL-1.2@, European Union Public License 1.2
    | Eurosym -- ^ @Eurosym@, Eurosym License
    | Fair -- ^ @Fair@, Fair License
    | FBM -- ^ @FBM@, Fuzzy Bitmap License, SPDX License List 3.23
    | FDK_AAC -- ^ @FDK-AAC@, Fraunhofer FDK AAC Codec Library, SPDX License List 3.16, SPDX License List 3.23
    | Ferguson_Twofish -- ^ @Ferguson-Twofish@, Ferguson Twofish License, SPDX License List 3.23
    | Frameworx_1_0 -- ^ @Frameworx-1.0@, Frameworx Open License 1.0
    | FreeBSD_DOC -- ^ @FreeBSD-DOC@, FreeBSD Documentation License, SPDX License List 3.16, SPDX License List 3.23
    | FreeImage -- ^ @FreeImage@, FreeImage Public License v1.0
    | FSFAP_no_warranty_disclaimer -- ^ @FSFAP-no-warranty-disclaimer@, FSF All Permissive License (without Warranty), SPDX License List 3.23
    | FSFAP -- ^ @FSFAP@, FSF All Permissive License
    | FSFULLRWD -- ^ @FSFULLRWD@, FSF Unlimited License (With License Retention and Warranty Disclaimer), SPDX License List 3.23
    | FSFULLR -- ^ @FSFULLR@, FSF Unlimited License (with License Retention)
    | FSFUL -- ^ @FSFUL@, FSF Unlimited License
    | FTL -- ^ @FTL@, Freetype Project License
    | Furuseth -- ^ @Furuseth@, Furuseth License, SPDX License List 3.23
    | Fwlw -- ^ @fwlw@, fwlw License, SPDX License List 3.23
    | GCR_docs -- ^ @GCR-docs@, Gnome GCR Documentation License, SPDX License List 3.23
    | GD -- ^ @GD@, GD License, SPDX License List 3.16, SPDX License List 3.23
    | GFDL_1_1_invariants_only -- ^ @GFDL-1.1-invariants-only@, GNU Free Documentation License v1.1 only - invariants, SPDX License List 3.10, SPDX License List 3.16, SPDX License List 3.23
    | GFDL_1_1_invariants_or_later -- ^ @GFDL-1.1-invariants-or-later@, GNU Free Documentation License v1.1 or later - invariants, SPDX License List 3.10, SPDX License List 3.16, SPDX License List 3.23
    | GFDL_1_1_no_invariants_only -- ^ @GFDL-1.1-no-invariants-only@, GNU Free Documentation License v1.1 only - no invariants, SPDX License List 3.10, SPDX License List 3.16, SPDX License List 3.23
    | GFDL_1_1_no_invariants_or_later -- ^ @GFDL-1.1-no-invariants-or-later@, GNU Free Documentation License v1.1 or later - no invariants, SPDX License List 3.10, SPDX License List 3.16, SPDX License List 3.23
    | GFDL_1_1_only -- ^ @GFDL-1.1-only@, GNU Free Documentation License v1.1 only
    | GFDL_1_1_or_later -- ^ @GFDL-1.1-or-later@, GNU Free Documentation License v1.1 or later
    | GFDL_1_2_invariants_only -- ^ @GFDL-1.2-invariants-only@, GNU Free Documentation License v1.2 only - invariants, SPDX License List 3.10, SPDX License List 3.16, SPDX License List 3.23
    | GFDL_1_2_invariants_or_later -- ^ @GFDL-1.2-invariants-or-later@, GNU Free Documentation License v1.2 or later - invariants, SPDX License List 3.10, SPDX License List 3.16, SPDX License List 3.23
    | GFDL_1_2_no_invariants_only -- ^ @GFDL-1.2-no-invariants-only@, GNU Free Documentation License v1.2 only - no invariants, SPDX License List 3.10, SPDX License List 3.16, SPDX License List 3.23
    | GFDL_1_2_no_invariants_or_later -- ^ @GFDL-1.2-no-invariants-or-later@, GNU Free Documentation License v1.2 or later - no invariants, SPDX License List 3.10, SPDX License List 3.16, SPDX License List 3.23
    | GFDL_1_2_only -- ^ @GFDL-1.2-only@, GNU Free Documentation License v1.2 only
    | GFDL_1_2_or_later -- ^ @GFDL-1.2-or-later@, GNU Free Documentation License v1.2 or later
    | GFDL_1_3_invariants_only -- ^ @GFDL-1.3-invariants-only@, GNU Free Documentation License v1.3 only - invariants, SPDX License List 3.10, SPDX License List 3.16, SPDX License List 3.23
    | GFDL_1_3_invariants_or_later -- ^ @GFDL-1.3-invariants-or-later@, GNU Free Documentation License v1.3 or later - invariants, SPDX License List 3.10, SPDX License List 3.16, SPDX License List 3.23
    | GFDL_1_3_no_invariants_only -- ^ @GFDL-1.3-no-invariants-only@, GNU Free Documentation License v1.3 only - no invariants, SPDX License List 3.10, SPDX License List 3.16, SPDX License List 3.23
    | GFDL_1_3_no_invariants_or_later -- ^ @GFDL-1.3-no-invariants-or-later@, GNU Free Documentation License v1.3 or later - no invariants, SPDX License List 3.10, SPDX License List 3.16, SPDX License List 3.23
    | GFDL_1_3_only -- ^ @GFDL-1.3-only@, GNU Free Documentation License v1.3 only
    | GFDL_1_3_or_later -- ^ @GFDL-1.3-or-later@, GNU Free Documentation License v1.3 or later
    | Giftware -- ^ @Giftware@, Giftware License
    | GL2PS -- ^ @GL2PS@, GL2PS License
    | Glide -- ^ @Glide@, 3dfx Glide License
    | Glulxe -- ^ @Glulxe@, Glulxe License
    | GLWTPL -- ^ @GLWTPL@, Good Luck With That Public License, SPDX License List 3.10, SPDX License List 3.16, SPDX License List 3.23
    | Gnuplot -- ^ @gnuplot@, gnuplot License
    | GPL_1_0_only -- ^ @GPL-1.0-only@, GNU General Public License v1.0 only
    | GPL_1_0_or_later -- ^ @GPL-1.0-or-later@, GNU General Public License v1.0 or later
    | GPL_2_0_only -- ^ @GPL-2.0-only@, GNU General Public License v2.0 only
    | GPL_2_0_or_later -- ^ @GPL-2.0-or-later@, GNU General Public License v2.0 or later
    | GPL_3_0_only -- ^ @GPL-3.0-only@, GNU General Public License v3.0 only
    | GPL_3_0_or_later -- ^ @GPL-3.0-or-later@, GNU General Public License v3.0 or later
    | Graphics_Gems -- ^ @Graphics-Gems@, Graphics Gems License, SPDX License List 3.23
    | GSOAP_1_3b -- ^ @gSOAP-1.3b@, gSOAP Public License v1.3b
    | Gtkbook -- ^ @gtkbook@, gtkbook License, SPDX License List 3.23
    | HaskellReport -- ^ @HaskellReport@, Haskell Language Report License
    | Hdparm -- ^ @hdparm@, hdparm License, SPDX License List 3.23
    | Hippocratic_2_1 -- ^ @Hippocratic-2.1@, Hippocratic License 2.1, SPDX License List 3.9, SPDX License List 3.10, SPDX License List 3.16, SPDX License List 3.23
    | HP_1986 -- ^ @HP-1986@, Hewlett-Packard 1986 License, SPDX License List 3.23
    | HP_1989 -- ^ @HP-1989@, Hewlett-Packard 1989 License, SPDX License List 3.23
    | HPND_DEC -- ^ @HPND-DEC@, Historical Permission Notice and Disclaimer - DEC variant, SPDX License List 3.23
    | HPND_doc_sell -- ^ @HPND-doc-sell@, Historical Permission Notice and Disclaimer - documentation sell variant, SPDX License List 3.23
    | HPND_doc -- ^ @HPND-doc@, Historical Permission Notice and Disclaimer - documentation variant, SPDX License List 3.23
    | HPND_export_US_modify -- ^ @HPND-export-US-modify@, HPND with US Government export control warning and modification rqmt, SPDX License List 3.23
    | HPND_export_US -- ^ @HPND-export-US@, HPND with US Government export control warning, SPDX License List 3.23
    | HPND_Fenneberg_Livingston -- ^ @HPND-Fenneberg-Livingston@, Historical Permission Notice and Disclaimer - Fenneberg-Livingston variant, SPDX License List 3.23
    | HPND_INRIA_IMAG -- ^ @HPND-INRIA-IMAG@, Historical Permission Notice and Disclaimer    - INRIA-IMAG variant, SPDX License List 3.23
    | HPND_Kevlin_Henney -- ^ @HPND-Kevlin-Henney@, Historical Permission Notice and Disclaimer - Kevlin Henney variant, SPDX License List 3.23
    | HPND_Markus_Kuhn -- ^ @HPND-Markus-Kuhn@, Historical Permission Notice and Disclaimer - Markus Kuhn variant, SPDX License List 3.23
    | HPND_MIT_disclaimer -- ^ @HPND-MIT-disclaimer@, Historical Permission Notice and Disclaimer with MIT disclaimer, SPDX License List 3.23
    | HPND_Pbmplus -- ^ @HPND-Pbmplus@, Historical Permission Notice and Disclaimer - Pbmplus variant, SPDX License List 3.23
    | HPND_sell_MIT_disclaimer_xserver -- ^ @HPND-sell-MIT-disclaimer-xserver@, Historical Permission Notice and Disclaimer - sell xserver variant with MIT disclaimer, SPDX License List 3.23
    | HPND_sell_regexpr -- ^ @HPND-sell-regexpr@, Historical Permission Notice and Disclaimer - sell regexpr variant, SPDX License List 3.23
    | HPND_sell_variant_MIT_disclaimer -- ^ @HPND-sell-variant-MIT-disclaimer@, HPND sell variant with MIT disclaimer, SPDX License List 3.23
    | HPND_sell_variant -- ^ @HPND-sell-variant@, Historical Permission Notice and Disclaimer - sell variant, SPDX License List 3.6, SPDX License List 3.9, SPDX License List 3.10, SPDX License List 3.16, SPDX License List 3.23
    | HPND_UC -- ^ @HPND-UC@, Historical Permission Notice and Disclaimer - University of California variant, SPDX License List 3.23
    | HPND -- ^ @HPND@, Historical Permission Notice and Disclaimer
    | HTMLTIDY -- ^ @HTMLTIDY@, HTML Tidy License, SPDX License List 3.16, SPDX License List 3.23
    | IBM_pibs -- ^ @IBM-pibs@, IBM PowerPC Initialization and Boot Software
    | ICU -- ^ @ICU@, ICU License
    | IEC_Code_Components_EULA -- ^ @IEC-Code-Components-EULA@, IEC    Code Components End-user licence agreement, SPDX License List 3.23
    | IJG_short -- ^ @IJG-short@, Independent JPEG Group License - short, SPDX License List 3.23
    | IJG -- ^ @IJG@, Independent JPEG Group License
    | ImageMagick -- ^ @ImageMagick@, ImageMagick License
    | IMatix -- ^ @iMatix@, iMatix Standard Function Library Agreement
    | Imlib2 -- ^ @Imlib2@, Imlib2 License
    | Info_ZIP -- ^ @Info-ZIP@, Info-ZIP License
    | Inner_Net_2_0 -- ^ @Inner-Net-2.0@, Inner Net License v2.0, SPDX License List 3.23
    | Intel_ACPI -- ^ @Intel-ACPI@, Intel ACPI Software License Agreement
    | Intel -- ^ @Intel@, Intel Open Source License
    | Interbase_1_0 -- ^ @Interbase-1.0@, Interbase Public License v1.0
    | IPA -- ^ @IPA@, IPA Font License
    | IPL_1_0 -- ^ @IPL-1.0@, IBM Public License v1.0
    | ISC_Veillard -- ^ @ISC-Veillard@, ISC Veillard variant, SPDX License List 3.23
    | ISC -- ^ @ISC@, ISC License
    | Jam -- ^ @Jam@, Jam License, SPDX License List 3.16, SPDX License List 3.23
    | JasPer_2_0 -- ^ @JasPer-2.0@, JasPer License
    | JPL_image -- ^ @JPL-image@, JPL Image Use Policy, SPDX License List 3.23
    | JPNIC -- ^ @JPNIC@, Japan Network Information Center License, SPDX License List 3.6, SPDX License List 3.9, SPDX License List 3.10, SPDX License List 3.16, SPDX License List 3.23
    | JSON -- ^ @JSON@, JSON License
    | Kastrup -- ^ @Kastrup@, Kastrup License, SPDX License List 3.23
    | Kazlib -- ^ @Kazlib@, Kazlib License, SPDX License List 3.23
    | Knuth_CTAN -- ^ @Knuth-CTAN@, Knuth CTAN License, SPDX License List 3.23
    | LAL_1_2 -- ^ @LAL-1.2@, Licence Art Libre 1.2
    | LAL_1_3 -- ^ @LAL-1.3@, Licence Art Libre 1.3
    | Latex2e_translated_notice -- ^ @Latex2e-translated-notice@, Latex2e with translated notice permission, SPDX License List 3.23
    | Latex2e -- ^ @Latex2e@, Latex2e License
    | Leptonica -- ^ @Leptonica@, Leptonica License
    | LGPL_2_0_only -- ^ @LGPL-2.0-only@, GNU Library General Public License v2 only
    | LGPL_2_0_or_later -- ^ @LGPL-2.0-or-later@, GNU Library General Public License v2 or later
    | LGPL_2_1_only -- ^ @LGPL-2.1-only@, GNU Lesser General Public License v2.1 only
    | LGPL_2_1_or_later -- ^ @LGPL-2.1-or-later@, GNU Lesser General Public License v2.1 or later
    | LGPL_3_0_only -- ^ @LGPL-3.0-only@, GNU Lesser General Public License v3.0 only
    | LGPL_3_0_or_later -- ^ @LGPL-3.0-or-later@, GNU Lesser General Public License v3.0 or later
    | LGPLLR -- ^ @LGPLLR@, Lesser General Public License For Linguistic Resources
    | Libpng_2_0 -- ^ @libpng-2.0@, PNG Reference Library version 2, SPDX License List 3.6, SPDX License List 3.9, SPDX License List 3.10, SPDX License List 3.16, SPDX License List 3.23
    | Libpng -- ^ @Libpng@, libpng License
    | Libselinux_1_0 -- ^ @libselinux-1.0@, libselinux public domain notice, SPDX License List 3.9, SPDX License List 3.10, SPDX License List 3.16, SPDX License List 3.23
    | Libtiff -- ^ @libtiff@, libtiff License
    | Libutil_David_Nugent -- ^ @libutil-David-Nugent@, libutil David Nugent License, SPDX License List 3.23
    | LiLiQ_P_1_1 -- ^ @LiLiQ-P-1.1@, Licence Libre du Québec – Permissive version 1.1
    | LiLiQ_R_1_1 -- ^ @LiLiQ-R-1.1@, Licence Libre du Québec – Réciprocité version 1.1
    | LiLiQ_Rplus_1_1 -- ^ @LiLiQ-Rplus-1.1@, Licence Libre du Québec – Réciprocité forte version 1.1
    | Linux_man_pages_1_para -- ^ @Linux-man-pages-1-para@, Linux man-pages - 1 paragraph, SPDX License List 3.23
    | Linux_man_pages_copyleft_2_para -- ^ @Linux-man-pages-copyleft-2-para@, Linux man-pages Copyleft - 2 paragraphs, SPDX License List 3.23
    | Linux_man_pages_copyleft_var -- ^ @Linux-man-pages-copyleft-var@, Linux man-pages Copyleft Variant, SPDX License List 3.23
    | Linux_man_pages_copyleft -- ^ @Linux-man-pages-copyleft@, Linux man-pages Copyleft, SPDX License List 3.16, SPDX License List 3.23
    | Linux_OpenIB -- ^ @Linux-OpenIB@, Linux Kernel Variant of OpenIB.org license, SPDX License List 3.2, SPDX License List 3.6, SPDX License List 3.9, SPDX License List 3.10, SPDX License List 3.16, SPDX License List 3.23
    | LOOP -- ^ @LOOP@, Common Lisp LOOP License, SPDX License List 3.23
    | LPD_document -- ^ @LPD-document@, LPD Documentation License, SPDX License List 3.23
    | LPL_1_02 -- ^ @LPL-1.02@, Lucent Public License v1.02
    | LPL_1_0 -- ^ @LPL-1.0@, Lucent Public License Version 1.0
    | LPPL_1_0 -- ^ @LPPL-1.0@, LaTeX Project Public License v1.0
    | LPPL_1_1 -- ^ @LPPL-1.1@, LaTeX Project Public License v1.1
    | LPPL_1_2 -- ^ @LPPL-1.2@, LaTeX Project Public License v1.2
    | LPPL_1_3a -- ^ @LPPL-1.3a@, LaTeX Project Public License v1.3a
    | LPPL_1_3c -- ^ @LPPL-1.3c@, LaTeX Project Public License v1.3c
    | Lsof -- ^ @lsof@, lsof License, SPDX License List 3.23
    | Lucida_Bitmap_Fonts -- ^ @Lucida-Bitmap-Fonts@, Lucida Bitmap Fonts License, SPDX License List 3.23
    | LZMA_SDK_9_11_to_9_20 -- ^ @LZMA-SDK-9.11-to-9.20@, LZMA SDK License (versions 9.11 to 9.20), SPDX License List 3.23
    | LZMA_SDK_9_22 -- ^ @LZMA-SDK-9.22@, LZMA SDK License (versions 9.22 and beyond), SPDX License List 3.23
    | Mackerras_3_Clause_acknowledgment -- ^ @Mackerras-3-Clause-acknowledgment@, Mackerras 3-Clause - acknowledgment variant, SPDX License List 3.23
    | Mackerras_3_Clause -- ^ @Mackerras-3-Clause@, Mackerras 3-Clause License, SPDX License List 3.23
    | Magaz -- ^ @magaz@, magaz License, SPDX License List 3.23
    | Mailprio -- ^ @mailprio@, mailprio License, SPDX License List 3.23
    | MakeIndex -- ^ @MakeIndex@, MakeIndex License
    | Martin_Birgmeier -- ^ @Martin-Birgmeier@, Martin Birgmeier License, SPDX License List 3.23
    | McPhee_slideshow -- ^ @McPhee-slideshow@, McPhee Slideshow License, SPDX License List 3.23
    | Metamail -- ^ @metamail@, metamail License, SPDX License List 3.23
    | Minpack -- ^ @Minpack@, Minpack License, SPDX License List 3.23
    | MirOS -- ^ @MirOS@, The MirOS Licence
    | MIT_0 -- ^ @MIT-0@, MIT No Attribution, SPDX License List 3.2, SPDX License List 3.6, SPDX License List 3.9, SPDX License List 3.10, SPDX License List 3.16, SPDX License List 3.23
    | MIT_advertising -- ^ @MIT-advertising@, Enlightenment License (e16)
    | MIT_CMU -- ^ @MIT-CMU@, CMU License
    | MIT_enna -- ^ @MIT-enna@, enna License
    | MIT_feh -- ^ @MIT-feh@, feh License
    | MIT_Festival -- ^ @MIT-Festival@, MIT Festival Variant, SPDX License List 3.23
    | MIT_Modern_Variant -- ^ @MIT-Modern-Variant@, MIT License Modern Variant, SPDX License List 3.16, SPDX License List 3.23
    | MIT_open_group -- ^ @MIT-open-group@, MIT Open Group variant, SPDX License List 3.16, SPDX License List 3.23
    | MIT_testregex -- ^ @MIT-testregex@, MIT testregex Variant, SPDX License List 3.23
    | MIT_Wu -- ^ @MIT-Wu@, MIT Tom Wu Variant, SPDX License List 3.23
    | MITNFA -- ^ @MITNFA@, MIT +no-false-attribs license
    | MIT -- ^ @MIT@, MIT License
    | MMIXware -- ^ @MMIXware@, MMIXware License, SPDX License List 3.23
    | Motosoto -- ^ @Motosoto@, Motosoto License
    | MPEG_SSG -- ^ @MPEG-SSG@, MPEG Software Simulation, SPDX License List 3.23
    | Mpi_permissive -- ^ @mpi-permissive@, mpi Permissive License, SPDX License List 3.23
    | Mpich2 -- ^ @mpich2@, mpich2 License
    | MPL_1_0 -- ^ @MPL-1.0@, Mozilla Public License 1.0
    | MPL_1_1 -- ^ @MPL-1.1@, Mozilla Public License 1.1
    | MPL_2_0_no_copyleft_exception -- ^ @MPL-2.0-no-copyleft-exception@, Mozilla Public License 2.0 (no copyleft exception)
    | MPL_2_0 -- ^ @MPL-2.0@, Mozilla Public License 2.0
    | Mplus -- ^ @mplus@, mplus Font License, SPDX License List 3.23
    | MS_LPL -- ^ @MS-LPL@, Microsoft Limited Public License, SPDX License List 3.23
    | MS_PL -- ^ @MS-PL@, Microsoft Public License
    | MS_RL -- ^ @MS-RL@, Microsoft Reciprocal License
    | MTLL -- ^ @MTLL@, Matrix Template Library License
    | MulanPSL_1_0 -- ^ @MulanPSL-1.0@, Mulan Permissive Software License, Version 1, SPDX License List 3.9, SPDX License List 3.10, SPDX License List 3.16, SPDX License List 3.23
    | MulanPSL_2_0 -- ^ @MulanPSL-2.0@, Mulan Permissive Software License, Version 2, SPDX License List 3.9, SPDX License List 3.10, SPDX License List 3.16, SPDX License List 3.23
    | Multics -- ^ @Multics@, Multics License
    | Mup -- ^ @Mup@, Mup License
    | NAIST_2003 -- ^ @NAIST-2003@, Nara Institute of Science and Technology License (2003), SPDX License List 3.16, SPDX License List 3.23
    | NASA_1_3 -- ^ @NASA-1.3@, NASA Open Source Agreement 1.3
    | Naumen -- ^ @Naumen@, Naumen Public License
    | NBPL_1_0 -- ^ @NBPL-1.0@, Net Boolean Public License v1
    | NCGL_UK_2_0 -- ^ @NCGL-UK-2.0@, Non-Commercial Government Licence, SPDX License List 3.9, SPDX License List 3.10, SPDX License List 3.16, SPDX License List 3.23
    | NCSA -- ^ @NCSA@, University of Illinois/NCSA Open Source License
    | Net_SNMP -- ^ @Net-SNMP@, Net-SNMP License
    | NetCDF -- ^ @NetCDF@, NetCDF license
    | Newsletr -- ^ @Newsletr@, Newsletr License
    | NGPL -- ^ @NGPL@, Nethack General Public License
    | NICTA_1_0 -- ^ @NICTA-1.0@, NICTA Public Software License, Version 1.0, SPDX License List 3.23
    | NIST_PD_fallback -- ^ @NIST-PD-fallback@, NIST Public Domain Notice with license fallback, SPDX License List 3.10, SPDX License List 3.16, SPDX License List 3.23
    | NIST_PD -- ^ @NIST-PD@, NIST Public Domain Notice, SPDX License List 3.10, SPDX License List 3.16, SPDX License List 3.23
    | NIST_Software -- ^ @NIST-Software@, NIST Software License, SPDX License List 3.23
    | NLOD_1_0 -- ^ @NLOD-1.0@, Norwegian Licence for Open Government Data (NLOD) 1.0
    | NLOD_2_0 -- ^ @NLOD-2.0@, Norwegian Licence for Open Government Data (NLOD) 2.0, SPDX License List 3.16, SPDX License List 3.23
    | NLPL -- ^ @NLPL@, No Limit Public License
    | Nokia -- ^ @Nokia@, Nokia Open Source License
    | NOSL -- ^ @NOSL@, Netizen Open Source License
    | Noweb -- ^ @Noweb@, Noweb License
    | NPL_1_0 -- ^ @NPL-1.0@, Netscape Public License v1.0
    | NPL_1_1 -- ^ @NPL-1.1@, Netscape Public License v1.1
    | NPOSL_3_0 -- ^ @NPOSL-3.0@, Non-Profit Open Software License 3.0
    | NRL -- ^ @NRL@, NRL License
    | NTP_0 -- ^ @NTP-0@, NTP No Attribution, SPDX License List 3.9, SPDX License List 3.10, SPDX License List 3.16, SPDX License List 3.23
    | NTP -- ^ @NTP@, NTP License
    | O_UDA_1_0 -- ^ @O-UDA-1.0@, Open Use of Data Agreement v1.0, SPDX License List 3.9, SPDX License List 3.10, SPDX License List 3.16, SPDX License List 3.23
    | OCCT_PL -- ^ @OCCT-PL@, Open CASCADE Technology Public License
    | OCLC_2_0 -- ^ @OCLC-2.0@, OCLC Research Public License 2.0
    | ODbL_1_0 -- ^ @ODbL-1.0@, Open Data Commons Open Database License v1.0
    | ODC_By_1_0 -- ^ @ODC-By-1.0@, Open Data Commons Attribution License v1.0, SPDX License List 3.2, SPDX License List 3.6, SPDX License List 3.9, SPDX License List 3.10, SPDX License List 3.16, SPDX License List 3.23
    | OFFIS -- ^ @OFFIS@, OFFIS License, SPDX License List 3.23
    | OFL_1_0_no_RFN -- ^ @OFL-1.0-no-RFN@, SIL Open Font License 1.0 with no Reserved Font Name, SPDX License List 3.9, SPDX License List 3.10, SPDX License List 3.16, SPDX License List 3.23
    | OFL_1_0_RFN -- ^ @OFL-1.0-RFN@, SIL Open Font License 1.0 with Reserved Font Name, SPDX License List 3.9, SPDX License List 3.10, SPDX License List 3.16, SPDX License List 3.23
    | OFL_1_0 -- ^ @OFL-1.0@, SIL Open Font License 1.0
    | OFL_1_1_no_RFN -- ^ @OFL-1.1-no-RFN@, SIL Open Font License 1.1 with no Reserved Font Name, SPDX License List 3.9, SPDX License List 3.10, SPDX License List 3.16, SPDX License List 3.23
    | OFL_1_1_RFN -- ^ @OFL-1.1-RFN@, SIL Open Font License 1.1 with Reserved Font Name, SPDX License List 3.9, SPDX License List 3.10, SPDX License List 3.16, SPDX License List 3.23
    | OFL_1_1 -- ^ @OFL-1.1@, SIL Open Font License 1.1
    | OGC_1_0 -- ^ @OGC-1.0@, OGC Software License, Version 1.0, SPDX License List 3.9, SPDX License List 3.10, SPDX License List 3.16, SPDX License List 3.23
    | OGDL_Taiwan_1_0 -- ^ @OGDL-Taiwan-1.0@, Taiwan Open Government Data License, version 1.0, SPDX License List 3.16, SPDX License List 3.23
    | OGL_Canada_2_0 -- ^ @OGL-Canada-2.0@, Open Government Licence - Canada, SPDX License List 3.9, SPDX License List 3.10, SPDX License List 3.16, SPDX License List 3.23
    | OGL_UK_1_0 -- ^ @OGL-UK-1.0@, Open Government Licence v1.0, SPDX License List 3.6, SPDX License List 3.9, SPDX License List 3.10, SPDX License List 3.16, SPDX License List 3.23
    | OGL_UK_2_0 -- ^ @OGL-UK-2.0@, Open Government Licence v2.0, SPDX License List 3.6, SPDX License List 3.9, SPDX License List 3.10, SPDX License List 3.16, SPDX License List 3.23
    | OGL_UK_3_0 -- ^ @OGL-UK-3.0@, Open Government Licence v3.0, SPDX License List 3.6, SPDX License List 3.9, SPDX License List 3.10, SPDX License List 3.16, SPDX License List 3.23
    | OGTSL -- ^ @OGTSL@, Open Group Test Suite License
    | OLDAP_1_1 -- ^ @OLDAP-1.1@, Open LDAP Public License v1.1
    | OLDAP_1_2 -- ^ @OLDAP-1.2@, Open LDAP Public License v1.2
    | OLDAP_1_3 -- ^ @OLDAP-1.3@, Open LDAP Public License v1.3
    | OLDAP_1_4 -- ^ @OLDAP-1.4@, Open LDAP Public License v1.4
    | OLDAP_2_0_1 -- ^ @OLDAP-2.0.1@, Open LDAP Public License v2.0.1
    | OLDAP_2_0 -- ^ @OLDAP-2.0@, Open LDAP Public License v2.0 (or possibly 2.0A and 2.0B)
    | OLDAP_2_1 -- ^ @OLDAP-2.1@, Open LDAP Public License v2.1
    | OLDAP_2_2_1 -- ^ @OLDAP-2.2.1@, Open LDAP Public License v2.2.1
    | OLDAP_2_2_2 -- ^ @OLDAP-2.2.2@, Open LDAP Public License 2.2.2
    | OLDAP_2_2 -- ^ @OLDAP-2.2@, Open LDAP Public License v2.2
    | OLDAP_2_3 -- ^ @OLDAP-2.3@, Open LDAP Public License v2.3
    | OLDAP_2_4 -- ^ @OLDAP-2.4@, Open LDAP Public License v2.4
    | OLDAP_2_5 -- ^ @OLDAP-2.5@, Open LDAP Public License v2.5
    | OLDAP_2_6 -- ^ @OLDAP-2.6@, Open LDAP Public License v2.6
    | OLDAP_2_7 -- ^ @OLDAP-2.7@, Open LDAP Public License v2.7
    | OLDAP_2_8 -- ^ @OLDAP-2.8@, Open LDAP Public License v2.8
    | OLFL_1_3 -- ^ @OLFL-1.3@, Open Logistics Foundation License Version 1.3, SPDX License List 3.23
    | OML -- ^ @OML@, Open Market License
    | OpenPBS_2_3 -- ^ @OpenPBS-2.3@, OpenPBS v2.3 Software License, SPDX License List 3.23
    | OpenSSL_standalone -- ^ @OpenSSL-standalone@, OpenSSL License - standalone, SPDX License List 3.23
    | OpenSSL -- ^ @OpenSSL@, OpenSSL License
    | OpenVision -- ^ @OpenVision@, OpenVision License, SPDX License List 3.23
    | OPL_1_0 -- ^ @OPL-1.0@, Open Public License v1.0
    | OPL_UK_3_0 -- ^ @OPL-UK-3.0@, United    Kingdom Open Parliament Licence v3.0, SPDX License List 3.23
    | OPUBL_1_0 -- ^ @OPUBL-1.0@, Open Publication License v1.0, SPDX License List 3.16, SPDX License List 3.23
    | OSET_PL_2_1 -- ^ @OSET-PL-2.1@, OSET Public License version 2.1
    | OSL_1_0 -- ^ @OSL-1.0@, Open Software License 1.0
    | OSL_1_1 -- ^ @OSL-1.1@, Open Software License 1.1
    | OSL_2_0 -- ^ @OSL-2.0@, Open Software License 2.0
    | OSL_2_1 -- ^ @OSL-2.1@, Open Software License 2.1
    | OSL_3_0 -- ^ @OSL-3.0@, Open Software License 3.0
    | PADL -- ^ @PADL@, PADL License, SPDX License List 3.23
    | Parity_6_0_0 -- ^ @Parity-6.0.0@, The Parity Public License 6.0.0, SPDX License List 3.6, SPDX License List 3.9, SPDX License List 3.10, SPDX License List 3.16, SPDX License List 3.23
    | Parity_7_0_0 -- ^ @Parity-7.0.0@, The Parity Public License 7.0.0, SPDX License List 3.9, SPDX License List 3.10, SPDX License List 3.16, SPDX License List 3.23
    | PDDL_1_0 -- ^ @PDDL-1.0@, Open Data Commons Public Domain Dedication & License 1.0
    | PHP_3_01 -- ^ @PHP-3.01@, PHP License v3.01
    | PHP_3_0 -- ^ @PHP-3.0@, PHP License v3.0
    | Pixar -- ^ @Pixar@, Pixar License, SPDX License List 3.23
    | Plexus -- ^ @Plexus@, Plexus Classworlds License
    | Pnmstitch -- ^ @pnmstitch@, pnmstitch License, SPDX License List 3.23
    | PolyForm_Noncommercial_1_0_0 -- ^ @PolyForm-Noncommercial-1.0.0@, PolyForm Noncommercial License 1.0.0, SPDX License List 3.9, SPDX License List 3.10, SPDX License List 3.16, SPDX License List 3.23
    | PolyForm_Small_Business_1_0_0 -- ^ @PolyForm-Small-Business-1.0.0@, PolyForm Small Business License 1.0.0, SPDX License List 3.9, SPDX License List 3.10, SPDX License List 3.16, SPDX License List 3.23
    | PostgreSQL -- ^ @PostgreSQL@, PostgreSQL License
    | PSF_2_0 -- ^ @PSF-2.0@, Python Software Foundation License 2.0, SPDX License List 3.9, SPDX License List 3.10, SPDX License List 3.16, SPDX License List 3.23
    | Psfrag -- ^ @psfrag@, psfrag License
    | Psutils -- ^ @psutils@, psutils License
    | Python_2_0_1 -- ^ @Python-2.0.1@, Python License 2.0.1, SPDX License List 3.23
    | Python_2_0 -- ^ @Python-2.0@, Python License 2.0
    | Python_ldap -- ^ @python-ldap@, Python ldap License, SPDX License List 3.23
    | Qhull -- ^ @Qhull@, Qhull License
    | QPL_1_0_INRIA_2004 -- ^ @QPL-1.0-INRIA-2004@, Q Public License 1.0 - INRIA 2004 variant, SPDX License List 3.23
    | QPL_1_0 -- ^ @QPL-1.0@, Q Public License 1.0
    | Radvd -- ^ @radvd@, radvd License, SPDX License List 3.23
    | Rdisc -- ^ @Rdisc@, Rdisc License
    | RHeCos_1_1 -- ^ @RHeCos-1.1@, Red Hat eCos Public License v1.1
    | RPL_1_1 -- ^ @RPL-1.1@, Reciprocal Public License 1.1
    | RPL_1_5 -- ^ @RPL-1.5@, Reciprocal Public License 1.5
    | RPSL_1_0 -- ^ @RPSL-1.0@, RealNetworks Public Source License v1.0
    | RSA_MD -- ^ @RSA-MD@, RSA Message-Digest License
    | RSCPL -- ^ @RSCPL@, Ricoh Source Code Public License
    | Ruby -- ^ @Ruby@, Ruby License
    | SAX_PD_2_0 -- ^ @SAX-PD-2.0@, Sax Public Domain Notice 2.0, SPDX License List 3.23
    | SAX_PD -- ^ @SAX-PD@, Sax Public Domain Notice
    | Saxpath -- ^ @Saxpath@, Saxpath License
    | SCEA -- ^ @SCEA@, SCEA Shared Source License
    | SchemeReport -- ^ @SchemeReport@, Scheme Language Report License, SPDX License List 3.16, SPDX License List 3.23
    | Sendmail_8_23 -- ^ @Sendmail-8.23@, Sendmail License 8.23, SPDX License List 3.6, SPDX License List 3.9, SPDX License List 3.10, SPDX License List 3.16, SPDX License List 3.23
    | Sendmail -- ^ @Sendmail@, Sendmail License
    | SGI_B_1_0 -- ^ @SGI-B-1.0@, SGI Free Software License B v1.0
    | SGI_B_1_1 -- ^ @SGI-B-1.1@, SGI Free Software License B v1.1
    | SGI_B_2_0 -- ^ @SGI-B-2.0@, SGI Free Software License B v2.0
    | SGI_OpenGL -- ^ @SGI-OpenGL@, SGI OpenGL License, SPDX License List 3.23
    | SGP4 -- ^ @SGP4@, SGP4 Permission Notice, SPDX License List 3.23
    | SHL_0_51 -- ^ @SHL-0.51@, Solderpad Hardware License, Version 0.51, SPDX License List 3.6, SPDX License List 3.9, SPDX License List 3.10, SPDX License List 3.16, SPDX License List 3.23
    | SHL_0_5 -- ^ @SHL-0.5@, Solderpad Hardware License v0.5, SPDX License List 3.6, SPDX License List 3.9, SPDX License List 3.10, SPDX License List 3.16, SPDX License List 3.23
    | SimPL_2_0 -- ^ @SimPL-2.0@, Simple Public License 2.0
    | SISSL_1_2 -- ^ @SISSL-1.2@, Sun Industry Standards Source License v1.2
    | SISSL -- ^ @SISSL@, Sun Industry Standards Source License v1.1
    | Sleepycat -- ^ @Sleepycat@, Sleepycat License
    | SL -- ^ @SL@, SL License, SPDX License List 3.23
    | SMLNJ -- ^ @SMLNJ@, Standard ML of New Jersey License
    | SMPPL -- ^ @SMPPL@, Secure Messaging Protocol Public License
    | SNIA -- ^ @SNIA@, SNIA Public License 1.1
    | Snprintf -- ^ @snprintf@, snprintf License, SPDX License List 3.23
    | SoftSurfer -- ^ @softSurfer@, softSurfer License, SPDX License List 3.23
    | Soundex -- ^ @Soundex@, Soundex License, SPDX License List 3.23
    | Spencer_86 -- ^ @Spencer-86@, Spencer License 86
    | Spencer_94 -- ^ @Spencer-94@, Spencer License 94
    | Spencer_99 -- ^ @Spencer-99@, Spencer License 99
    | SPL_1_0 -- ^ @SPL-1.0@, Sun Public License v1.0
    | Ssh_keyscan -- ^ @ssh-keyscan@, ssh-keyscan License, SPDX License List 3.23
    | SSH_OpenSSH -- ^ @SSH-OpenSSH@, SSH OpenSSH license, SPDX License List 3.9, SPDX License List 3.10, SPDX License List 3.16, SPDX License List 3.23
    | SSH_short -- ^ @SSH-short@, SSH short notice, SPDX License List 3.9, SPDX License List 3.10, SPDX License List 3.16, SPDX License List 3.23
    | SSLeay_standalone -- ^ @SSLeay-standalone@, SSLeay License - standalone, SPDX License List 3.23
    | SSPL_1_0 -- ^ @SSPL-1.0@, Server Side Public License, v 1, SPDX License List 3.6, SPDX License List 3.9, SPDX License List 3.10, SPDX License List 3.16, SPDX License List 3.23
    | SugarCRM_1_1_3 -- ^ @SugarCRM-1.1.3@, SugarCRM Public License v1.1.3
    | Sun_PPP -- ^ @Sun-PPP@, Sun PPP License, SPDX License List 3.23
    | SunPro -- ^ @SunPro@, SunPro License, SPDX License List 3.23
    | SWL -- ^ @SWL@, Scheme Widget Library (SWL) Software License Agreement
    | Swrule -- ^ @swrule@, swrule License, SPDX License List 3.23
    | Symlinks -- ^ @Symlinks@, Symlinks License, SPDX License List 3.23
    | TAPR_OHL_1_0 -- ^ @TAPR-OHL-1.0@, TAPR Open Hardware License v1.0, SPDX License List 3.6, SPDX License List 3.9, SPDX License List 3.10, SPDX License List 3.16, SPDX License List 3.23
    | TCL -- ^ @TCL@, TCL/TK License
    | TCP_wrappers -- ^ @TCP-wrappers@, TCP Wrappers License
    | TermReadKey -- ^ @TermReadKey@, TermReadKey License, SPDX License List 3.23
    | TGPPL_1_0 -- ^ @TGPPL-1.0@, Transitive Grace Period Public Licence 1.0, SPDX License List 3.23
    | TMate -- ^ @TMate@, TMate Open Source License
    | TORQUE_1_1 -- ^ @TORQUE-1.1@, TORQUE v2.5+ Software License v1.1
    | TOSL -- ^ @TOSL@, Trusster Open Source License
    | TPDL -- ^ @TPDL@, Time::ParseDate License, SPDX License List 3.23
    | TPL_1_0 -- ^ @TPL-1.0@, THOR Public License 1.0, SPDX License List 3.23
    | TTWL -- ^ @TTWL@, Text-Tabs+Wrap License, SPDX License List 3.23
    | TTYP0 -- ^ @TTYP0@, TTYP0 License, SPDX License List 3.23
    | TU_Berlin_1_0 -- ^ @TU-Berlin-1.0@, Technische Universitaet Berlin License 1.0, SPDX License List 3.2, SPDX License List 3.6, SPDX License List 3.9, SPDX License List 3.10, SPDX License List 3.16, SPDX License List 3.23
    | TU_Berlin_2_0 -- ^ @TU-Berlin-2.0@, Technische Universitaet Berlin License 2.0, SPDX License List 3.2, SPDX License List 3.6, SPDX License List 3.9, SPDX License List 3.10, SPDX License List 3.16, SPDX License List 3.23
    | UCAR -- ^ @UCAR@, UCAR License, SPDX License List 3.23
    | UCL_1_0 -- ^ @UCL-1.0@, Upstream Compatibility License v1.0, SPDX License List 3.9, SPDX License List 3.10, SPDX License List 3.16, SPDX License List 3.23
    | Ulem -- ^ @ulem@, ulem License, SPDX License List 3.23
    | UMich_Merit -- ^ @UMich-Merit@, Michigan/Merit Networks License, SPDX License List 3.23
    | Unicode_3_0 -- ^ @Unicode-3.0@, Unicode License v3, SPDX License List 3.23
    | Unicode_DFS_2015 -- ^ @Unicode-DFS-2015@, Unicode License Agreement - Data Files and Software (2015)
    | Unicode_DFS_2016 -- ^ @Unicode-DFS-2016@, Unicode License Agreement - Data Files and Software (2016)
    | Unicode_TOU -- ^ @Unicode-TOU@, Unicode Terms of Use
    | UnixCrypt -- ^ @UnixCrypt@, UnixCrypt License, SPDX License List 3.23
    | Unlicense -- ^ @Unlicense@, The Unlicense
    | UPL_1_0 -- ^ @UPL-1.0@, Universal Permissive License v1.0
    | URT_RLE -- ^ @URT-RLE@, Utah Raster Toolkit Run Length Encoded License, SPDX License List 3.23
    | Vim -- ^ @Vim@, Vim License
    | VOSTROM -- ^ @VOSTROM@, VOSTROM Public License for Open Source
    | VSL_1_0 -- ^ @VSL-1.0@, Vovida Software License v1.0
    | W3C_19980720 -- ^ @W3C-19980720@, W3C Software Notice and License (1998-07-20)
    | W3C_20150513 -- ^ @W3C-20150513@, W3C Software Notice and Document License (2015-05-13)
    | W3C -- ^ @W3C@, W3C Software Notice and License (2002-12-31)
    | W3m -- ^ @w3m@, w3m License, SPDX License List 3.23
    | Watcom_1_0 -- ^ @Watcom-1.0@, Sybase Open Watcom Public License 1.0
    | Widget_Workshop -- ^ @Widget-Workshop@, Widget Workshop License, SPDX License List 3.23
    | Wsuipa -- ^ @Wsuipa@, Wsuipa License
    | WTFPL -- ^ @WTFPL@, Do What The F*ck You Want To Public License
    | X11_distribute_modifications_variant -- ^ @X11-distribute-modifications-variant@, X11 License Distribution Modification Variant, SPDX License List 3.16, SPDX License List 3.23
    | X11 -- ^ @X11@, X11 License
    | Xdebug_1_03 -- ^ @Xdebug-1.03@, Xdebug License v 1.03, SPDX License List 3.23
    | Xerox -- ^ @Xerox@, Xerox License
    | Xfig -- ^ @Xfig@, Xfig License, SPDX License List 3.23
    | XFree86_1_1 -- ^ @XFree86-1.1@, XFree86 License 1.1
    | Xinetd -- ^ @xinetd@, xinetd License
    | Xkeyboard_config_Zinoviev -- ^ @xkeyboard-config-Zinoviev@, xkeyboard-config Zinoviev License, SPDX License List 3.23
    | Xlock -- ^ @xlock@, xlock License, SPDX License List 3.23
    | Xnet -- ^ @Xnet@, X.Net License
    | Xpp -- ^ @xpp@, XPP License
    | XSkat -- ^ @XSkat@, XSkat License
    | YPL_1_0 -- ^ @YPL-1.0@, Yahoo! Public License v1.0
    | YPL_1_1 -- ^ @YPL-1.1@, Yahoo! Public License v1.1
    | Zed -- ^ @Zed@, Zed License
    | Zeeff -- ^ @Zeeff@, Zeeff License, SPDX License List 3.23
    | Zend_2_0 -- ^ @Zend-2.0@, Zend License v2.0
    | Zimbra_1_3 -- ^ @Zimbra-1.3@, Zimbra Public License v1.3
    | Zimbra_1_4 -- ^ @Zimbra-1.4@, Zimbra Public License v1.4
    | Zlib_acknowledgement -- ^ @zlib-acknowledgement@, zlib/libpng License with Acknowledgement
    | Zlib -- ^ @Zlib@, zlib License
    | ZPL_1_1 -- ^ @ZPL-1.1@, Zope Public License 1.1
    | ZPL_2_0 -- ^ @ZPL-2.0@, Zope Public License 2.0
    | ZPL_2_1 -- ^ @ZPL-2.1@, Zope Public License 2.1
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
licenseId NullBSD = "0BSD"
licenseId AAL = "AAL"
licenseId Abstyles = "Abstyles"
licenseId AdaCore_doc = "AdaCore-doc"
licenseId Adobe_2006 = "Adobe-2006"
licenseId Adobe_Display_PostScript = "Adobe-Display-PostScript"
licenseId Adobe_Glyph = "Adobe-Glyph"
licenseId Adobe_Utopia = "Adobe-Utopia"
licenseId ADSL = "ADSL"
licenseId AFL_1_1 = "AFL-1.1"
licenseId AFL_1_2 = "AFL-1.2"
licenseId AFL_2_0 = "AFL-2.0"
licenseId AFL_2_1 = "AFL-2.1"
licenseId AFL_3_0 = "AFL-3.0"
licenseId Afmparse = "Afmparse"
licenseId AGPL_1_0 = "AGPL-1.0"
licenseId AGPL_1_0_only = "AGPL-1.0-only"
licenseId AGPL_1_0_or_later = "AGPL-1.0-or-later"
licenseId AGPL_3_0_only = "AGPL-3.0-only"
licenseId AGPL_3_0_or_later = "AGPL-3.0-or-later"
licenseId Aladdin = "Aladdin"
licenseId AMDPLPA = "AMDPLPA"
licenseId AML_glslang = "AML-glslang"
licenseId AML = "AML"
licenseId AMPAS = "AMPAS"
licenseId ANTLR_PD_fallback = "ANTLR-PD-fallback"
licenseId ANTLR_PD = "ANTLR-PD"
licenseId Apache_1_0 = "Apache-1.0"
licenseId Apache_1_1 = "Apache-1.1"
licenseId Apache_2_0 = "Apache-2.0"
licenseId APAFML = "APAFML"
licenseId APL_1_0 = "APL-1.0"
licenseId App_s2p = "App-s2p"
licenseId APSL_1_0 = "APSL-1.0"
licenseId APSL_1_1 = "APSL-1.1"
licenseId APSL_1_2 = "APSL-1.2"
licenseId APSL_2_0 = "APSL-2.0"
licenseId Arphic_1999 = "Arphic-1999"
licenseId Artistic_1_0_cl8 = "Artistic-1.0-cl8"
licenseId Artistic_1_0_Perl = "Artistic-1.0-Perl"
licenseId Artistic_1_0 = "Artistic-1.0"
licenseId Artistic_2_0 = "Artistic-2.0"
licenseId ASWF_Digital_Assets_1_0 = "ASWF-Digital-Assets-1.0"
licenseId ASWF_Digital_Assets_1_1 = "ASWF-Digital-Assets-1.1"
licenseId Baekmuk = "Baekmuk"
licenseId Bahyph = "Bahyph"
licenseId Barr = "Barr"
licenseId Bcrypt_Solar_Designer = "bcrypt-Solar-Designer"
licenseId Beerware = "Beerware"
licenseId Bitstream_Charter = "Bitstream-Charter"
licenseId Bitstream_Vera = "Bitstream-Vera"
licenseId BitTorrent_1_0 = "BitTorrent-1.0"
licenseId BitTorrent_1_1 = "BitTorrent-1.1"
licenseId Blessing = "blessing"
licenseId BlueOak_1_0_0 = "BlueOak-1.0.0"
licenseId Boehm_GC = "Boehm-GC"
licenseId Borceux = "Borceux"
licenseId Brian_Gladman_2_Clause = "Brian-Gladman-2-Clause"
licenseId Brian_Gladman_3_Clause = "Brian-Gladman-3-Clause"
licenseId BSD_1_Clause = "BSD-1-Clause"
licenseId BSD_2_Clause_FreeBSD = "BSD-2-Clause-FreeBSD"
licenseId BSD_2_Clause_NetBSD = "BSD-2-Clause-NetBSD"
licenseId BSD_2_Clause_Darwin = "BSD-2-Clause-Darwin"
licenseId BSD_2_Clause_Patent = "BSD-2-Clause-Patent"
licenseId BSD_2_Clause_Views = "BSD-2-Clause-Views"
licenseId BSD_2_Clause = "BSD-2-Clause"
licenseId BSD_3_Clause_acpica = "BSD-3-Clause-acpica"
licenseId BSD_3_Clause_Attribution = "BSD-3-Clause-Attribution"
licenseId BSD_3_Clause_Clear = "BSD-3-Clause-Clear"
licenseId BSD_3_Clause_flex = "BSD-3-Clause-flex"
licenseId BSD_3_Clause_HP = "BSD-3-Clause-HP"
licenseId BSD_3_Clause_LBNL = "BSD-3-Clause-LBNL"
licenseId BSD_3_Clause_Modification = "BSD-3-Clause-Modification"
licenseId BSD_3_Clause_No_Military_License = "BSD-3-Clause-No-Military-License"
licenseId BSD_3_Clause_No_Nuclear_License_2014 = "BSD-3-Clause-No-Nuclear-License-2014"
licenseId BSD_3_Clause_No_Nuclear_License = "BSD-3-Clause-No-Nuclear-License"
licenseId BSD_3_Clause_No_Nuclear_Warranty = "BSD-3-Clause-No-Nuclear-Warranty"
licenseId BSD_3_Clause_Open_MPI = "BSD-3-Clause-Open-MPI"
licenseId BSD_3_Clause_Sun = "BSD-3-Clause-Sun"
licenseId BSD_3_Clause = "BSD-3-Clause"
licenseId BSD_4_Clause_Shortened = "BSD-4-Clause-Shortened"
licenseId BSD_4_Clause_UC = "BSD-4-Clause-UC"
licenseId BSD_4_Clause = "BSD-4-Clause"
licenseId BSD_4_3RENO = "BSD-4.3RENO"
licenseId BSD_4_3TAHOE = "BSD-4.3TAHOE"
licenseId BSD_Advertising_Acknowledgement = "BSD-Advertising-Acknowledgement"
licenseId BSD_Attribution_HPND_disclaimer = "BSD-Attribution-HPND-disclaimer"
licenseId BSD_Inferno_Nettverk = "BSD-Inferno-Nettverk"
licenseId BSD_Protection = "BSD-Protection"
licenseId BSD_Source_beginning_file = "BSD-Source-beginning-file"
licenseId BSD_Source_Code = "BSD-Source-Code"
licenseId BSD_Systemics_W3Works = "BSD-Systemics-W3Works"
licenseId BSD_Systemics = "BSD-Systemics"
licenseId BSL_1_0 = "BSL-1.0"
licenseId Bzip2_1_0_5 = "bzip2-1.0.5"
licenseId BUSL_1_1 = "BUSL-1.1"
licenseId Bzip2_1_0_6 = "bzip2-1.0.6"
licenseId C_UDA_1_0 = "C-UDA-1.0"
licenseId CAL_1_0_Combined_Work_Exception = "CAL-1.0-Combined-Work-Exception"
licenseId CAL_1_0 = "CAL-1.0"
licenseId Caldera_no_preamble = "Caldera-no-preamble"
licenseId Caldera = "Caldera"
licenseId CATOSL_1_1 = "CATOSL-1.1"
licenseId CC_BY_1_0 = "CC-BY-1.0"
licenseId CC_BY_2_0 = "CC-BY-2.0"
licenseId CC_BY_2_5_AU = "CC-BY-2.5-AU"
licenseId CC_BY_2_5 = "CC-BY-2.5"
licenseId CC_BY_3_0_AT = "CC-BY-3.0-AT"
licenseId CC_BY_3_0_AU = "CC-BY-3.0-AU"
licenseId CC_BY_3_0_DE = "CC-BY-3.0-DE"
licenseId CC_BY_3_0_IGO = "CC-BY-3.0-IGO"
licenseId CC_BY_3_0_NL = "CC-BY-3.0-NL"
licenseId CC_BY_3_0_US = "CC-BY-3.0-US"
licenseId CC_BY_3_0 = "CC-BY-3.0"
licenseId CC_BY_4_0 = "CC-BY-4.0"
licenseId CC_BY_NC_1_0 = "CC-BY-NC-1.0"
licenseId CC_BY_NC_2_0 = "CC-BY-NC-2.0"
licenseId CC_BY_NC_2_5 = "CC-BY-NC-2.5"
licenseId CC_BY_NC_3_0_DE = "CC-BY-NC-3.0-DE"
licenseId CC_BY_NC_3_0 = "CC-BY-NC-3.0"
licenseId CC_BY_NC_4_0 = "CC-BY-NC-4.0"
licenseId CC_BY_NC_ND_1_0 = "CC-BY-NC-ND-1.0"
licenseId CC_BY_NC_ND_2_0 = "CC-BY-NC-ND-2.0"
licenseId CC_BY_NC_ND_2_5 = "CC-BY-NC-ND-2.5"
licenseId CC_BY_NC_ND_3_0_DE = "CC-BY-NC-ND-3.0-DE"
licenseId CC_BY_NC_ND_3_0_IGO = "CC-BY-NC-ND-3.0-IGO"
licenseId CC_BY_NC_ND_3_0 = "CC-BY-NC-ND-3.0"
licenseId CC_BY_NC_ND_4_0 = "CC-BY-NC-ND-4.0"
licenseId CC_BY_NC_SA_1_0 = "CC-BY-NC-SA-1.0"
licenseId CC_BY_NC_SA_2_0_DE = "CC-BY-NC-SA-2.0-DE"
licenseId CC_BY_NC_SA_2_0_FR = "CC-BY-NC-SA-2.0-FR"
licenseId CC_BY_NC_SA_2_0_UK = "CC-BY-NC-SA-2.0-UK"
licenseId CC_BY_NC_SA_2_0 = "CC-BY-NC-SA-2.0"
licenseId CC_BY_NC_SA_2_5 = "CC-BY-NC-SA-2.5"
licenseId CC_BY_NC_SA_3_0_DE = "CC-BY-NC-SA-3.0-DE"
licenseId CC_BY_NC_SA_3_0_IGO = "CC-BY-NC-SA-3.0-IGO"
licenseId CC_BY_NC_SA_3_0 = "CC-BY-NC-SA-3.0"
licenseId CC_BY_NC_SA_4_0 = "CC-BY-NC-SA-4.0"
licenseId CC_BY_ND_1_0 = "CC-BY-ND-1.0"
licenseId CC_BY_ND_2_0 = "CC-BY-ND-2.0"
licenseId CC_BY_ND_2_5 = "CC-BY-ND-2.5"
licenseId CC_BY_ND_3_0_DE = "CC-BY-ND-3.0-DE"
licenseId CC_BY_ND_3_0 = "CC-BY-ND-3.0"
licenseId CC_BY_ND_4_0 = "CC-BY-ND-4.0"
licenseId CC_BY_SA_1_0 = "CC-BY-SA-1.0"
licenseId CC_BY_SA_2_0_UK = "CC-BY-SA-2.0-UK"
licenseId CC_BY_SA_2_0 = "CC-BY-SA-2.0"
licenseId CC_BY_SA_2_1_JP = "CC-BY-SA-2.1-JP"
licenseId CC_BY_SA_2_5 = "CC-BY-SA-2.5"
licenseId CC_BY_SA_3_0_AT = "CC-BY-SA-3.0-AT"
licenseId CC_BY_SA_3_0_DE = "CC-BY-SA-3.0-DE"
licenseId CC_BY_SA_3_0_IGO = "CC-BY-SA-3.0-IGO"
licenseId CC_BY_SA_3_0 = "CC-BY-SA-3.0"
licenseId CC_BY_SA_4_0 = "CC-BY-SA-4.0"
licenseId CC_PDDC = "CC-PDDC"
licenseId CC0_1_0 = "CC0-1.0"
licenseId CDDL_1_0 = "CDDL-1.0"
licenseId CDDL_1_1 = "CDDL-1.1"
licenseId CDL_1_0 = "CDL-1.0"
licenseId CDLA_Permissive_1_0 = "CDLA-Permissive-1.0"
licenseId CDLA_Permissive_2_0 = "CDLA-Permissive-2.0"
licenseId CDLA_Sharing_1_0 = "CDLA-Sharing-1.0"
licenseId CECILL_1_0 = "CECILL-1.0"
licenseId CECILL_1_1 = "CECILL-1.1"
licenseId CECILL_2_0 = "CECILL-2.0"
licenseId CECILL_2_1 = "CECILL-2.1"
licenseId CECILL_B = "CECILL-B"
licenseId CECILL_C = "CECILL-C"
licenseId CERN_OHL_1_1 = "CERN-OHL-1.1"
licenseId CERN_OHL_1_2 = "CERN-OHL-1.2"
licenseId CERN_OHL_P_2_0 = "CERN-OHL-P-2.0"
licenseId CERN_OHL_S_2_0 = "CERN-OHL-S-2.0"
licenseId CERN_OHL_W_2_0 = "CERN-OHL-W-2.0"
licenseId CFITSIO = "CFITSIO"
licenseId Check_cvs = "check-cvs"
licenseId Checkmk = "checkmk"
licenseId ClArtistic = "ClArtistic"
licenseId Clips = "Clips"
licenseId CMU_Mach_nodoc = "CMU-Mach-nodoc"
licenseId CMU_Mach = "CMU-Mach"
licenseId CNRI_Jython = "CNRI-Jython"
licenseId CNRI_Python_GPL_Compatible = "CNRI-Python-GPL-Compatible"
licenseId CNRI_Python = "CNRI-Python"
licenseId COIL_1_0 = "COIL-1.0"
licenseId Community_Spec_1_0 = "Community-Spec-1.0"
licenseId Condor_1_1 = "Condor-1.1"
licenseId Copyleft_next_0_3_0 = "copyleft-next-0.3.0"
licenseId Copyleft_next_0_3_1 = "copyleft-next-0.3.1"
licenseId Cornell_Lossless_JPEG = "Cornell-Lossless-JPEG"
licenseId CPAL_1_0 = "CPAL-1.0"
licenseId CPL_1_0 = "CPL-1.0"
licenseId CPOL_1_02 = "CPOL-1.02"
licenseId Cronyx = "Cronyx"
licenseId Crossword = "Crossword"
licenseId CrystalStacker = "CrystalStacker"
licenseId CUA_OPL_1_0 = "CUA-OPL-1.0"
licenseId Cube = "Cube"
licenseId Curl = "curl"
licenseId D_FSL_1_0 = "D-FSL-1.0"
licenseId DEC_3_Clause = "DEC-3-Clause"
licenseId Diffmark = "diffmark"
licenseId DL_DE_BY_2_0 = "DL-DE-BY-2.0"
licenseId DL_DE_ZERO_2_0 = "DL-DE-ZERO-2.0"
licenseId DOC = "DOC"
licenseId Dotseqn = "Dotseqn"
licenseId DRL_1_0 = "DRL-1.0"
licenseId DRL_1_1 = "DRL-1.1"
licenseId DSDP = "DSDP"
licenseId Dtoa = "dtoa"
licenseId Dvipdfm = "dvipdfm"
licenseId ECL_1_0 = "ECL-1.0"
licenseId ECL_2_0 = "ECL-2.0"
licenseId EFL_1_0 = "EFL-1.0"
licenseId EFL_2_0 = "EFL-2.0"
licenseId EGenix = "eGenix"
licenseId Elastic_2_0 = "Elastic-2.0"
licenseId Entessa = "Entessa"
licenseId EPICS = "EPICS"
licenseId EPL_1_0 = "EPL-1.0"
licenseId EPL_2_0 = "EPL-2.0"
licenseId ErlPL_1_1 = "ErlPL-1.1"
licenseId Etalab_2_0 = "etalab-2.0"
licenseId EUDatagrid = "EUDatagrid"
licenseId EUPL_1_0 = "EUPL-1.0"
licenseId EUPL_1_1 = "EUPL-1.1"
licenseId EUPL_1_2 = "EUPL-1.2"
licenseId Eurosym = "Eurosym"
licenseId Fair = "Fair"
licenseId FBM = "FBM"
licenseId FDK_AAC = "FDK-AAC"
licenseId Ferguson_Twofish = "Ferguson-Twofish"
licenseId Frameworx_1_0 = "Frameworx-1.0"
licenseId FreeBSD_DOC = "FreeBSD-DOC"
licenseId FreeImage = "FreeImage"
licenseId FSFAP_no_warranty_disclaimer = "FSFAP-no-warranty-disclaimer"
licenseId FSFAP = "FSFAP"
licenseId FSFULLRWD = "FSFULLRWD"
licenseId FSFULLR = "FSFULLR"
licenseId FSFUL = "FSFUL"
licenseId FTL = "FTL"
licenseId Furuseth = "Furuseth"
licenseId Fwlw = "fwlw"
licenseId GCR_docs = "GCR-docs"
licenseId GD = "GD"
licenseId GFDL_1_1_invariants_only = "GFDL-1.1-invariants-only"
licenseId GFDL_1_1_invariants_or_later = "GFDL-1.1-invariants-or-later"
licenseId GFDL_1_1_no_invariants_only = "GFDL-1.1-no-invariants-only"
licenseId GFDL_1_1_no_invariants_or_later = "GFDL-1.1-no-invariants-or-later"
licenseId GFDL_1_1_only = "GFDL-1.1-only"
licenseId GFDL_1_1_or_later = "GFDL-1.1-or-later"
licenseId GFDL_1_2_invariants_only = "GFDL-1.2-invariants-only"
licenseId GFDL_1_2_invariants_or_later = "GFDL-1.2-invariants-or-later"
licenseId GFDL_1_2_no_invariants_only = "GFDL-1.2-no-invariants-only"
licenseId GFDL_1_2_no_invariants_or_later = "GFDL-1.2-no-invariants-or-later"
licenseId GFDL_1_2_only = "GFDL-1.2-only"
licenseId GFDL_1_2_or_later = "GFDL-1.2-or-later"
licenseId GFDL_1_3_invariants_only = "GFDL-1.3-invariants-only"
licenseId GFDL_1_3_invariants_or_later = "GFDL-1.3-invariants-or-later"
licenseId GFDL_1_3_no_invariants_only = "GFDL-1.3-no-invariants-only"
licenseId GFDL_1_3_no_invariants_or_later = "GFDL-1.3-no-invariants-or-later"
licenseId GFDL_1_3_only = "GFDL-1.3-only"
licenseId GFDL_1_3_or_later = "GFDL-1.3-or-later"
licenseId Giftware = "Giftware"
licenseId GL2PS = "GL2PS"
licenseId Glide = "Glide"
licenseId Glulxe = "Glulxe"
licenseId GLWTPL = "GLWTPL"
licenseId Gnuplot = "gnuplot"
licenseId GPL_1_0_only = "GPL-1.0-only"
licenseId GPL_1_0_or_later = "GPL-1.0-or-later"
licenseId GPL_2_0_only = "GPL-2.0-only"
licenseId GPL_2_0_or_later = "GPL-2.0-or-later"
licenseId GPL_3_0_only = "GPL-3.0-only"
licenseId GPL_3_0_or_later = "GPL-3.0-or-later"
licenseId Graphics_Gems = "Graphics-Gems"
licenseId GSOAP_1_3b = "gSOAP-1.3b"
licenseId Gtkbook = "gtkbook"
licenseId HaskellReport = "HaskellReport"
licenseId Hdparm = "hdparm"
licenseId Hippocratic_2_1 = "Hippocratic-2.1"
licenseId HP_1986 = "HP-1986"
licenseId HP_1989 = "HP-1989"
licenseId HPND_DEC = "HPND-DEC"
licenseId HPND_doc_sell = "HPND-doc-sell"
licenseId HPND_doc = "HPND-doc"
licenseId HPND_export_US_modify = "HPND-export-US-modify"
licenseId HPND_export_US = "HPND-export-US"
licenseId HPND_Fenneberg_Livingston = "HPND-Fenneberg-Livingston"
licenseId HPND_INRIA_IMAG = "HPND-INRIA-IMAG"
licenseId HPND_Kevlin_Henney = "HPND-Kevlin-Henney"
licenseId HPND_Markus_Kuhn = "HPND-Markus-Kuhn"
licenseId HPND_MIT_disclaimer = "HPND-MIT-disclaimer"
licenseId HPND_Pbmplus = "HPND-Pbmplus"
licenseId HPND_sell_MIT_disclaimer_xserver = "HPND-sell-MIT-disclaimer-xserver"
licenseId HPND_sell_regexpr = "HPND-sell-regexpr"
licenseId HPND_sell_variant_MIT_disclaimer = "HPND-sell-variant-MIT-disclaimer"
licenseId HPND_sell_variant = "HPND-sell-variant"
licenseId HPND_UC = "HPND-UC"
licenseId HPND = "HPND"
licenseId HTMLTIDY = "HTMLTIDY"
licenseId IBM_pibs = "IBM-pibs"
licenseId ICU = "ICU"
licenseId IEC_Code_Components_EULA = "IEC-Code-Components-EULA"
licenseId IJG_short = "IJG-short"
licenseId IJG = "IJG"
licenseId ImageMagick = "ImageMagick"
licenseId IMatix = "iMatix"
licenseId Imlib2 = "Imlib2"
licenseId Info_ZIP = "Info-ZIP"
licenseId Inner_Net_2_0 = "Inner-Net-2.0"
licenseId Intel_ACPI = "Intel-ACPI"
licenseId Intel = "Intel"
licenseId Interbase_1_0 = "Interbase-1.0"
licenseId IPA = "IPA"
licenseId IPL_1_0 = "IPL-1.0"
licenseId ISC_Veillard = "ISC-Veillard"
licenseId ISC = "ISC"
licenseId Jam = "Jam"
licenseId JasPer_2_0 = "JasPer-2.0"
licenseId JPL_image = "JPL-image"
licenseId JPNIC = "JPNIC"
licenseId JSON = "JSON"
licenseId Kastrup = "Kastrup"
licenseId Kazlib = "Kazlib"
licenseId Knuth_CTAN = "Knuth-CTAN"
licenseId LAL_1_2 = "LAL-1.2"
licenseId LAL_1_3 = "LAL-1.3"
licenseId Latex2e_translated_notice = "Latex2e-translated-notice"
licenseId Latex2e = "Latex2e"
licenseId Leptonica = "Leptonica"
licenseId LGPL_2_0_only = "LGPL-2.0-only"
licenseId LGPL_2_0_or_later = "LGPL-2.0-or-later"
licenseId LGPL_2_1_only = "LGPL-2.1-only"
licenseId LGPL_2_1_or_later = "LGPL-2.1-or-later"
licenseId LGPL_3_0_only = "LGPL-3.0-only"
licenseId LGPL_3_0_or_later = "LGPL-3.0-or-later"
licenseId LGPLLR = "LGPLLR"
licenseId Libpng_2_0 = "libpng-2.0"
licenseId Libpng = "Libpng"
licenseId Libselinux_1_0 = "libselinux-1.0"
licenseId Libtiff = "libtiff"
licenseId Libutil_David_Nugent = "libutil-David-Nugent"
licenseId LiLiQ_P_1_1 = "LiLiQ-P-1.1"
licenseId LiLiQ_R_1_1 = "LiLiQ-R-1.1"
licenseId LiLiQ_Rplus_1_1 = "LiLiQ-Rplus-1.1"
licenseId Linux_man_pages_1_para = "Linux-man-pages-1-para"
licenseId Linux_man_pages_copyleft_2_para = "Linux-man-pages-copyleft-2-para"
licenseId Linux_man_pages_copyleft_var = "Linux-man-pages-copyleft-var"
licenseId Linux_man_pages_copyleft = "Linux-man-pages-copyleft"
licenseId Linux_OpenIB = "Linux-OpenIB"
licenseId LOOP = "LOOP"
licenseId LPD_document = "LPD-document"
licenseId LPL_1_02 = "LPL-1.02"
licenseId LPL_1_0 = "LPL-1.0"
licenseId LPPL_1_0 = "LPPL-1.0"
licenseId LPPL_1_1 = "LPPL-1.1"
licenseId LPPL_1_2 = "LPPL-1.2"
licenseId LPPL_1_3a = "LPPL-1.3a"
licenseId LPPL_1_3c = "LPPL-1.3c"
licenseId Lsof = "lsof"
licenseId Lucida_Bitmap_Fonts = "Lucida-Bitmap-Fonts"
licenseId LZMA_SDK_9_11_to_9_20 = "LZMA-SDK-9.11-to-9.20"
licenseId LZMA_SDK_9_22 = "LZMA-SDK-9.22"
licenseId Mackerras_3_Clause_acknowledgment = "Mackerras-3-Clause-acknowledgment"
licenseId Mackerras_3_Clause = "Mackerras-3-Clause"
licenseId Magaz = "magaz"
licenseId Mailprio = "mailprio"
licenseId MakeIndex = "MakeIndex"
licenseId Martin_Birgmeier = "Martin-Birgmeier"
licenseId McPhee_slideshow = "McPhee-slideshow"
licenseId Metamail = "metamail"
licenseId Minpack = "Minpack"
licenseId MirOS = "MirOS"
licenseId MIT_0 = "MIT-0"
licenseId MIT_advertising = "MIT-advertising"
licenseId MIT_CMU = "MIT-CMU"
licenseId MIT_enna = "MIT-enna"
licenseId MIT_feh = "MIT-feh"
licenseId MIT_Festival = "MIT-Festival"
licenseId MIT_Modern_Variant = "MIT-Modern-Variant"
licenseId MIT_open_group = "MIT-open-group"
licenseId MIT_testregex = "MIT-testregex"
licenseId MIT_Wu = "MIT-Wu"
licenseId MITNFA = "MITNFA"
licenseId MIT = "MIT"
licenseId MMIXware = "MMIXware"
licenseId Motosoto = "Motosoto"
licenseId MPEG_SSG = "MPEG-SSG"
licenseId Mpi_permissive = "mpi-permissive"
licenseId Mpich2 = "mpich2"
licenseId MPL_1_0 = "MPL-1.0"
licenseId MPL_1_1 = "MPL-1.1"
licenseId MPL_2_0_no_copyleft_exception = "MPL-2.0-no-copyleft-exception"
licenseId MPL_2_0 = "MPL-2.0"
licenseId Mplus = "mplus"
licenseId MS_LPL = "MS-LPL"
licenseId MS_PL = "MS-PL"
licenseId MS_RL = "MS-RL"
licenseId MTLL = "MTLL"
licenseId MulanPSL_1_0 = "MulanPSL-1.0"
licenseId MulanPSL_2_0 = "MulanPSL-2.0"
licenseId Multics = "Multics"
licenseId Mup = "Mup"
licenseId NAIST_2003 = "NAIST-2003"
licenseId NASA_1_3 = "NASA-1.3"
licenseId Naumen = "Naumen"
licenseId NBPL_1_0 = "NBPL-1.0"
licenseId NCGL_UK_2_0 = "NCGL-UK-2.0"
licenseId NCSA = "NCSA"
licenseId Net_SNMP = "Net-SNMP"
licenseId NetCDF = "NetCDF"
licenseId Newsletr = "Newsletr"
licenseId NGPL = "NGPL"
licenseId NICTA_1_0 = "NICTA-1.0"
licenseId NIST_PD_fallback = "NIST-PD-fallback"
licenseId NIST_PD = "NIST-PD"
licenseId NIST_Software = "NIST-Software"
licenseId NLOD_1_0 = "NLOD-1.0"
licenseId NLOD_2_0 = "NLOD-2.0"
licenseId NLPL = "NLPL"
licenseId Nokia = "Nokia"
licenseId NOSL = "NOSL"
licenseId Noweb = "Noweb"
licenseId NPL_1_0 = "NPL-1.0"
licenseId NPL_1_1 = "NPL-1.1"
licenseId NPOSL_3_0 = "NPOSL-3.0"
licenseId NRL = "NRL"
licenseId NTP_0 = "NTP-0"
licenseId NTP = "NTP"
licenseId O_UDA_1_0 = "O-UDA-1.0"
licenseId OCCT_PL = "OCCT-PL"
licenseId OCLC_2_0 = "OCLC-2.0"
licenseId ODbL_1_0 = "ODbL-1.0"
licenseId ODC_By_1_0 = "ODC-By-1.0"
licenseId OFFIS = "OFFIS"
licenseId OFL_1_0_no_RFN = "OFL-1.0-no-RFN"
licenseId OFL_1_0_RFN = "OFL-1.0-RFN"
licenseId OFL_1_0 = "OFL-1.0"
licenseId OFL_1_1_no_RFN = "OFL-1.1-no-RFN"
licenseId OFL_1_1_RFN = "OFL-1.1-RFN"
licenseId OFL_1_1 = "OFL-1.1"
licenseId OGC_1_0 = "OGC-1.0"
licenseId OGDL_Taiwan_1_0 = "OGDL-Taiwan-1.0"
licenseId OGL_Canada_2_0 = "OGL-Canada-2.0"
licenseId OGL_UK_1_0 = "OGL-UK-1.0"
licenseId OGL_UK_2_0 = "OGL-UK-2.0"
licenseId OGL_UK_3_0 = "OGL-UK-3.0"
licenseId OGTSL = "OGTSL"
licenseId OLDAP_1_1 = "OLDAP-1.1"
licenseId OLDAP_1_2 = "OLDAP-1.2"
licenseId OLDAP_1_3 = "OLDAP-1.3"
licenseId OLDAP_1_4 = "OLDAP-1.4"
licenseId OLDAP_2_0_1 = "OLDAP-2.0.1"
licenseId OLDAP_2_0 = "OLDAP-2.0"
licenseId OLDAP_2_1 = "OLDAP-2.1"
licenseId OLDAP_2_2_1 = "OLDAP-2.2.1"
licenseId OLDAP_2_2_2 = "OLDAP-2.2.2"
licenseId OLDAP_2_2 = "OLDAP-2.2"
licenseId OLDAP_2_3 = "OLDAP-2.3"
licenseId OLDAP_2_4 = "OLDAP-2.4"
licenseId OLDAP_2_5 = "OLDAP-2.5"
licenseId OLDAP_2_6 = "OLDAP-2.6"
licenseId OLDAP_2_7 = "OLDAP-2.7"
licenseId OLDAP_2_8 = "OLDAP-2.8"
licenseId OLFL_1_3 = "OLFL-1.3"
licenseId OML = "OML"
licenseId OpenPBS_2_3 = "OpenPBS-2.3"
licenseId OpenSSL_standalone = "OpenSSL-standalone"
licenseId OpenSSL = "OpenSSL"
licenseId OpenVision = "OpenVision"
licenseId OPL_1_0 = "OPL-1.0"
licenseId OPL_UK_3_0 = "OPL-UK-3.0"
licenseId OPUBL_1_0 = "OPUBL-1.0"
licenseId OSET_PL_2_1 = "OSET-PL-2.1"
licenseId OSL_1_0 = "OSL-1.0"
licenseId OSL_1_1 = "OSL-1.1"
licenseId OSL_2_0 = "OSL-2.0"
licenseId OSL_2_1 = "OSL-2.1"
licenseId OSL_3_0 = "OSL-3.0"
licenseId PADL = "PADL"
licenseId Parity_6_0_0 = "Parity-6.0.0"
licenseId Parity_7_0_0 = "Parity-7.0.0"
licenseId PDDL_1_0 = "PDDL-1.0"
licenseId PHP_3_01 = "PHP-3.01"
licenseId PHP_3_0 = "PHP-3.0"
licenseId Pixar = "Pixar"
licenseId Plexus = "Plexus"
licenseId Pnmstitch = "pnmstitch"
licenseId PolyForm_Noncommercial_1_0_0 = "PolyForm-Noncommercial-1.0.0"
licenseId PolyForm_Small_Business_1_0_0 = "PolyForm-Small-Business-1.0.0"
licenseId PostgreSQL = "PostgreSQL"
licenseId PSF_2_0 = "PSF-2.0"
licenseId Psfrag = "psfrag"
licenseId Psutils = "psutils"
licenseId Python_2_0_1 = "Python-2.0.1"
licenseId Python_2_0 = "Python-2.0"
licenseId Python_ldap = "python-ldap"
licenseId Qhull = "Qhull"
licenseId QPL_1_0_INRIA_2004 = "QPL-1.0-INRIA-2004"
licenseId QPL_1_0 = "QPL-1.0"
licenseId Radvd = "radvd"
licenseId Rdisc = "Rdisc"
licenseId RHeCos_1_1 = "RHeCos-1.1"
licenseId RPL_1_1 = "RPL-1.1"
licenseId RPL_1_5 = "RPL-1.5"
licenseId RPSL_1_0 = "RPSL-1.0"
licenseId RSA_MD = "RSA-MD"
licenseId RSCPL = "RSCPL"
licenseId Ruby = "Ruby"
licenseId SAX_PD_2_0 = "SAX-PD-2.0"
licenseId SAX_PD = "SAX-PD"
licenseId Saxpath = "Saxpath"
licenseId SCEA = "SCEA"
licenseId SchemeReport = "SchemeReport"
licenseId Sendmail_8_23 = "Sendmail-8.23"
licenseId Sendmail = "Sendmail"
licenseId SGI_B_1_0 = "SGI-B-1.0"
licenseId SGI_B_1_1 = "SGI-B-1.1"
licenseId SGI_B_2_0 = "SGI-B-2.0"
licenseId SGI_OpenGL = "SGI-OpenGL"
licenseId SGP4 = "SGP4"
licenseId SHL_0_51 = "SHL-0.51"
licenseId SHL_0_5 = "SHL-0.5"
licenseId SimPL_2_0 = "SimPL-2.0"
licenseId SISSL_1_2 = "SISSL-1.2"
licenseId SISSL = "SISSL"
licenseId Sleepycat = "Sleepycat"
licenseId SL = "SL"
licenseId SMLNJ = "SMLNJ"
licenseId SMPPL = "SMPPL"
licenseId SNIA = "SNIA"
licenseId Snprintf = "snprintf"
licenseId SoftSurfer = "softSurfer"
licenseId Soundex = "Soundex"
licenseId Spencer_86 = "Spencer-86"
licenseId Spencer_94 = "Spencer-94"
licenseId Spencer_99 = "Spencer-99"
licenseId SPL_1_0 = "SPL-1.0"
licenseId Ssh_keyscan = "ssh-keyscan"
licenseId SSH_OpenSSH = "SSH-OpenSSH"
licenseId SSH_short = "SSH-short"
licenseId SSLeay_standalone = "SSLeay-standalone"
licenseId SSPL_1_0 = "SSPL-1.0"
licenseId SugarCRM_1_1_3 = "SugarCRM-1.1.3"
licenseId Sun_PPP = "Sun-PPP"
licenseId SunPro = "SunPro"
licenseId SWL = "SWL"
licenseId Swrule = "swrule"
licenseId Symlinks = "Symlinks"
licenseId TAPR_OHL_1_0 = "TAPR-OHL-1.0"
licenseId TCL = "TCL"
licenseId TCP_wrappers = "TCP-wrappers"
licenseId TermReadKey = "TermReadKey"
licenseId TGPPL_1_0 = "TGPPL-1.0"
licenseId TMate = "TMate"
licenseId TORQUE_1_1 = "TORQUE-1.1"
licenseId TOSL = "TOSL"
licenseId TPDL = "TPDL"
licenseId TPL_1_0 = "TPL-1.0"
licenseId TTWL = "TTWL"
licenseId TTYP0 = "TTYP0"
licenseId TU_Berlin_1_0 = "TU-Berlin-1.0"
licenseId TU_Berlin_2_0 = "TU-Berlin-2.0"
licenseId UCAR = "UCAR"
licenseId UCL_1_0 = "UCL-1.0"
licenseId Ulem = "ulem"
licenseId UMich_Merit = "UMich-Merit"
licenseId Unicode_3_0 = "Unicode-3.0"
licenseId Unicode_DFS_2015 = "Unicode-DFS-2015"
licenseId Unicode_DFS_2016 = "Unicode-DFS-2016"
licenseId Unicode_TOU = "Unicode-TOU"
licenseId UnixCrypt = "UnixCrypt"
licenseId Unlicense = "Unlicense"
licenseId UPL_1_0 = "UPL-1.0"
licenseId URT_RLE = "URT-RLE"
licenseId Vim = "Vim"
licenseId VOSTROM = "VOSTROM"
licenseId VSL_1_0 = "VSL-1.0"
licenseId W3C_19980720 = "W3C-19980720"
licenseId W3C_20150513 = "W3C-20150513"
licenseId W3C = "W3C"
licenseId W3m = "w3m"
licenseId Watcom_1_0 = "Watcom-1.0"
licenseId Widget_Workshop = "Widget-Workshop"
licenseId Wsuipa = "Wsuipa"
licenseId WTFPL = "WTFPL"
licenseId X11_distribute_modifications_variant = "X11-distribute-modifications-variant"
licenseId X11 = "X11"
licenseId Xdebug_1_03 = "Xdebug-1.03"
licenseId Xerox = "Xerox"
licenseId Xfig = "Xfig"
licenseId XFree86_1_1 = "XFree86-1.1"
licenseId Xinetd = "xinetd"
licenseId Xkeyboard_config_Zinoviev = "xkeyboard-config-Zinoviev"
licenseId Xlock = "xlock"
licenseId Xnet = "Xnet"
licenseId Xpp = "xpp"
licenseId XSkat = "XSkat"
licenseId YPL_1_0 = "YPL-1.0"
licenseId YPL_1_1 = "YPL-1.1"
licenseId Zed = "Zed"
licenseId Zeeff = "Zeeff"
licenseId Zend_2_0 = "Zend-2.0"
licenseId Zimbra_1_3 = "Zimbra-1.3"
licenseId Zimbra_1_4 = "Zimbra-1.4"
licenseId Zlib_acknowledgement = "zlib-acknowledgement"
licenseId Zlib = "Zlib"
licenseId ZPL_1_1 = "ZPL-1.1"
licenseId ZPL_2_0 = "ZPL-2.0"
licenseId ZPL_2_1 = "ZPL-2.1"

-- | License name, e.g. @"GNU General Public License v2.0 only"@
licenseName :: LicenseId -> String
licenseName NullBSD = "BSD Zero Clause License"
licenseName AAL = "Attribution Assurance License"
licenseName Abstyles = "Abstyles License"
licenseName AdaCore_doc = "AdaCore Doc License"
licenseName Adobe_2006 = "Adobe Systems Incorporated Source Code License Agreement"
licenseName Adobe_Display_PostScript = "Adobe Display PostScript License"
licenseName Adobe_Glyph = "Adobe Glyph List License"
licenseName Adobe_Utopia = "Adobe Utopia Font License"
licenseName ADSL = "Amazon Digital Services License"
licenseName AFL_1_1 = "Academic Free License v1.1"
licenseName AFL_1_2 = "Academic Free License v1.2"
licenseName AFL_2_0 = "Academic Free License v2.0"
licenseName AFL_2_1 = "Academic Free License v2.1"
licenseName AFL_3_0 = "Academic Free License v3.0"
licenseName Afmparse = "Afmparse License"
licenseName AGPL_1_0 = "Affero General Public License v1.0"
licenseName AGPL_1_0_only = "Affero General Public License v1.0 only"
licenseName AGPL_1_0_or_later = "Affero General Public License v1.0 or later"
licenseName AGPL_3_0_only = "GNU Affero General Public License v3.0 only"
licenseName AGPL_3_0_or_later = "GNU Affero General Public License v3.0 or later"
licenseName Aladdin = "Aladdin Free Public License"
licenseName AMDPLPA = "AMD's plpa_map.c License"
licenseName AML_glslang = "AML glslang variant License"
licenseName AML = "Apple MIT License"
licenseName AMPAS = "Academy of Motion Picture Arts and Sciences BSD"
licenseName ANTLR_PD_fallback = "ANTLR Software Rights Notice with license fallback"
licenseName ANTLR_PD = "ANTLR Software Rights Notice"
licenseName Apache_1_0 = "Apache License 1.0"
licenseName Apache_1_1 = "Apache License 1.1"
licenseName Apache_2_0 = "Apache License 2.0"
licenseName APAFML = "Adobe Postscript AFM License"
licenseName APL_1_0 = "Adaptive Public License 1.0"
licenseName App_s2p = "App::s2p License"
licenseName APSL_1_0 = "Apple Public Source License 1.0"
licenseName APSL_1_1 = "Apple Public Source License 1.1"
licenseName APSL_1_2 = "Apple Public Source License 1.2"
licenseName APSL_2_0 = "Apple Public Source License 2.0"
licenseName Arphic_1999 = "Arphic Public License"
licenseName Artistic_1_0_cl8 = "Artistic License 1.0 w/clause 8"
licenseName Artistic_1_0_Perl = "Artistic License 1.0 (Perl)"
licenseName Artistic_1_0 = "Artistic License 1.0"
licenseName Artistic_2_0 = "Artistic License 2.0"
licenseName ASWF_Digital_Assets_1_0 = "ASWF Digital Assets License version 1.0"
licenseName ASWF_Digital_Assets_1_1 = "ASWF Digital Assets License 1.1"
licenseName Baekmuk = "Baekmuk License"
licenseName Bahyph = "Bahyph License"
licenseName Barr = "Barr License"
licenseName Bcrypt_Solar_Designer = "bcrypt Solar Designer License"
licenseName Beerware = "Beerware License"
licenseName Bitstream_Charter = "Bitstream Charter Font License"
licenseName Bitstream_Vera = "Bitstream Vera Font License"
licenseName BitTorrent_1_0 = "BitTorrent Open Source License v1.0"
licenseName BitTorrent_1_1 = "BitTorrent Open Source License v1.1"
licenseName Blessing = "SQLite Blessing"
licenseName BlueOak_1_0_0 = "Blue Oak Model License 1.0.0"
licenseName Boehm_GC = "Boehm-Demers-Weiser GC License"
licenseName Borceux = "Borceux license"
licenseName Brian_Gladman_2_Clause = "Brian Gladman 2-Clause License"
licenseName Brian_Gladman_3_Clause = "Brian Gladman 3-Clause License"
licenseName BSD_1_Clause = "BSD 1-Clause License"
licenseName BSD_2_Clause_FreeBSD = "BSD 2-Clause FreeBSD License"
licenseName BSD_2_Clause_NetBSD = "BSD 2-Clause NetBSD License"
licenseName BSD_2_Clause_Darwin = "BSD 2-Clause - Ian Darwin variant"
licenseName BSD_2_Clause_Patent = "BSD-2-Clause Plus Patent License"
licenseName BSD_2_Clause_Views = "BSD 2-Clause with views sentence"
licenseName BSD_2_Clause = "BSD 2-Clause \"Simplified\" License"
licenseName BSD_3_Clause_acpica = "BSD 3-Clause acpica variant"
licenseName BSD_3_Clause_Attribution = "BSD with attribution"
licenseName BSD_3_Clause_Clear = "BSD 3-Clause Clear License"
licenseName BSD_3_Clause_flex = "BSD 3-Clause Flex variant"
licenseName BSD_3_Clause_HP = "Hewlett-Packard BSD variant license"
licenseName BSD_3_Clause_LBNL = "Lawrence Berkeley National Labs BSD variant license"
licenseName BSD_3_Clause_Modification = "BSD 3-Clause Modification"
licenseName BSD_3_Clause_No_Military_License = "BSD 3-Clause No Military License"
licenseName BSD_3_Clause_No_Nuclear_License_2014 = "BSD 3-Clause No Nuclear License 2014"
licenseName BSD_3_Clause_No_Nuclear_License = "BSD 3-Clause No Nuclear License"
licenseName BSD_3_Clause_No_Nuclear_Warranty = "BSD 3-Clause No Nuclear Warranty"
licenseName BSD_3_Clause_Open_MPI = "BSD 3-Clause Open MPI variant"
licenseName BSD_3_Clause_Sun = "BSD 3-Clause Sun Microsystems"
licenseName BSD_3_Clause = "BSD 3-Clause \"New\" or \"Revised\" License"
licenseName BSD_4_Clause_Shortened = "BSD 4 Clause Shortened"
licenseName BSD_4_Clause_UC = "BSD-4-Clause (University of California-Specific)"
licenseName BSD_4_Clause = "BSD 4-Clause \"Original\" or \"Old\" License"
licenseName BSD_4_3RENO = "BSD 4.3 RENO License"
licenseName BSD_4_3TAHOE = "BSD 4.3 TAHOE License"
licenseName BSD_Advertising_Acknowledgement = "BSD Advertising Acknowledgement License"
licenseName BSD_Attribution_HPND_disclaimer = "BSD with Attribution and HPND disclaimer"
licenseName BSD_Inferno_Nettverk = "BSD-Inferno-Nettverk"
licenseName BSD_Protection = "BSD Protection License"
licenseName BSD_Source_beginning_file = "BSD Source Code Attribution - beginning of file variant"
licenseName BSD_Source_Code = "BSD Source Code Attribution"
licenseName BSD_Systemics_W3Works = "Systemics W3Works BSD variant license"
licenseName BSD_Systemics = "Systemics BSD variant license"
licenseName BSL_1_0 = "Boost Software License 1.0"
licenseName Bzip2_1_0_5 = "bzip2 and libbzip2 License v1.0.5"
licenseName BUSL_1_1 = "Business Source License 1.1"
licenseName Bzip2_1_0_6 = "bzip2 and libbzip2 License v1.0.6"
licenseName C_UDA_1_0 = "Computational Use of Data Agreement v1.0"
licenseName CAL_1_0_Combined_Work_Exception = "Cryptographic Autonomy License 1.0 (Combined Work Exception)"
licenseName CAL_1_0 = "Cryptographic Autonomy License 1.0"
licenseName Caldera_no_preamble = "Caldera License (without preamble)"
licenseName Caldera = "Caldera License"
licenseName CATOSL_1_1 = "Computer Associates Trusted Open Source License 1.1"
licenseName CC_BY_1_0 = "Creative Commons Attribution 1.0 Generic"
licenseName CC_BY_2_0 = "Creative Commons Attribution 2.0 Generic"
licenseName CC_BY_2_5_AU = "Creative Commons Attribution 2.5 Australia"
licenseName CC_BY_2_5 = "Creative Commons Attribution 2.5 Generic"
licenseName CC_BY_3_0_AT = "Creative Commons Attribution 3.0 Austria"
licenseName CC_BY_3_0_AU = "Creative Commons Attribution 3.0 Australia"
licenseName CC_BY_3_0_DE = "Creative Commons Attribution 3.0 Germany"
licenseName CC_BY_3_0_IGO = "Creative Commons Attribution 3.0 IGO"
licenseName CC_BY_3_0_NL = "Creative Commons Attribution 3.0 Netherlands"
licenseName CC_BY_3_0_US = "Creative Commons Attribution 3.0 United States"
licenseName CC_BY_3_0 = "Creative Commons Attribution 3.0 Unported"
licenseName CC_BY_4_0 = "Creative Commons Attribution 4.0 International"
licenseName CC_BY_NC_1_0 = "Creative Commons Attribution Non Commercial 1.0 Generic"
licenseName CC_BY_NC_2_0 = "Creative Commons Attribution Non Commercial 2.0 Generic"
licenseName CC_BY_NC_2_5 = "Creative Commons Attribution Non Commercial 2.5 Generic"
licenseName CC_BY_NC_3_0_DE = "Creative Commons Attribution Non Commercial 3.0 Germany"
licenseName CC_BY_NC_3_0 = "Creative Commons Attribution Non Commercial 3.0 Unported"
licenseName CC_BY_NC_4_0 = "Creative Commons Attribution Non Commercial 4.0 International"
licenseName CC_BY_NC_ND_1_0 = "Creative Commons Attribution Non Commercial No Derivatives 1.0 Generic"
licenseName CC_BY_NC_ND_2_0 = "Creative Commons Attribution Non Commercial No Derivatives 2.0 Generic"
licenseName CC_BY_NC_ND_2_5 = "Creative Commons Attribution Non Commercial No Derivatives 2.5 Generic"
licenseName CC_BY_NC_ND_3_0_DE = "Creative Commons Attribution Non Commercial No Derivatives 3.0 Germany"
licenseName CC_BY_NC_ND_3_0_IGO = "Creative Commons Attribution Non Commercial No Derivatives 3.0 IGO"
licenseName CC_BY_NC_ND_3_0 = "Creative Commons Attribution Non Commercial No Derivatives 3.0 Unported"
licenseName CC_BY_NC_ND_4_0 = "Creative Commons Attribution Non Commercial No Derivatives 4.0 International"
licenseName CC_BY_NC_SA_1_0 = "Creative Commons Attribution Non Commercial Share Alike 1.0 Generic"
licenseName CC_BY_NC_SA_2_0_DE = "Creative Commons Attribution Non Commercial Share Alike 2.0 Germany"
licenseName CC_BY_NC_SA_2_0_FR = "Creative Commons Attribution-NonCommercial-ShareAlike 2.0 France"
licenseName CC_BY_NC_SA_2_0_UK = "Creative Commons Attribution Non Commercial Share Alike 2.0 England and Wales"
licenseName CC_BY_NC_SA_2_0 = "Creative Commons Attribution Non Commercial Share Alike 2.0 Generic"
licenseName CC_BY_NC_SA_2_5 = "Creative Commons Attribution Non Commercial Share Alike 2.5 Generic"
licenseName CC_BY_NC_SA_3_0_DE = "Creative Commons Attribution Non Commercial Share Alike 3.0 Germany"
licenseName CC_BY_NC_SA_3_0_IGO = "Creative Commons Attribution Non Commercial Share Alike 3.0 IGO"
licenseName CC_BY_NC_SA_3_0 = "Creative Commons Attribution Non Commercial Share Alike 3.0 Unported"
licenseName CC_BY_NC_SA_4_0 = "Creative Commons Attribution Non Commercial Share Alike 4.0 International"
licenseName CC_BY_ND_1_0 = "Creative Commons Attribution No Derivatives 1.0 Generic"
licenseName CC_BY_ND_2_0 = "Creative Commons Attribution No Derivatives 2.0 Generic"
licenseName CC_BY_ND_2_5 = "Creative Commons Attribution No Derivatives 2.5 Generic"
licenseName CC_BY_ND_3_0_DE = "Creative Commons Attribution No Derivatives 3.0 Germany"
licenseName CC_BY_ND_3_0 = "Creative Commons Attribution No Derivatives 3.0 Unported"
licenseName CC_BY_ND_4_0 = "Creative Commons Attribution No Derivatives 4.0 International"
licenseName CC_BY_SA_1_0 = "Creative Commons Attribution Share Alike 1.0 Generic"
licenseName CC_BY_SA_2_0_UK = "Creative Commons Attribution Share Alike 2.0 England and Wales"
licenseName CC_BY_SA_2_0 = "Creative Commons Attribution Share Alike 2.0 Generic"
licenseName CC_BY_SA_2_1_JP = "Creative Commons Attribution Share Alike 2.1 Japan"
licenseName CC_BY_SA_2_5 = "Creative Commons Attribution Share Alike 2.5 Generic"
licenseName CC_BY_SA_3_0_AT = "Creative Commons Attribution Share Alike 3.0 Austria"
licenseName CC_BY_SA_3_0_DE = "Creative Commons Attribution Share Alike 3.0 Germany"
licenseName CC_BY_SA_3_0_IGO = "Creative Commons Attribution-ShareAlike 3.0 IGO"
licenseName CC_BY_SA_3_0 = "Creative Commons Attribution Share Alike 3.0 Unported"
licenseName CC_BY_SA_4_0 = "Creative Commons Attribution Share Alike 4.0 International"
licenseName CC_PDDC = "Creative Commons Public Domain Dedication and Certification"
licenseName CC0_1_0 = "Creative Commons Zero v1.0 Universal"
licenseName CDDL_1_0 = "Common Development and Distribution License 1.0"
licenseName CDDL_1_1 = "Common Development and Distribution License 1.1"
licenseName CDL_1_0 = "Common Documentation License 1.0"
licenseName CDLA_Permissive_1_0 = "Community Data License Agreement Permissive 1.0"
licenseName CDLA_Permissive_2_0 = "Community Data License Agreement Permissive 2.0"
licenseName CDLA_Sharing_1_0 = "Community Data License Agreement Sharing 1.0"
licenseName CECILL_1_0 = "CeCILL Free Software License Agreement v1.0"
licenseName CECILL_1_1 = "CeCILL Free Software License Agreement v1.1"
licenseName CECILL_2_0 = "CeCILL Free Software License Agreement v2.0"
licenseName CECILL_2_1 = "CeCILL Free Software License Agreement v2.1"
licenseName CECILL_B = "CeCILL-B Free Software License Agreement"
licenseName CECILL_C = "CeCILL-C Free Software License Agreement"
licenseName CERN_OHL_1_1 = "CERN Open Hardware Licence v1.1"
licenseName CERN_OHL_1_2 = "CERN Open Hardware Licence v1.2"
licenseName CERN_OHL_P_2_0 = "CERN Open Hardware Licence Version 2 - Permissive"
licenseName CERN_OHL_S_2_0 = "CERN Open Hardware Licence Version 2 - Strongly Reciprocal"
licenseName CERN_OHL_W_2_0 = "CERN Open Hardware Licence Version 2 - Weakly Reciprocal"
licenseName CFITSIO = "CFITSIO License"
licenseName Check_cvs = "check-cvs License"
licenseName Checkmk = "Checkmk License"
licenseName ClArtistic = "Clarified Artistic License"
licenseName Clips = "Clips License"
licenseName CMU_Mach_nodoc = "CMU    Mach - no notices-in-documentation variant"
licenseName CMU_Mach = "CMU Mach License"
licenseName CNRI_Jython = "CNRI Jython License"
licenseName CNRI_Python_GPL_Compatible = "CNRI Python Open Source GPL Compatible License Agreement"
licenseName CNRI_Python = "CNRI Python License"
licenseName COIL_1_0 = "Copyfree Open Innovation License"
licenseName Community_Spec_1_0 = "Community Specification License 1.0"
licenseName Condor_1_1 = "Condor Public License v1.1"
licenseName Copyleft_next_0_3_0 = "copyleft-next 0.3.0"
licenseName Copyleft_next_0_3_1 = "copyleft-next 0.3.1"
licenseName Cornell_Lossless_JPEG = "Cornell Lossless JPEG License"
licenseName CPAL_1_0 = "Common Public Attribution License 1.0"
licenseName CPL_1_0 = "Common Public License 1.0"
licenseName CPOL_1_02 = "Code Project Open License 1.02"
licenseName Cronyx = "Cronyx License"
licenseName Crossword = "Crossword License"
licenseName CrystalStacker = "CrystalStacker License"
licenseName CUA_OPL_1_0 = "CUA Office Public License v1.0"
licenseName Cube = "Cube License"
licenseName Curl = "curl License"
licenseName D_FSL_1_0 = "Deutsche Freie Software Lizenz"
licenseName DEC_3_Clause = "DEC 3-Clause License"
licenseName Diffmark = "diffmark license"
licenseName DL_DE_BY_2_0 = "Data licence Germany \8211 attribution \8211 version 2.0"
licenseName DL_DE_ZERO_2_0 = "Data licence Germany \8211 zero \8211 version 2.0"
licenseName DOC = "DOC License"
licenseName Dotseqn = "Dotseqn License"
licenseName DRL_1_0 = "Detection Rule License 1.0"
licenseName DRL_1_1 = "Detection Rule License 1.1"
licenseName DSDP = "DSDP License"
licenseName Dtoa = "David M. Gay dtoa License"
licenseName Dvipdfm = "dvipdfm License"
licenseName ECL_1_0 = "Educational Community License v1.0"
licenseName ECL_2_0 = "Educational Community License v2.0"
licenseName EFL_1_0 = "Eiffel Forum License v1.0"
licenseName EFL_2_0 = "Eiffel Forum License v2.0"
licenseName EGenix = "eGenix.com Public License 1.1.0"
licenseName Elastic_2_0 = "Elastic License 2.0"
licenseName Entessa = "Entessa Public License v1.0"
licenseName EPICS = "EPICS Open License"
licenseName EPL_1_0 = "Eclipse Public License 1.0"
licenseName EPL_2_0 = "Eclipse Public License 2.0"
licenseName ErlPL_1_1 = "Erlang Public License v1.1"
licenseName Etalab_2_0 = "Etalab Open License 2.0"
licenseName EUDatagrid = "EU DataGrid Software License"
licenseName EUPL_1_0 = "European Union Public License 1.0"
licenseName EUPL_1_1 = "European Union Public License 1.1"
licenseName EUPL_1_2 = "European Union Public License 1.2"
licenseName Eurosym = "Eurosym License"
licenseName Fair = "Fair License"
licenseName FBM = "Fuzzy Bitmap License"
licenseName FDK_AAC = "Fraunhofer FDK AAC Codec Library"
licenseName Ferguson_Twofish = "Ferguson Twofish License"
licenseName Frameworx_1_0 = "Frameworx Open License 1.0"
licenseName FreeBSD_DOC = "FreeBSD Documentation License"
licenseName FreeImage = "FreeImage Public License v1.0"
licenseName FSFAP_no_warranty_disclaimer = "FSF All Permissive License (without Warranty)"
licenseName FSFAP = "FSF All Permissive License"
licenseName FSFULLRWD = "FSF Unlimited License (With License Retention and Warranty Disclaimer)"
licenseName FSFULLR = "FSF Unlimited License (with License Retention)"
licenseName FSFUL = "FSF Unlimited License"
licenseName FTL = "Freetype Project License"
licenseName Furuseth = "Furuseth License"
licenseName Fwlw = "fwlw License"
licenseName GCR_docs = "Gnome GCR Documentation License"
licenseName GD = "GD License"
licenseName GFDL_1_1_invariants_only = "GNU Free Documentation License v1.1 only - invariants"
licenseName GFDL_1_1_invariants_or_later = "GNU Free Documentation License v1.1 or later - invariants"
licenseName GFDL_1_1_no_invariants_only = "GNU Free Documentation License v1.1 only - no invariants"
licenseName GFDL_1_1_no_invariants_or_later = "GNU Free Documentation License v1.1 or later - no invariants"
licenseName GFDL_1_1_only = "GNU Free Documentation License v1.1 only"
licenseName GFDL_1_1_or_later = "GNU Free Documentation License v1.1 or later"
licenseName GFDL_1_2_invariants_only = "GNU Free Documentation License v1.2 only - invariants"
licenseName GFDL_1_2_invariants_or_later = "GNU Free Documentation License v1.2 or later - invariants"
licenseName GFDL_1_2_no_invariants_only = "GNU Free Documentation License v1.2 only - no invariants"
licenseName GFDL_1_2_no_invariants_or_later = "GNU Free Documentation License v1.2 or later - no invariants"
licenseName GFDL_1_2_only = "GNU Free Documentation License v1.2 only"
licenseName GFDL_1_2_or_later = "GNU Free Documentation License v1.2 or later"
licenseName GFDL_1_3_invariants_only = "GNU Free Documentation License v1.3 only - invariants"
licenseName GFDL_1_3_invariants_or_later = "GNU Free Documentation License v1.3 or later - invariants"
licenseName GFDL_1_3_no_invariants_only = "GNU Free Documentation License v1.3 only - no invariants"
licenseName GFDL_1_3_no_invariants_or_later = "GNU Free Documentation License v1.3 or later - no invariants"
licenseName GFDL_1_3_only = "GNU Free Documentation License v1.3 only"
licenseName GFDL_1_3_or_later = "GNU Free Documentation License v1.3 or later"
licenseName Giftware = "Giftware License"
licenseName GL2PS = "GL2PS License"
licenseName Glide = "3dfx Glide License"
licenseName Glulxe = "Glulxe License"
licenseName GLWTPL = "Good Luck With That Public License"
licenseName Gnuplot = "gnuplot License"
licenseName GPL_1_0_only = "GNU General Public License v1.0 only"
licenseName GPL_1_0_or_later = "GNU General Public License v1.0 or later"
licenseName GPL_2_0_only = "GNU General Public License v2.0 only"
licenseName GPL_2_0_or_later = "GNU General Public License v2.0 or later"
licenseName GPL_3_0_only = "GNU General Public License v3.0 only"
licenseName GPL_3_0_or_later = "GNU General Public License v3.0 or later"
licenseName Graphics_Gems = "Graphics Gems License"
licenseName GSOAP_1_3b = "gSOAP Public License v1.3b"
licenseName Gtkbook = "gtkbook License"
licenseName HaskellReport = "Haskell Language Report License"
licenseName Hdparm = "hdparm License"
licenseName Hippocratic_2_1 = "Hippocratic License 2.1"
licenseName HP_1986 = "Hewlett-Packard 1986 License"
licenseName HP_1989 = "Hewlett-Packard 1989 License"
licenseName HPND_DEC = "Historical Permission Notice and Disclaimer - DEC variant"
licenseName HPND_doc_sell = "Historical Permission Notice and Disclaimer - documentation sell variant"
licenseName HPND_doc = "Historical Permission Notice and Disclaimer - documentation variant"
licenseName HPND_export_US_modify = "HPND with US Government export control warning and modification rqmt"
licenseName HPND_export_US = "HPND with US Government export control warning"
licenseName HPND_Fenneberg_Livingston = "Historical Permission Notice and Disclaimer - Fenneberg-Livingston variant"
licenseName HPND_INRIA_IMAG = "Historical Permission Notice and Disclaimer    - INRIA-IMAG variant"
licenseName HPND_Kevlin_Henney = "Historical Permission Notice and Disclaimer - Kevlin Henney variant"
licenseName HPND_Markus_Kuhn = "Historical Permission Notice and Disclaimer - Markus Kuhn variant"
licenseName HPND_MIT_disclaimer = "Historical Permission Notice and Disclaimer with MIT disclaimer"
licenseName HPND_Pbmplus = "Historical Permission Notice and Disclaimer - Pbmplus variant"
licenseName HPND_sell_MIT_disclaimer_xserver = "Historical Permission Notice and Disclaimer - sell xserver variant with MIT disclaimer"
licenseName HPND_sell_regexpr = "Historical Permission Notice and Disclaimer - sell regexpr variant"
licenseName HPND_sell_variant_MIT_disclaimer = "HPND sell variant with MIT disclaimer"
licenseName HPND_sell_variant = "Historical Permission Notice and Disclaimer - sell variant"
licenseName HPND_UC = "Historical Permission Notice and Disclaimer - University of California variant"
licenseName HPND = "Historical Permission Notice and Disclaimer"
licenseName HTMLTIDY = "HTML Tidy License"
licenseName IBM_pibs = "IBM PowerPC Initialization and Boot Software"
licenseName ICU = "ICU License"
licenseName IEC_Code_Components_EULA = "IEC    Code Components End-user licence agreement"
licenseName IJG_short = "Independent JPEG Group License - short"
licenseName IJG = "Independent JPEG Group License"
licenseName ImageMagick = "ImageMagick License"
licenseName IMatix = "iMatix Standard Function Library Agreement"
licenseName Imlib2 = "Imlib2 License"
licenseName Info_ZIP = "Info-ZIP License"
licenseName Inner_Net_2_0 = "Inner Net License v2.0"
licenseName Intel_ACPI = "Intel ACPI Software License Agreement"
licenseName Intel = "Intel Open Source License"
licenseName Interbase_1_0 = "Interbase Public License v1.0"
licenseName IPA = "IPA Font License"
licenseName IPL_1_0 = "IBM Public License v1.0"
licenseName ISC_Veillard = "ISC Veillard variant"
licenseName ISC = "ISC License"
licenseName Jam = "Jam License"
licenseName JasPer_2_0 = "JasPer License"
licenseName JPL_image = "JPL Image Use Policy"
licenseName JPNIC = "Japan Network Information Center License"
licenseName JSON = "JSON License"
licenseName Kastrup = "Kastrup License"
licenseName Kazlib = "Kazlib License"
licenseName Knuth_CTAN = "Knuth CTAN License"
licenseName LAL_1_2 = "Licence Art Libre 1.2"
licenseName LAL_1_3 = "Licence Art Libre 1.3"
licenseName Latex2e_translated_notice = "Latex2e with translated notice permission"
licenseName Latex2e = "Latex2e License"
licenseName Leptonica = "Leptonica License"
licenseName LGPL_2_0_only = "GNU Library General Public License v2 only"
licenseName LGPL_2_0_or_later = "GNU Library General Public License v2 or later"
licenseName LGPL_2_1_only = "GNU Lesser General Public License v2.1 only"
licenseName LGPL_2_1_or_later = "GNU Lesser General Public License v2.1 or later"
licenseName LGPL_3_0_only = "GNU Lesser General Public License v3.0 only"
licenseName LGPL_3_0_or_later = "GNU Lesser General Public License v3.0 or later"
licenseName LGPLLR = "Lesser General Public License For Linguistic Resources"
licenseName Libpng_2_0 = "PNG Reference Library version 2"
licenseName Libpng = "libpng License"
licenseName Libselinux_1_0 = "libselinux public domain notice"
licenseName Libtiff = "libtiff License"
licenseName Libutil_David_Nugent = "libutil David Nugent License"
licenseName LiLiQ_P_1_1 = "Licence Libre du Qu\233bec \8211 Permissive version 1.1"
licenseName LiLiQ_R_1_1 = "Licence Libre du Qu\233bec \8211 R\233ciprocit\233 version 1.1"
licenseName LiLiQ_Rplus_1_1 = "Licence Libre du Qu\233bec \8211 R\233ciprocit\233 forte version 1.1"
licenseName Linux_man_pages_1_para = "Linux man-pages - 1 paragraph"
licenseName Linux_man_pages_copyleft_2_para = "Linux man-pages Copyleft - 2 paragraphs"
licenseName Linux_man_pages_copyleft_var = "Linux man-pages Copyleft Variant"
licenseName Linux_man_pages_copyleft = "Linux man-pages Copyleft"
licenseName Linux_OpenIB = "Linux Kernel Variant of OpenIB.org license"
licenseName LOOP = "Common Lisp LOOP License"
licenseName LPD_document = "LPD Documentation License"
licenseName LPL_1_02 = "Lucent Public License v1.02"
licenseName LPL_1_0 = "Lucent Public License Version 1.0"
licenseName LPPL_1_0 = "LaTeX Project Public License v1.0"
licenseName LPPL_1_1 = "LaTeX Project Public License v1.1"
licenseName LPPL_1_2 = "LaTeX Project Public License v1.2"
licenseName LPPL_1_3a = "LaTeX Project Public License v1.3a"
licenseName LPPL_1_3c = "LaTeX Project Public License v1.3c"
licenseName Lsof = "lsof License"
licenseName Lucida_Bitmap_Fonts = "Lucida Bitmap Fonts License"
licenseName LZMA_SDK_9_11_to_9_20 = "LZMA SDK License (versions 9.11 to 9.20)"
licenseName LZMA_SDK_9_22 = "LZMA SDK License (versions 9.22 and beyond)"
licenseName Mackerras_3_Clause_acknowledgment = "Mackerras 3-Clause - acknowledgment variant"
licenseName Mackerras_3_Clause = "Mackerras 3-Clause License"
licenseName Magaz = "magaz License"
licenseName Mailprio = "mailprio License"
licenseName MakeIndex = "MakeIndex License"
licenseName Martin_Birgmeier = "Martin Birgmeier License"
licenseName McPhee_slideshow = "McPhee Slideshow License"
licenseName Metamail = "metamail License"
licenseName Minpack = "Minpack License"
licenseName MirOS = "The MirOS Licence"
licenseName MIT_0 = "MIT No Attribution"
licenseName MIT_advertising = "Enlightenment License (e16)"
licenseName MIT_CMU = "CMU License"
licenseName MIT_enna = "enna License"
licenseName MIT_feh = "feh License"
licenseName MIT_Festival = "MIT Festival Variant"
licenseName MIT_Modern_Variant = "MIT License Modern Variant"
licenseName MIT_open_group = "MIT Open Group variant"
licenseName MIT_testregex = "MIT testregex Variant"
licenseName MIT_Wu = "MIT Tom Wu Variant"
licenseName MITNFA = "MIT +no-false-attribs license"
licenseName MIT = "MIT License"
licenseName MMIXware = "MMIXware License"
licenseName Motosoto = "Motosoto License"
licenseName MPEG_SSG = "MPEG Software Simulation"
licenseName Mpi_permissive = "mpi Permissive License"
licenseName Mpich2 = "mpich2 License"
licenseName MPL_1_0 = "Mozilla Public License 1.0"
licenseName MPL_1_1 = "Mozilla Public License 1.1"
licenseName MPL_2_0_no_copyleft_exception = "Mozilla Public License 2.0 (no copyleft exception)"
licenseName MPL_2_0 = "Mozilla Public License 2.0"
licenseName Mplus = "mplus Font License"
licenseName MS_LPL = "Microsoft Limited Public License"
licenseName MS_PL = "Microsoft Public License"
licenseName MS_RL = "Microsoft Reciprocal License"
licenseName MTLL = "Matrix Template Library License"
licenseName MulanPSL_1_0 = "Mulan Permissive Software License, Version 1"
licenseName MulanPSL_2_0 = "Mulan Permissive Software License, Version 2"
licenseName Multics = "Multics License"
licenseName Mup = "Mup License"
licenseName NAIST_2003 = "Nara Institute of Science and Technology License (2003)"
licenseName NASA_1_3 = "NASA Open Source Agreement 1.3"
licenseName Naumen = "Naumen Public License"
licenseName NBPL_1_0 = "Net Boolean Public License v1"
licenseName NCGL_UK_2_0 = "Non-Commercial Government Licence"
licenseName NCSA = "University of Illinois/NCSA Open Source License"
licenseName Net_SNMP = "Net-SNMP License"
licenseName NetCDF = "NetCDF license"
licenseName Newsletr = "Newsletr License"
licenseName NGPL = "Nethack General Public License"
licenseName NICTA_1_0 = "NICTA Public Software License, Version 1.0"
licenseName NIST_PD_fallback = "NIST Public Domain Notice with license fallback"
licenseName NIST_PD = "NIST Public Domain Notice"
licenseName NIST_Software = "NIST Software License"
licenseName NLOD_1_0 = "Norwegian Licence for Open Government Data (NLOD) 1.0"
licenseName NLOD_2_0 = "Norwegian Licence for Open Government Data (NLOD) 2.0"
licenseName NLPL = "No Limit Public License"
licenseName Nokia = "Nokia Open Source License"
licenseName NOSL = "Netizen Open Source License"
licenseName Noweb = "Noweb License"
licenseName NPL_1_0 = "Netscape Public License v1.0"
licenseName NPL_1_1 = "Netscape Public License v1.1"
licenseName NPOSL_3_0 = "Non-Profit Open Software License 3.0"
licenseName NRL = "NRL License"
licenseName NTP_0 = "NTP No Attribution"
licenseName NTP = "NTP License"
licenseName O_UDA_1_0 = "Open Use of Data Agreement v1.0"
licenseName OCCT_PL = "Open CASCADE Technology Public License"
licenseName OCLC_2_0 = "OCLC Research Public License 2.0"
licenseName ODbL_1_0 = "Open Data Commons Open Database License v1.0"
licenseName ODC_By_1_0 = "Open Data Commons Attribution License v1.0"
licenseName OFFIS = "OFFIS License"
licenseName OFL_1_0_no_RFN = "SIL Open Font License 1.0 with no Reserved Font Name"
licenseName OFL_1_0_RFN = "SIL Open Font License 1.0 with Reserved Font Name"
licenseName OFL_1_0 = "SIL Open Font License 1.0"
licenseName OFL_1_1_no_RFN = "SIL Open Font License 1.1 with no Reserved Font Name"
licenseName OFL_1_1_RFN = "SIL Open Font License 1.1 with Reserved Font Name"
licenseName OFL_1_1 = "SIL Open Font License 1.1"
licenseName OGC_1_0 = "OGC Software License, Version 1.0"
licenseName OGDL_Taiwan_1_0 = "Taiwan Open Government Data License, version 1.0"
licenseName OGL_Canada_2_0 = "Open Government Licence - Canada"
licenseName OGL_UK_1_0 = "Open Government Licence v1.0"
licenseName OGL_UK_2_0 = "Open Government Licence v2.0"
licenseName OGL_UK_3_0 = "Open Government Licence v3.0"
licenseName OGTSL = "Open Group Test Suite License"
licenseName OLDAP_1_1 = "Open LDAP Public License v1.1"
licenseName OLDAP_1_2 = "Open LDAP Public License v1.2"
licenseName OLDAP_1_3 = "Open LDAP Public License v1.3"
licenseName OLDAP_1_4 = "Open LDAP Public License v1.4"
licenseName OLDAP_2_0_1 = "Open LDAP Public License v2.0.1"
licenseName OLDAP_2_0 = "Open LDAP Public License v2.0 (or possibly 2.0A and 2.0B)"
licenseName OLDAP_2_1 = "Open LDAP Public License v2.1"
licenseName OLDAP_2_2_1 = "Open LDAP Public License v2.2.1"
licenseName OLDAP_2_2_2 = "Open LDAP Public License 2.2.2"
licenseName OLDAP_2_2 = "Open LDAP Public License v2.2"
licenseName OLDAP_2_3 = "Open LDAP Public License v2.3"
licenseName OLDAP_2_4 = "Open LDAP Public License v2.4"
licenseName OLDAP_2_5 = "Open LDAP Public License v2.5"
licenseName OLDAP_2_6 = "Open LDAP Public License v2.6"
licenseName OLDAP_2_7 = "Open LDAP Public License v2.7"
licenseName OLDAP_2_8 = "Open LDAP Public License v2.8"
licenseName OLFL_1_3 = "Open Logistics Foundation License Version 1.3"
licenseName OML = "Open Market License"
licenseName OpenPBS_2_3 = "OpenPBS v2.3 Software License"
licenseName OpenSSL_standalone = "OpenSSL License - standalone"
licenseName OpenSSL = "OpenSSL License"
licenseName OpenVision = "OpenVision License"
licenseName OPL_1_0 = "Open Public License v1.0"
licenseName OPL_UK_3_0 = "United    Kingdom Open Parliament Licence v3.0"
licenseName OPUBL_1_0 = "Open Publication License v1.0"
licenseName OSET_PL_2_1 = "OSET Public License version 2.1"
licenseName OSL_1_0 = "Open Software License 1.0"
licenseName OSL_1_1 = "Open Software License 1.1"
licenseName OSL_2_0 = "Open Software License 2.0"
licenseName OSL_2_1 = "Open Software License 2.1"
licenseName OSL_3_0 = "Open Software License 3.0"
licenseName PADL = "PADL License"
licenseName Parity_6_0_0 = "The Parity Public License 6.0.0"
licenseName Parity_7_0_0 = "The Parity Public License 7.0.0"
licenseName PDDL_1_0 = "Open Data Commons Public Domain Dedication & License 1.0"
licenseName PHP_3_01 = "PHP License v3.01"
licenseName PHP_3_0 = "PHP License v3.0"
licenseName Pixar = "Pixar License"
licenseName Plexus = "Plexus Classworlds License"
licenseName Pnmstitch = "pnmstitch License"
licenseName PolyForm_Noncommercial_1_0_0 = "PolyForm Noncommercial License 1.0.0"
licenseName PolyForm_Small_Business_1_0_0 = "PolyForm Small Business License 1.0.0"
licenseName PostgreSQL = "PostgreSQL License"
licenseName PSF_2_0 = "Python Software Foundation License 2.0"
licenseName Psfrag = "psfrag License"
licenseName Psutils = "psutils License"
licenseName Python_2_0_1 = "Python License 2.0.1"
licenseName Python_2_0 = "Python License 2.0"
licenseName Python_ldap = "Python ldap License"
licenseName Qhull = "Qhull License"
licenseName QPL_1_0_INRIA_2004 = "Q Public License 1.0 - INRIA 2004 variant"
licenseName QPL_1_0 = "Q Public License 1.0"
licenseName Radvd = "radvd License"
licenseName Rdisc = "Rdisc License"
licenseName RHeCos_1_1 = "Red Hat eCos Public License v1.1"
licenseName RPL_1_1 = "Reciprocal Public License 1.1"
licenseName RPL_1_5 = "Reciprocal Public License 1.5"
licenseName RPSL_1_0 = "RealNetworks Public Source License v1.0"
licenseName RSA_MD = "RSA Message-Digest License"
licenseName RSCPL = "Ricoh Source Code Public License"
licenseName Ruby = "Ruby License"
licenseName SAX_PD_2_0 = "Sax Public Domain Notice 2.0"
licenseName SAX_PD = "Sax Public Domain Notice"
licenseName Saxpath = "Saxpath License"
licenseName SCEA = "SCEA Shared Source License"
licenseName SchemeReport = "Scheme Language Report License"
licenseName Sendmail_8_23 = "Sendmail License 8.23"
licenseName Sendmail = "Sendmail License"
licenseName SGI_B_1_0 = "SGI Free Software License B v1.0"
licenseName SGI_B_1_1 = "SGI Free Software License B v1.1"
licenseName SGI_B_2_0 = "SGI Free Software License B v2.0"
licenseName SGI_OpenGL = "SGI OpenGL License"
licenseName SGP4 = "SGP4 Permission Notice"
licenseName SHL_0_51 = "Solderpad Hardware License, Version 0.51"
licenseName SHL_0_5 = "Solderpad Hardware License v0.5"
licenseName SimPL_2_0 = "Simple Public License 2.0"
licenseName SISSL_1_2 = "Sun Industry Standards Source License v1.2"
licenseName SISSL = "Sun Industry Standards Source License v1.1"
licenseName Sleepycat = "Sleepycat License"
licenseName SL = "SL License"
licenseName SMLNJ = "Standard ML of New Jersey License"
licenseName SMPPL = "Secure Messaging Protocol Public License"
licenseName SNIA = "SNIA Public License 1.1"
licenseName Snprintf = "snprintf License"
licenseName SoftSurfer = "softSurfer License"
licenseName Soundex = "Soundex License"
licenseName Spencer_86 = "Spencer License 86"
licenseName Spencer_94 = "Spencer License 94"
licenseName Spencer_99 = "Spencer License 99"
licenseName SPL_1_0 = "Sun Public License v1.0"
licenseName Ssh_keyscan = "ssh-keyscan License"
licenseName SSH_OpenSSH = "SSH OpenSSH license"
licenseName SSH_short = "SSH short notice"
licenseName SSLeay_standalone = "SSLeay License - standalone"
licenseName SSPL_1_0 = "Server Side Public License, v 1"
licenseName SugarCRM_1_1_3 = "SugarCRM Public License v1.1.3"
licenseName Sun_PPP = "Sun PPP License"
licenseName SunPro = "SunPro License"
licenseName SWL = "Scheme Widget Library (SWL) Software License Agreement"
licenseName Swrule = "swrule License"
licenseName Symlinks = "Symlinks License"
licenseName TAPR_OHL_1_0 = "TAPR Open Hardware License v1.0"
licenseName TCL = "TCL/TK License"
licenseName TCP_wrappers = "TCP Wrappers License"
licenseName TermReadKey = "TermReadKey License"
licenseName TGPPL_1_0 = "Transitive Grace Period Public Licence 1.0"
licenseName TMate = "TMate Open Source License"
licenseName TORQUE_1_1 = "TORQUE v2.5+ Software License v1.1"
licenseName TOSL = "Trusster Open Source License"
licenseName TPDL = "Time::ParseDate License"
licenseName TPL_1_0 = "THOR Public License 1.0"
licenseName TTWL = "Text-Tabs+Wrap License"
licenseName TTYP0 = "TTYP0 License"
licenseName TU_Berlin_1_0 = "Technische Universitaet Berlin License 1.0"
licenseName TU_Berlin_2_0 = "Technische Universitaet Berlin License 2.0"
licenseName UCAR = "UCAR License"
licenseName UCL_1_0 = "Upstream Compatibility License v1.0"
licenseName Ulem = "ulem License"
licenseName UMich_Merit = "Michigan/Merit Networks License"
licenseName Unicode_3_0 = "Unicode License v3"
licenseName Unicode_DFS_2015 = "Unicode License Agreement - Data Files and Software (2015)"
licenseName Unicode_DFS_2016 = "Unicode License Agreement - Data Files and Software (2016)"
licenseName Unicode_TOU = "Unicode Terms of Use"
licenseName UnixCrypt = "UnixCrypt License"
licenseName Unlicense = "The Unlicense"
licenseName UPL_1_0 = "Universal Permissive License v1.0"
licenseName URT_RLE = "Utah Raster Toolkit Run Length Encoded License"
licenseName Vim = "Vim License"
licenseName VOSTROM = "VOSTROM Public License for Open Source"
licenseName VSL_1_0 = "Vovida Software License v1.0"
licenseName W3C_19980720 = "W3C Software Notice and License (1998-07-20)"
licenseName W3C_20150513 = "W3C Software Notice and Document License (2015-05-13)"
licenseName W3C = "W3C Software Notice and License (2002-12-31)"
licenseName W3m = "w3m License"
licenseName Watcom_1_0 = "Sybase Open Watcom Public License 1.0"
licenseName Widget_Workshop = "Widget Workshop License"
licenseName Wsuipa = "Wsuipa License"
licenseName WTFPL = "Do What The F*ck You Want To Public License"
licenseName X11_distribute_modifications_variant = "X11 License Distribution Modification Variant"
licenseName X11 = "X11 License"
licenseName Xdebug_1_03 = "Xdebug License v 1.03"
licenseName Xerox = "Xerox License"
licenseName Xfig = "Xfig License"
licenseName XFree86_1_1 = "XFree86 License 1.1"
licenseName Xinetd = "xinetd License"
licenseName Xkeyboard_config_Zinoviev = "xkeyboard-config Zinoviev License"
licenseName Xlock = "xlock License"
licenseName Xnet = "X.Net License"
licenseName Xpp = "XPP License"
licenseName XSkat = "XSkat License"
licenseName YPL_1_0 = "Yahoo! Public License v1.0"
licenseName YPL_1_1 = "Yahoo! Public License v1.1"
licenseName Zed = "Zed License"
licenseName Zeeff = "Zeeff License"
licenseName Zend_2_0 = "Zend License v2.0"
licenseName Zimbra_1_3 = "Zimbra Public License v1.3"
licenseName Zimbra_1_4 = "Zimbra Public License v1.4"
licenseName Zlib_acknowledgement = "zlib/libpng License with Acknowledgement"
licenseName Zlib = "zlib License"
licenseName ZPL_1_1 = "Zope Public License 1.1"
licenseName ZPL_2_0 = "Zope Public License 2.0"
licenseName ZPL_2_1 = "Zope Public License 2.1"

-- | Whether the license is approved by Open Source Initiative (OSI).
--
-- See <https://opensource.org/licenses/alphabetical>.
licenseIsOsiApproved :: LicenseId -> Bool
licenseIsOsiApproved NullBSD = True
licenseIsOsiApproved AAL = True
licenseIsOsiApproved AFL_1_1 = True
licenseIsOsiApproved AFL_1_2 = True
licenseIsOsiApproved AFL_2_0 = True
licenseIsOsiApproved AFL_2_1 = True
licenseIsOsiApproved AFL_3_0 = True
licenseIsOsiApproved AGPL_3_0_only = True
licenseIsOsiApproved AGPL_3_0_or_later = True
licenseIsOsiApproved Apache_1_1 = True
licenseIsOsiApproved Apache_2_0 = True
licenseIsOsiApproved APL_1_0 = True
licenseIsOsiApproved APSL_1_0 = True
licenseIsOsiApproved APSL_1_1 = True
licenseIsOsiApproved APSL_1_2 = True
licenseIsOsiApproved APSL_2_0 = True
licenseIsOsiApproved Artistic_1_0_cl8 = True
licenseIsOsiApproved Artistic_1_0_Perl = True
licenseIsOsiApproved Artistic_1_0 = True
licenseIsOsiApproved Artistic_2_0 = True
licenseIsOsiApproved BlueOak_1_0_0 = True
licenseIsOsiApproved BSD_1_Clause = True
licenseIsOsiApproved BSD_2_Clause_Patent = True
licenseIsOsiApproved BSD_2_Clause = True
licenseIsOsiApproved BSD_3_Clause_LBNL = True
licenseIsOsiApproved BSD_3_Clause = True
licenseIsOsiApproved BSL_1_0 = True
licenseIsOsiApproved CAL_1_0_Combined_Work_Exception = True
licenseIsOsiApproved CAL_1_0 = True
licenseIsOsiApproved CATOSL_1_1 = True
licenseIsOsiApproved CDDL_1_0 = True
licenseIsOsiApproved CECILL_2_1 = True
licenseIsOsiApproved CERN_OHL_P_2_0 = True
licenseIsOsiApproved CERN_OHL_S_2_0 = True
licenseIsOsiApproved CERN_OHL_W_2_0 = True
licenseIsOsiApproved CNRI_Python = True
licenseIsOsiApproved CPAL_1_0 = True
licenseIsOsiApproved CPL_1_0 = True
licenseIsOsiApproved CUA_OPL_1_0 = True
licenseIsOsiApproved ECL_1_0 = True
licenseIsOsiApproved ECL_2_0 = True
licenseIsOsiApproved EFL_1_0 = True
licenseIsOsiApproved EFL_2_0 = True
licenseIsOsiApproved Entessa = True
licenseIsOsiApproved EPL_1_0 = True
licenseIsOsiApproved EPL_2_0 = True
licenseIsOsiApproved EUDatagrid = True
licenseIsOsiApproved EUPL_1_1 = True
licenseIsOsiApproved EUPL_1_2 = True
licenseIsOsiApproved Fair = True
licenseIsOsiApproved Frameworx_1_0 = True
licenseIsOsiApproved GPL_2_0_only = True
licenseIsOsiApproved GPL_2_0_or_later = True
licenseIsOsiApproved GPL_3_0_only = True
licenseIsOsiApproved GPL_3_0_or_later = True
licenseIsOsiApproved HPND = True
licenseIsOsiApproved ICU = True
licenseIsOsiApproved Intel = True
licenseIsOsiApproved IPA = True
licenseIsOsiApproved IPL_1_0 = True
licenseIsOsiApproved ISC = True
licenseIsOsiApproved Jam = True
licenseIsOsiApproved LGPL_2_0_only = True
licenseIsOsiApproved LGPL_2_0_or_later = True
licenseIsOsiApproved LGPL_2_1_only = True
licenseIsOsiApproved LGPL_2_1_or_later = True
licenseIsOsiApproved LGPL_3_0_only = True
licenseIsOsiApproved LGPL_3_0_or_later = True
licenseIsOsiApproved LiLiQ_P_1_1 = True
licenseIsOsiApproved LiLiQ_R_1_1 = True
licenseIsOsiApproved LiLiQ_Rplus_1_1 = True
licenseIsOsiApproved LPL_1_02 = True
licenseIsOsiApproved LPL_1_0 = True
licenseIsOsiApproved LPPL_1_3c = True
licenseIsOsiApproved MirOS = True
licenseIsOsiApproved MIT_0 = True
licenseIsOsiApproved MIT_Modern_Variant = True
licenseIsOsiApproved MIT = True
licenseIsOsiApproved Motosoto = True
licenseIsOsiApproved MPL_1_0 = True
licenseIsOsiApproved MPL_1_1 = True
licenseIsOsiApproved MPL_2_0_no_copyleft_exception = True
licenseIsOsiApproved MPL_2_0 = True
licenseIsOsiApproved MS_PL = True
licenseIsOsiApproved MS_RL = True
licenseIsOsiApproved MulanPSL_2_0 = True
licenseIsOsiApproved Multics = True
licenseIsOsiApproved NASA_1_3 = True
licenseIsOsiApproved Naumen = True
licenseIsOsiApproved NCSA = True
licenseIsOsiApproved NGPL = True
licenseIsOsiApproved Nokia = True
licenseIsOsiApproved NPOSL_3_0 = True
licenseIsOsiApproved NTP = True
licenseIsOsiApproved OCLC_2_0 = True
licenseIsOsiApproved OFL_1_1_no_RFN = True
licenseIsOsiApproved OFL_1_1_RFN = True
licenseIsOsiApproved OFL_1_1 = True
licenseIsOsiApproved OGTSL = True
licenseIsOsiApproved OLDAP_2_8 = True
licenseIsOsiApproved OLFL_1_3 = True
licenseIsOsiApproved OSET_PL_2_1 = True
licenseIsOsiApproved OSL_1_0 = True
licenseIsOsiApproved OSL_2_0 = True
licenseIsOsiApproved OSL_2_1 = True
licenseIsOsiApproved OSL_3_0 = True
licenseIsOsiApproved PHP_3_01 = True
licenseIsOsiApproved PHP_3_0 = True
licenseIsOsiApproved PostgreSQL = True
licenseIsOsiApproved Python_2_0 = True
licenseIsOsiApproved QPL_1_0 = True
licenseIsOsiApproved RPL_1_1 = True
licenseIsOsiApproved RPL_1_5 = True
licenseIsOsiApproved RPSL_1_0 = True
licenseIsOsiApproved RSCPL = True
licenseIsOsiApproved SimPL_2_0 = True
licenseIsOsiApproved SISSL = True
licenseIsOsiApproved Sleepycat = True
licenseIsOsiApproved SPL_1_0 = True
licenseIsOsiApproved UCL_1_0 = True
licenseIsOsiApproved Unicode_3_0 = True
licenseIsOsiApproved Unicode_DFS_2016 = True
licenseIsOsiApproved Unlicense = True
licenseIsOsiApproved UPL_1_0 = True
licenseIsOsiApproved VSL_1_0 = True
licenseIsOsiApproved W3C = True
licenseIsOsiApproved Watcom_1_0 = True
licenseIsOsiApproved Xnet = True
licenseIsOsiApproved Zlib = True
licenseIsOsiApproved ZPL_2_0 = True
licenseIsOsiApproved ZPL_2_1 = True
licenseIsOsiApproved _ = False

-- | Whether the license is considered libre by Free Software Foundation (FSF).
--
-- See <https://www.gnu.org/licenses/license-list.en.html>
--
-- @since 3.4.0.0
--
licenseIsFsfLibre :: LicenseId -> Bool
licenseIsFsfLibre AFL_1_1 = True
licenseIsFsfLibre AFL_1_2 = True
licenseIsFsfLibre AFL_2_0 = True
licenseIsFsfLibre AFL_2_1 = True
licenseIsFsfLibre AFL_3_0 = True
licenseIsFsfLibre AGPL_1_0 = True
licenseIsFsfLibre AGPL_3_0_only = True
licenseIsFsfLibre AGPL_3_0_or_later = True
licenseIsFsfLibre Apache_1_0 = True
licenseIsFsfLibre Apache_1_1 = True
licenseIsFsfLibre Apache_2_0 = True
licenseIsFsfLibre APSL_2_0 = True
licenseIsFsfLibre Artistic_2_0 = True
licenseIsFsfLibre BitTorrent_1_1 = True
licenseIsFsfLibre BSD_2_Clause_FreeBSD = True
licenseIsFsfLibre BSD_2_Clause = True
licenseIsFsfLibre BSD_3_Clause_Clear = True
licenseIsFsfLibre BSD_3_Clause = True
licenseIsFsfLibre BSD_4_Clause = True
licenseIsFsfLibre BSL_1_0 = True
licenseIsFsfLibre CC_BY_4_0 = True
licenseIsFsfLibre CC_BY_SA_4_0 = True
licenseIsFsfLibre CC0_1_0 = True
licenseIsFsfLibre CDDL_1_0 = True
licenseIsFsfLibre CECILL_2_0 = True
licenseIsFsfLibre CECILL_B = True
licenseIsFsfLibre CECILL_C = True
licenseIsFsfLibre ClArtistic = True
licenseIsFsfLibre Condor_1_1 = True
licenseIsFsfLibre CPAL_1_0 = True
licenseIsFsfLibre CPL_1_0 = True
licenseIsFsfLibre ECL_2_0 = True
licenseIsFsfLibre EFL_2_0 = True
licenseIsFsfLibre EPL_1_0 = True
licenseIsFsfLibre EPL_2_0 = True
licenseIsFsfLibre EUDatagrid = True
licenseIsFsfLibre EUPL_1_1 = True
licenseIsFsfLibre EUPL_1_2 = True
licenseIsFsfLibre FSFAP = True
licenseIsFsfLibre FTL = True
licenseIsFsfLibre GFDL_1_1_only = True
licenseIsFsfLibre GFDL_1_1_or_later = True
licenseIsFsfLibre GFDL_1_2_only = True
licenseIsFsfLibre GFDL_1_2_or_later = True
licenseIsFsfLibre GFDL_1_3_only = True
licenseIsFsfLibre GFDL_1_3_or_later = True
licenseIsFsfLibre Gnuplot = True
licenseIsFsfLibre GPL_2_0_only = True
licenseIsFsfLibre GPL_2_0_or_later = True
licenseIsFsfLibre GPL_3_0_only = True
licenseIsFsfLibre GPL_3_0_or_later = True
licenseIsFsfLibre HPND = True
licenseIsFsfLibre IJG = True
licenseIsFsfLibre IMatix = True
licenseIsFsfLibre Imlib2 = True
licenseIsFsfLibre Intel = True
licenseIsFsfLibre IPA = True
licenseIsFsfLibre IPL_1_0 = True
licenseIsFsfLibre ISC = True
licenseIsFsfLibre LGPL_2_1_only = True
licenseIsFsfLibre LGPL_2_1_or_later = True
licenseIsFsfLibre LGPL_3_0_only = True
licenseIsFsfLibre LGPL_3_0_or_later = True
licenseIsFsfLibre LPL_1_02 = True
licenseIsFsfLibre LPPL_1_2 = True
licenseIsFsfLibre LPPL_1_3a = True
licenseIsFsfLibre MIT = True
licenseIsFsfLibre MPL_1_1 = True
licenseIsFsfLibre MPL_2_0 = True
licenseIsFsfLibre MS_PL = True
licenseIsFsfLibre MS_RL = True
licenseIsFsfLibre NCSA = True
licenseIsFsfLibre Nokia = True
licenseIsFsfLibre NOSL = True
licenseIsFsfLibre NPL_1_0 = True
licenseIsFsfLibre NPL_1_1 = True
licenseIsFsfLibre ODbL_1_0 = True
licenseIsFsfLibre OFL_1_0 = True
licenseIsFsfLibre OFL_1_1 = True
licenseIsFsfLibre OLDAP_2_3 = True
licenseIsFsfLibre OLDAP_2_7 = True
licenseIsFsfLibre OpenSSL = True
licenseIsFsfLibre OSL_1_0 = True
licenseIsFsfLibre OSL_1_1 = True
licenseIsFsfLibre OSL_2_0 = True
licenseIsFsfLibre OSL_2_1 = True
licenseIsFsfLibre OSL_3_0 = True
licenseIsFsfLibre PHP_3_01 = True
licenseIsFsfLibre Python_2_0 = True
licenseIsFsfLibre QPL_1_0 = True
licenseIsFsfLibre RPSL_1_0 = True
licenseIsFsfLibre Ruby = True
licenseIsFsfLibre SGI_B_2_0 = True
licenseIsFsfLibre SISSL = True
licenseIsFsfLibre Sleepycat = True
licenseIsFsfLibre SMLNJ = True
licenseIsFsfLibre SPL_1_0 = True
licenseIsFsfLibre Unlicense = True
licenseIsFsfLibre UPL_1_0 = True
licenseIsFsfLibre Vim = True
licenseIsFsfLibre W3C = True
licenseIsFsfLibre WTFPL = True
licenseIsFsfLibre X11 = True
licenseIsFsfLibre XFree86_1_1 = True
licenseIsFsfLibre Xinetd = True
licenseIsFsfLibre YPL_1_1 = True
licenseIsFsfLibre Zend_2_0 = True
licenseIsFsfLibre Zimbra_1_3 = True
licenseIsFsfLibre Zlib = True
licenseIsFsfLibre ZPL_2_0 = True
licenseIsFsfLibre ZPL_2_1 = True
licenseIsFsfLibre _ = False

-------------------------------------------------------------------------------
-- Creation
-------------------------------------------------------------------------------

licenseIdList :: LicenseListVersion -> [LicenseId]
licenseIdList LicenseListVersion_3_0 =
    [ AGPL_1_0
    , BSD_2_Clause_FreeBSD
    , BSD_2_Clause_NetBSD
    , Bzip2_1_0_5
    ]
    ++ bulkOfLicenses
licenseIdList LicenseListVersion_3_2 =
    [ AGPL_1_0_only
    , AGPL_1_0_or_later
    , BSD_2_Clause_FreeBSD
    , BSD_2_Clause_NetBSD
    , Bzip2_1_0_5
    , Linux_OpenIB
    , MIT_0
    , ODC_By_1_0
    , TU_Berlin_1_0
    , TU_Berlin_2_0
    ]
    ++ bulkOfLicenses
licenseIdList LicenseListVersion_3_6 =
    [ AGPL_1_0_only
    , AGPL_1_0_or_later
    , Blessing
    , BlueOak_1_0_0
    , BSD_2_Clause_FreeBSD
    , BSD_2_Clause_NetBSD
    , BSD_3_Clause_Open_MPI
    , Bzip2_1_0_5
    , CC_PDDC
    , CERN_OHL_1_1
    , CERN_OHL_1_2
    , Copyleft_next_0_3_0
    , Copyleft_next_0_3_1
    , HPND_sell_variant
    , JPNIC
    , Libpng_2_0
    , Linux_OpenIB
    , MIT_0
    , ODC_By_1_0
    , OGL_UK_1_0
    , OGL_UK_2_0
    , OGL_UK_3_0
    , Parity_6_0_0
    , Sendmail_8_23
    , SHL_0_51
    , SHL_0_5
    , SSPL_1_0
    , TAPR_OHL_1_0
    , TU_Berlin_1_0
    , TU_Berlin_2_0
    ]
    ++ bulkOfLicenses
licenseIdList LicenseListVersion_3_9 =
    [ AGPL_1_0_only
    , AGPL_1_0_or_later
    , Blessing
    , BlueOak_1_0_0
    , BSD_2_Clause_FreeBSD
    , BSD_3_Clause_Open_MPI
    , Bzip2_1_0_5
    , CAL_1_0_Combined_Work_Exception
    , CAL_1_0
    , CC_PDDC
    , CERN_OHL_1_1
    , CERN_OHL_1_2
    , CERN_OHL_P_2_0
    , CERN_OHL_S_2_0
    , CERN_OHL_W_2_0
    , Copyleft_next_0_3_0
    , Copyleft_next_0_3_1
    , Etalab_2_0
    , Hippocratic_2_1
    , HPND_sell_variant
    , JPNIC
    , Libpng_2_0
    , Libselinux_1_0
    , Linux_OpenIB
    , MIT_0
    , MulanPSL_1_0
    , MulanPSL_2_0
    , NCGL_UK_2_0
    , NTP_0
    , O_UDA_1_0
    , ODC_By_1_0
    , OFL_1_0_no_RFN
    , OFL_1_0_RFN
    , OFL_1_1_no_RFN
    , OFL_1_1_RFN
    , OGC_1_0
    , OGL_Canada_2_0
    , OGL_UK_1_0
    , OGL_UK_2_0
    , OGL_UK_3_0
    , Parity_6_0_0
    , Parity_7_0_0
    , PolyForm_Noncommercial_1_0_0
    , PolyForm_Small_Business_1_0_0
    , PSF_2_0
    , Sendmail_8_23
    , SHL_0_51
    , SHL_0_5
    , SSH_OpenSSH
    , SSH_short
    , SSPL_1_0
    , TAPR_OHL_1_0
    , TU_Berlin_1_0
    , TU_Berlin_2_0
    , UCL_1_0
    ]
    ++ bulkOfLicenses
licenseIdList LicenseListVersion_3_10 =
    [ AGPL_1_0_only
    , AGPL_1_0_or_later
    , Blessing
    , BlueOak_1_0_0
    , BSD_2_Clause_Views
    , BSD_3_Clause_Open_MPI
    , Bzip2_1_0_5
    , CAL_1_0_Combined_Work_Exception
    , CAL_1_0
    , CC_BY_3_0_AT
    , CC_BY_NC_ND_3_0_IGO
    , CC_BY_SA_3_0_AT
    , CC_PDDC
    , CERN_OHL_1_1
    , CERN_OHL_1_2
    , CERN_OHL_P_2_0
    , CERN_OHL_S_2_0
    , CERN_OHL_W_2_0
    , Copyleft_next_0_3_0
    , Copyleft_next_0_3_1
    , EPICS
    , Etalab_2_0
    , GFDL_1_1_invariants_only
    , GFDL_1_1_invariants_or_later
    , GFDL_1_1_no_invariants_only
    , GFDL_1_1_no_invariants_or_later
    , GFDL_1_2_invariants_only
    , GFDL_1_2_invariants_or_later
    , GFDL_1_2_no_invariants_only
    , GFDL_1_2_no_invariants_or_later
    , GFDL_1_3_invariants_only
    , GFDL_1_3_invariants_or_later
    , GFDL_1_3_no_invariants_only
    , GFDL_1_3_no_invariants_or_later
    , GLWTPL
    , Hippocratic_2_1
    , HPND_sell_variant
    , JPNIC
    , Libpng_2_0
    , Libselinux_1_0
    , Linux_OpenIB
    , MIT_0
    , MulanPSL_1_0
    , MulanPSL_2_0
    , NCGL_UK_2_0
    , NIST_PD_fallback
    , NIST_PD
    , NTP_0
    , O_UDA_1_0
    , ODC_By_1_0
    , OFL_1_0_no_RFN
    , OFL_1_0_RFN
    , OFL_1_1_no_RFN
    , OFL_1_1_RFN
    , OGC_1_0
    , OGL_Canada_2_0
    , OGL_UK_1_0
    , OGL_UK_2_0
    , OGL_UK_3_0
    , Parity_6_0_0
    , Parity_7_0_0
    , PolyForm_Noncommercial_1_0_0
    , PolyForm_Small_Business_1_0_0
    , PSF_2_0
    , Sendmail_8_23
    , SHL_0_51
    , SHL_0_5
    , SSH_OpenSSH
    , SSH_short
    , SSPL_1_0
    , TAPR_OHL_1_0
    , TU_Berlin_1_0
    , TU_Berlin_2_0
    , UCL_1_0
    ]
    ++ bulkOfLicenses
licenseIdList LicenseListVersion_3_16 =
    [ AGPL_1_0_only
    , AGPL_1_0_or_later
    , ANTLR_PD_fallback
    , App_s2p
    , Blessing
    , BlueOak_1_0_0
    , BSD_2_Clause_Views
    , BSD_3_Clause_Modification
    , BSD_3_Clause_No_Military_License
    , BSD_3_Clause_Open_MPI
    , BSD_4_Clause_Shortened
    , BUSL_1_1
    , C_UDA_1_0
    , CAL_1_0_Combined_Work_Exception
    , CAL_1_0
    , CC_BY_2_5_AU
    , CC_BY_3_0_AT
    , CC_BY_3_0_DE
    , CC_BY_3_0_NL
    , CC_BY_3_0_US
    , CC_BY_NC_3_0_DE
    , CC_BY_NC_ND_3_0_DE
    , CC_BY_NC_ND_3_0_IGO
    , CC_BY_NC_SA_2_0_FR
    , CC_BY_NC_SA_2_0_UK
    , CC_BY_NC_SA_3_0_DE
    , CC_BY_NC_SA_3_0_IGO
    , CC_BY_ND_3_0_DE
    , CC_BY_SA_2_0_UK
    , CC_BY_SA_2_1_JP
    , CC_BY_SA_3_0_AT
    , CC_BY_SA_3_0_DE
    , CC_PDDC
    , CDL_1_0
    , CDLA_Permissive_2_0
    , CERN_OHL_1_1
    , CERN_OHL_1_2
    , CERN_OHL_P_2_0
    , CERN_OHL_S_2_0
    , CERN_OHL_W_2_0
    , COIL_1_0
    , Community_Spec_1_0
    , Copyleft_next_0_3_0
    , Copyleft_next_0_3_1
    , DL_DE_BY_2_0
    , DRL_1_0
    , Elastic_2_0
    , EPICS
    , Etalab_2_0
    , FDK_AAC
    , FreeBSD_DOC
    , GD
    , GFDL_1_1_invariants_only
    , GFDL_1_1_invariants_or_later
    , GFDL_1_1_no_invariants_only
    , GFDL_1_1_no_invariants_or_later
    , GFDL_1_2_invariants_only
    , GFDL_1_2_invariants_or_later
    , GFDL_1_2_no_invariants_only
    , GFDL_1_2_no_invariants_or_later
    , GFDL_1_3_invariants_only
    , GFDL_1_3_invariants_or_later
    , GFDL_1_3_no_invariants_only
    , GFDL_1_3_no_invariants_or_later
    , GLWTPL
    , Hippocratic_2_1
    , HPND_sell_variant
    , HTMLTIDY
    , Jam
    , JPNIC
    , Libpng_2_0
    , Libselinux_1_0
    , Linux_man_pages_copyleft
    , Linux_OpenIB
    , MIT_0
    , MIT_Modern_Variant
    , MIT_open_group
    , MulanPSL_1_0
    , MulanPSL_2_0
    , NAIST_2003
    , NCGL_UK_2_0
    , NIST_PD_fallback
    , NIST_PD
    , NLOD_2_0
    , NTP_0
    , O_UDA_1_0
    , ODC_By_1_0
    , OFL_1_0_no_RFN
    , OFL_1_0_RFN
    , OFL_1_1_no_RFN
    , OFL_1_1_RFN
    , OGC_1_0
    , OGDL_Taiwan_1_0
    , OGL_Canada_2_0
    , OGL_UK_1_0
    , OGL_UK_2_0
    , OGL_UK_3_0
    , OPUBL_1_0
    , Parity_6_0_0
    , Parity_7_0_0
    , PolyForm_Noncommercial_1_0_0
    , PolyForm_Small_Business_1_0_0
    , PSF_2_0
    , SchemeReport
    , Sendmail_8_23
    , SHL_0_51
    , SHL_0_5
    , SSH_OpenSSH
    , SSH_short
    , SSPL_1_0
    , TAPR_OHL_1_0
    , TU_Berlin_1_0
    , TU_Berlin_2_0
    , UCL_1_0
    , X11_distribute_modifications_variant
    ]
    ++ bulkOfLicenses
licenseIdList LicenseListVersion_3_23 =
    [ AdaCore_doc
    , Adobe_Display_PostScript
    , Adobe_Utopia
    , AGPL_1_0_only
    , AGPL_1_0_or_later
    , AML_glslang
    , ANTLR_PD_fallback
    , App_s2p
    , Arphic_1999
    , ASWF_Digital_Assets_1_0
    , ASWF_Digital_Assets_1_1
    , Baekmuk
    , Bcrypt_Solar_Designer
    , Bitstream_Charter
    , Bitstream_Vera
    , Blessing
    , BlueOak_1_0_0
    , Boehm_GC
    , Brian_Gladman_2_Clause
    , Brian_Gladman_3_Clause
    , BSD_2_Clause_Darwin
    , BSD_2_Clause_Views
    , BSD_3_Clause_acpica
    , BSD_3_Clause_flex
    , BSD_3_Clause_HP
    , BSD_3_Clause_Modification
    , BSD_3_Clause_No_Military_License
    , BSD_3_Clause_Open_MPI
    , BSD_3_Clause_Sun
    , BSD_4_Clause_Shortened
    , BSD_4_3RENO
    , BSD_4_3TAHOE
    , BSD_Advertising_Acknowledgement
    , BSD_Attribution_HPND_disclaimer
    , BSD_Inferno_Nettverk
    , BSD_Source_beginning_file
    , BSD_Systemics_W3Works
    , BSD_Systemics
    , BUSL_1_1
    , C_UDA_1_0
    , CAL_1_0_Combined_Work_Exception
    , CAL_1_0
    , Caldera_no_preamble
    , CC_BY_2_5_AU
    , CC_BY_3_0_AT
    , CC_BY_3_0_AU
    , CC_BY_3_0_DE
    , CC_BY_3_0_IGO
    , CC_BY_3_0_NL
    , CC_BY_3_0_US
    , CC_BY_NC_3_0_DE
    , CC_BY_NC_ND_3_0_DE
    , CC_BY_NC_ND_3_0_IGO
    , CC_BY_NC_SA_2_0_DE
    , CC_BY_NC_SA_2_0_FR
    , CC_BY_NC_SA_2_0_UK
    , CC_BY_NC_SA_3_0_DE
    , CC_BY_NC_SA_3_0_IGO
    , CC_BY_ND_3_0_DE
    , CC_BY_SA_2_0_UK
    , CC_BY_SA_2_1_JP
    , CC_BY_SA_3_0_AT
    , CC_BY_SA_3_0_DE
    , CC_BY_SA_3_0_IGO
    , CC_PDDC
    , CDL_1_0
    , CDLA_Permissive_2_0
    , CERN_OHL_1_1
    , CERN_OHL_1_2
    , CERN_OHL_P_2_0
    , CERN_OHL_S_2_0
    , CERN_OHL_W_2_0
    , CFITSIO
    , Check_cvs
    , Checkmk
    , Clips
    , CMU_Mach_nodoc
    , CMU_Mach
    , COIL_1_0
    , Community_Spec_1_0
    , Copyleft_next_0_3_0
    , Copyleft_next_0_3_1
    , Cornell_Lossless_JPEG
    , Cronyx
    , DEC_3_Clause
    , DL_DE_BY_2_0
    , DL_DE_ZERO_2_0
    , DRL_1_0
    , DRL_1_1
    , Dtoa
    , Elastic_2_0
    , EPICS
    , Etalab_2_0
    , FBM
    , FDK_AAC
    , Ferguson_Twofish
    , FreeBSD_DOC
    , FSFAP_no_warranty_disclaimer
    , FSFULLRWD
    , Furuseth
    , Fwlw
    , GCR_docs
    , GD
    , GFDL_1_1_invariants_only
    , GFDL_1_1_invariants_or_later
    , GFDL_1_1_no_invariants_only
    , GFDL_1_1_no_invariants_or_later
    , GFDL_1_2_invariants_only
    , GFDL_1_2_invariants_or_later
    , GFDL_1_2_no_invariants_only
    , GFDL_1_2_no_invariants_or_later
    , GFDL_1_3_invariants_only
    , GFDL_1_3_invariants_or_later
    , GFDL_1_3_no_invariants_only
    , GFDL_1_3_no_invariants_or_later
    , GLWTPL
    , Graphics_Gems
    , Gtkbook
    , Hdparm
    , Hippocratic_2_1
    , HP_1986
    , HP_1989
    , HPND_DEC
    , HPND_doc_sell
    , HPND_doc
    , HPND_export_US_modify
    , HPND_export_US
    , HPND_Fenneberg_Livingston
    , HPND_INRIA_IMAG
    , HPND_Kevlin_Henney
    , HPND_Markus_Kuhn
    , HPND_MIT_disclaimer
    , HPND_Pbmplus
    , HPND_sell_MIT_disclaimer_xserver
    , HPND_sell_regexpr
    , HPND_sell_variant_MIT_disclaimer
    , HPND_sell_variant
    , HPND_UC
    , HTMLTIDY
    , IEC_Code_Components_EULA
    , IJG_short
    , Inner_Net_2_0
    , ISC_Veillard
    , Jam
    , JPL_image
    , JPNIC
    , Kastrup
    , Kazlib
    , Knuth_CTAN
    , Latex2e_translated_notice
    , Libpng_2_0
    , Libselinux_1_0
    , Libutil_David_Nugent
    , Linux_man_pages_1_para
    , Linux_man_pages_copyleft_2_para
    , Linux_man_pages_copyleft_var
    , Linux_man_pages_copyleft
    , Linux_OpenIB
    , LOOP
    , LPD_document
    , Lsof
    , Lucida_Bitmap_Fonts
    , LZMA_SDK_9_11_to_9_20
    , LZMA_SDK_9_22
    , Mackerras_3_Clause_acknowledgment
    , Mackerras_3_Clause
    , Magaz
    , Mailprio
    , Martin_Birgmeier
    , McPhee_slideshow
    , Metamail
    , Minpack
    , MIT_0
    , MIT_Festival
    , MIT_Modern_Variant
    , MIT_open_group
    , MIT_testregex
    , MIT_Wu
    , MMIXware
    , MPEG_SSG
    , Mpi_permissive
    , Mplus
    , MS_LPL
    , MulanPSL_1_0
    , MulanPSL_2_0
    , NAIST_2003
    , NCGL_UK_2_0
    , NICTA_1_0
    , NIST_PD_fallback
    , NIST_PD
    , NIST_Software
    , NLOD_2_0
    , NTP_0
    , O_UDA_1_0
    , ODC_By_1_0
    , OFFIS
    , OFL_1_0_no_RFN
    , OFL_1_0_RFN
    , OFL_1_1_no_RFN
    , OFL_1_1_RFN
    , OGC_1_0
    , OGDL_Taiwan_1_0
    , OGL_Canada_2_0
    , OGL_UK_1_0
    , OGL_UK_2_0
    , OGL_UK_3_0
    , OLFL_1_3
    , OpenPBS_2_3
    , OpenSSL_standalone
    , OpenVision
    , OPL_UK_3_0
    , OPUBL_1_0
    , PADL
    , Parity_6_0_0
    , Parity_7_0_0
    , Pixar
    , Pnmstitch
    , PolyForm_Noncommercial_1_0_0
    , PolyForm_Small_Business_1_0_0
    , PSF_2_0
    , Python_2_0_1
    , Python_ldap
    , QPL_1_0_INRIA_2004
    , Radvd
    , SAX_PD_2_0
    , SchemeReport
    , Sendmail_8_23
    , SGI_OpenGL
    , SGP4
    , SHL_0_51
    , SHL_0_5
    , SL
    , Snprintf
    , SoftSurfer
    , Soundex
    , Ssh_keyscan
    , SSH_OpenSSH
    , SSH_short
    , SSLeay_standalone
    , SSPL_1_0
    , Sun_PPP
    , SunPro
    , Swrule
    , Symlinks
    , TAPR_OHL_1_0
    , TermReadKey
    , TGPPL_1_0
    , TPDL
    , TPL_1_0
    , TTWL
    , TTYP0
    , TU_Berlin_1_0
    , TU_Berlin_2_0
    , UCAR
    , UCL_1_0
    , Ulem
    , UMich_Merit
    , Unicode_3_0
    , UnixCrypt
    , URT_RLE
    , W3m
    , Widget_Workshop
    , X11_distribute_modifications_variant
    , Xdebug_1_03
    , Xfig
    , Xkeyboard_config_Zinoviev
    , Xlock
    , Zeeff
    ]
    ++ bulkOfLicenses

-- | Create a 'LicenseId' from a 'String'.
mkLicenseId :: LicenseListVersion -> String -> Maybe LicenseId
mkLicenseId LicenseListVersion_3_0  s = Map.lookup s stringLookup_3_0
mkLicenseId LicenseListVersion_3_2  s = Map.lookup s stringLookup_3_2
mkLicenseId LicenseListVersion_3_6  s = Map.lookup s stringLookup_3_6
mkLicenseId LicenseListVersion_3_9  s = Map.lookup s stringLookup_3_9
mkLicenseId LicenseListVersion_3_10 s = Map.lookup s stringLookup_3_10
mkLicenseId LicenseListVersion_3_16 s = Map.lookup s stringLookup_3_16
mkLicenseId LicenseListVersion_3_23 s = Map.lookup s stringLookup_3_23

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

stringLookup_3_23 :: Map String LicenseId
stringLookup_3_23 = Map.fromList $ map (\i -> (licenseId i, i)) $
    licenseIdList LicenseListVersion_3_23

--  | Licenses in all SPDX License lists
bulkOfLicenses :: [LicenseId]
bulkOfLicenses =
    [ NullBSD
    , AAL
    , Abstyles
    , Adobe_2006
    , Adobe_Glyph
    , ADSL
    , AFL_1_1
    , AFL_1_2
    , AFL_2_0
    , AFL_2_1
    , AFL_3_0
    , Afmparse
    , AGPL_3_0_only
    , AGPL_3_0_or_later
    , Aladdin
    , AMDPLPA
    , AML
    , AMPAS
    , ANTLR_PD
    , Apache_1_0
    , Apache_1_1
    , Apache_2_0
    , APAFML
    , APL_1_0
    , APSL_1_0
    , APSL_1_1
    , APSL_1_2
    , APSL_2_0
    , Artistic_1_0_cl8
    , Artistic_1_0_Perl
    , Artistic_1_0
    , Artistic_2_0
    , Bahyph
    , Barr
    , Beerware
    , BitTorrent_1_0
    , BitTorrent_1_1
    , Borceux
    , BSD_1_Clause
    , BSD_2_Clause_Patent
    , BSD_2_Clause
    , BSD_3_Clause_Attribution
    , BSD_3_Clause_Clear
    , BSD_3_Clause_LBNL
    , BSD_3_Clause_No_Nuclear_License_2014
    , BSD_3_Clause_No_Nuclear_License
    , BSD_3_Clause_No_Nuclear_Warranty
    , BSD_3_Clause
    , BSD_4_Clause_UC
    , BSD_4_Clause
    , BSD_Protection
    , BSD_Source_Code
    , BSL_1_0
    , Bzip2_1_0_6
    , Caldera
    , CATOSL_1_1
    , CC_BY_1_0
    , CC_BY_2_0
    , CC_BY_2_5
    , CC_BY_3_0
    , CC_BY_4_0
    , CC_BY_NC_1_0
    , CC_BY_NC_2_0
    , CC_BY_NC_2_5
    , CC_BY_NC_3_0
    , CC_BY_NC_4_0
    , CC_BY_NC_ND_1_0
    , CC_BY_NC_ND_2_0
    , CC_BY_NC_ND_2_5
    , CC_BY_NC_ND_3_0
    , CC_BY_NC_ND_4_0
    , CC_BY_NC_SA_1_0
    , CC_BY_NC_SA_2_0
    , CC_BY_NC_SA_2_5
    , CC_BY_NC_SA_3_0
    , CC_BY_NC_SA_4_0
    , CC_BY_ND_1_0
    , CC_BY_ND_2_0
    , CC_BY_ND_2_5
    , CC_BY_ND_3_0
    , CC_BY_ND_4_0
    , CC_BY_SA_1_0
    , CC_BY_SA_2_0
    , CC_BY_SA_2_5
    , CC_BY_SA_3_0
    , CC_BY_SA_4_0
    , CC0_1_0
    , CDDL_1_0
    , CDDL_1_1
    , CDLA_Permissive_1_0
    , CDLA_Sharing_1_0
    , CECILL_1_0
    , CECILL_1_1
    , CECILL_2_0
    , CECILL_2_1
    , CECILL_B
    , CECILL_C
    , ClArtistic
    , CNRI_Jython
    , CNRI_Python_GPL_Compatible
    , CNRI_Python
    , Condor_1_1
    , CPAL_1_0
    , CPL_1_0
    , CPOL_1_02
    , Crossword
    , CrystalStacker
    , CUA_OPL_1_0
    , Cube
    , Curl
    , D_FSL_1_0
    , Diffmark
    , DOC
    , Dotseqn
    , DSDP
    , Dvipdfm
    , ECL_1_0
    , ECL_2_0
    , EFL_1_0
    , EFL_2_0
    , EGenix
    , Entessa
    , EPL_1_0
    , EPL_2_0
    , ErlPL_1_1
    , EUDatagrid
    , EUPL_1_0
    , EUPL_1_1
    , EUPL_1_2
    , Eurosym
    , Fair
    , Frameworx_1_0
    , FreeImage
    , FSFAP
    , FSFULLR
    , FSFUL
    , FTL
    , GFDL_1_1_only
    , GFDL_1_1_or_later
    , GFDL_1_2_only
    , GFDL_1_2_or_later
    , GFDL_1_3_only
    , GFDL_1_3_or_later
    , Giftware
    , GL2PS
    , Glide
    , Glulxe
    , Gnuplot
    , GPL_1_0_only
    , GPL_1_0_or_later
    , GPL_2_0_only
    , GPL_2_0_or_later
    , GPL_3_0_only
    , GPL_3_0_or_later
    , GSOAP_1_3b
    , HaskellReport
    , HPND
    , IBM_pibs
    , ICU
    , IJG
    , ImageMagick
    , IMatix
    , Imlib2
    , Info_ZIP
    , Intel_ACPI
    , Intel
    , Interbase_1_0
    , IPA
    , IPL_1_0
    , ISC
    , JasPer_2_0
    , JSON
    , LAL_1_2
    , LAL_1_3
    , Latex2e
    , Leptonica
    , LGPL_2_0_only
    , LGPL_2_0_or_later
    , LGPL_2_1_only
    , LGPL_2_1_or_later
    , LGPL_3_0_only
    , LGPL_3_0_or_later
    , LGPLLR
    , Libpng
    , Libtiff
    , LiLiQ_P_1_1
    , LiLiQ_R_1_1
    , LiLiQ_Rplus_1_1
    , LPL_1_02
    , LPL_1_0
    , LPPL_1_0
    , LPPL_1_1
    , LPPL_1_2
    , LPPL_1_3a
    , LPPL_1_3c
    , MakeIndex
    , MirOS
    , MIT_advertising
    , MIT_CMU
    , MIT_enna
    , MIT_feh
    , MITNFA
    , MIT
    , Motosoto
    , Mpich2
    , MPL_1_0
    , MPL_1_1
    , MPL_2_0_no_copyleft_exception
    , MPL_2_0
    , MS_PL
    , MS_RL
    , MTLL
    , Multics
    , Mup
    , NASA_1_3
    , Naumen
    , NBPL_1_0
    , NCSA
    , Net_SNMP
    , NetCDF
    , Newsletr
    , NGPL
    , NLOD_1_0
    , NLPL
    , Nokia
    , NOSL
    , Noweb
    , NPL_1_0
    , NPL_1_1
    , NPOSL_3_0
    , NRL
    , NTP
    , OCCT_PL
    , OCLC_2_0
    , ODbL_1_0
    , OFL_1_0
    , OFL_1_1
    , OGTSL
    , OLDAP_1_1
    , OLDAP_1_2
    , OLDAP_1_3
    , OLDAP_1_4
    , OLDAP_2_0_1
    , OLDAP_2_0
    , OLDAP_2_1
    , OLDAP_2_2_1
    , OLDAP_2_2_2
    , OLDAP_2_2
    , OLDAP_2_3
    , OLDAP_2_4
    , OLDAP_2_5
    , OLDAP_2_6
    , OLDAP_2_7
    , OLDAP_2_8
    , OML
    , OpenSSL
    , OPL_1_0
    , OSET_PL_2_1
    , OSL_1_0
    , OSL_1_1
    , OSL_2_0
    , OSL_2_1
    , OSL_3_0
    , PDDL_1_0
    , PHP_3_01
    , PHP_3_0
    , Plexus
    , PostgreSQL
    , Psfrag
    , Psutils
    , Python_2_0
    , Qhull
    , QPL_1_0
    , Rdisc
    , RHeCos_1_1
    , RPL_1_1
    , RPL_1_5
    , RPSL_1_0
    , RSA_MD
    , RSCPL
    , Ruby
    , SAX_PD
    , Saxpath
    , SCEA
    , Sendmail
    , SGI_B_1_0
    , SGI_B_1_1
    , SGI_B_2_0
    , SimPL_2_0
    , SISSL_1_2
    , SISSL
    , Sleepycat
    , SMLNJ
    , SMPPL
    , SNIA
    , Spencer_86
    , Spencer_94
    , Spencer_99
    , SPL_1_0
    , SugarCRM_1_1_3
    , SWL
    , TCL
    , TCP_wrappers
    , TMate
    , TORQUE_1_1
    , TOSL
    , Unicode_DFS_2015
    , Unicode_DFS_2016
    , Unicode_TOU
    , Unlicense
    , UPL_1_0
    , Vim
    , VOSTROM
    , VSL_1_0
    , W3C_19980720
    , W3C_20150513
    , W3C
    , Watcom_1_0
    , Wsuipa
    , WTFPL
    , X11
    , Xerox
    , XFree86_1_1
    , Xinetd
    , Xnet
    , Xpp
    , XSkat
    , YPL_1_0
    , YPL_1_1
    , Zed
    , Zend_2_0
    , Zimbra_1_3
    , Zimbra_1_4
    , Zlib_acknowledgement
    , Zlib
    , ZPL_1_1
    , ZPL_2_0
    , ZPL_2_1
    ]
