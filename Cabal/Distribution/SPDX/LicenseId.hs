-- This file is generated. See Makefile's spdx rule
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
module Distribution.SPDX.LicenseId (
    LicenseId (..),
    licenseId,
    licenseName,
    licenseIsOsiApproved,
    mkLicenseId,
    licenseIdList,
    -- * Helpers
    licenseIdMigrationMessage,
    ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Pretty
import Distribution.Parsec
import Distribution.Utils.Generic (isAsciiAlphaNum)
import Distribution.SPDX.LicenseListVersion

import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.Map.Strict as Map
import qualified Distribution.Compat.CharParsing as P
import qualified Text.PrettyPrint as Disp

-------------------------------------------------------------------------------
-- LicenseId
-------------------------------------------------------------------------------

-- | SPDX License identifier
data LicenseId
    = NullBSD -- ^ @0BSD@, BSD Zero Clause License
    | AAL -- ^ @AAL@, Attribution Assurance License
    | Abstyles -- ^ @Abstyles@, Abstyles License
    | Adobe_2006 -- ^ @Adobe-2006@, Adobe Systems Incorporated Source Code License Agreement
    | Adobe_Glyph -- ^ @Adobe-Glyph@, Adobe Glyph List License
    | ADSL -- ^ @ADSL@, Amazon Digital Services License
    | AFL_1_1 -- ^ @AFL-1.1@, Academic Free License v1.1
    | AFL_1_2 -- ^ @AFL-1.2@, Academic Free License v1.2
    | AFL_2_0 -- ^ @AFL-2.0@, Academic Free License v2.0
    | AFL_2_1 -- ^ @AFL-2.1@, Academic Free License v2.1
    | AFL_3_0 -- ^ @AFL-3.0@, Academic Free License v3.0
    | Afmparse -- ^ @Afmparse@, Afmparse License
    | AGPL_1_0 -- ^ @AGPL-1.0@, Affero General Public License v1.0, SPDX License List 3.0
    | AGPL_1_0_only -- ^ @AGPL-1.0-only@, Affero General Public License v1.0 only, SPDX License List 3.2, SPDX License List 3.6
    | AGPL_1_0_or_later -- ^ @AGPL-1.0-or-later@, Affero General Public License v1.0 or later, SPDX License List 3.2, SPDX License List 3.6
    | AGPL_3_0_only -- ^ @AGPL-3.0-only@, GNU Affero General Public License v3.0 only
    | AGPL_3_0_or_later -- ^ @AGPL-3.0-or-later@, GNU Affero General Public License v3.0 or later
    | Aladdin -- ^ @Aladdin@, Aladdin Free Public License
    | AMDPLPA -- ^ @AMDPLPA@, AMD's plpa_map.c License
    | AML -- ^ @AML@, Apple MIT License
    | AMPAS -- ^ @AMPAS@, Academy of Motion Picture Arts and Sciences BSD
    | ANTLR_PD -- ^ @ANTLR-PD@, ANTLR Software Rights Notice
    | Apache_1_0 -- ^ @Apache-1.0@, Apache License 1.0
    | Apache_1_1 -- ^ @Apache-1.1@, Apache License 1.1
    | Apache_2_0 -- ^ @Apache-2.0@, Apache License 2.0
    | APAFML -- ^ @APAFML@, Adobe Postscript AFM License
    | APL_1_0 -- ^ @APL-1.0@, Adaptive Public License 1.0
    | APSL_1_0 -- ^ @APSL-1.0@, Apple Public Source License 1.0
    | APSL_1_1 -- ^ @APSL-1.1@, Apple Public Source License 1.1
    | APSL_1_2 -- ^ @APSL-1.2@, Apple Public Source License 1.2
    | APSL_2_0 -- ^ @APSL-2.0@, Apple Public Source License 2.0
    | Artistic_1_0_cl8 -- ^ @Artistic-1.0-cl8@, Artistic License 1.0 w/clause 8
    | Artistic_1_0_Perl -- ^ @Artistic-1.0-Perl@, Artistic License 1.0 (Perl)
    | Artistic_1_0 -- ^ @Artistic-1.0@, Artistic License 1.0
    | Artistic_2_0 -- ^ @Artistic-2.0@, Artistic License 2.0
    | Bahyph -- ^ @Bahyph@, Bahyph License
    | Barr -- ^ @Barr@, Barr License
    | Beerware -- ^ @Beerware@, Beerware License
    | BitTorrent_1_0 -- ^ @BitTorrent-1.0@, BitTorrent Open Source License v1.0
    | BitTorrent_1_1 -- ^ @BitTorrent-1.1@, BitTorrent Open Source License v1.1
    | Blessing -- ^ @blessing@, SQLite Blessing, SPDX License List 3.6
    | BlueOak_1_0_0 -- ^ @BlueOak-1.0.0@, Blue Oak Model License 1.0.0, SPDX License List 3.6
    | Borceux -- ^ @Borceux@, Borceux license
    | BSD_1_Clause -- ^ @BSD-1-Clause@, BSD 1-Clause License
    | BSD_2_Clause_FreeBSD -- ^ @BSD-2-Clause-FreeBSD@, BSD 2-Clause FreeBSD License
    | BSD_2_Clause_NetBSD -- ^ @BSD-2-Clause-NetBSD@, BSD 2-Clause NetBSD License
    | BSD_2_Clause_Patent -- ^ @BSD-2-Clause-Patent@, BSD-2-Clause Plus Patent License
    | BSD_2_Clause -- ^ @BSD-2-Clause@, BSD 2-Clause "Simplified" License
    | BSD_3_Clause_Attribution -- ^ @BSD-3-Clause-Attribution@, BSD with attribution
    | BSD_3_Clause_Clear -- ^ @BSD-3-Clause-Clear@, BSD 3-Clause Clear License
    | BSD_3_Clause_LBNL -- ^ @BSD-3-Clause-LBNL@, Lawrence Berkeley National Labs BSD variant license
    | BSD_3_Clause_No_Nuclear_License_2014 -- ^ @BSD-3-Clause-No-Nuclear-License-2014@, BSD 3-Clause No Nuclear License 2014
    | BSD_3_Clause_No_Nuclear_License -- ^ @BSD-3-Clause-No-Nuclear-License@, BSD 3-Clause No Nuclear License
    | BSD_3_Clause_No_Nuclear_Warranty -- ^ @BSD-3-Clause-No-Nuclear-Warranty@, BSD 3-Clause No Nuclear Warranty
    | BSD_3_Clause_Open_MPI -- ^ @BSD-3-Clause-Open-MPI@, BSD 3-Clause Open MPI variant, SPDX License List 3.6
    | BSD_3_Clause -- ^ @BSD-3-Clause@, BSD 3-Clause "New" or "Revised" License
    | BSD_4_Clause_UC -- ^ @BSD-4-Clause-UC@, BSD-4-Clause (University of California-Specific)
    | BSD_4_Clause -- ^ @BSD-4-Clause@, BSD 4-Clause "Original" or "Old" License
    | BSD_Protection -- ^ @BSD-Protection@, BSD Protection License
    | BSD_Source_Code -- ^ @BSD-Source-Code@, BSD Source Code Attribution
    | BSL_1_0 -- ^ @BSL-1.0@, Boost Software License 1.0
    | Bzip2_1_0_5 -- ^ @bzip2-1.0.5@, bzip2 and libbzip2 License v1.0.5
    | Bzip2_1_0_6 -- ^ @bzip2-1.0.6@, bzip2 and libbzip2 License v1.0.6
    | Caldera -- ^ @Caldera@, Caldera License
    | CATOSL_1_1 -- ^ @CATOSL-1.1@, Computer Associates Trusted Open Source License 1.1
    | CC_BY_1_0 -- ^ @CC-BY-1.0@, Creative Commons Attribution 1.0 Generic
    | CC_BY_2_0 -- ^ @CC-BY-2.0@, Creative Commons Attribution 2.0 Generic
    | CC_BY_2_5 -- ^ @CC-BY-2.5@, Creative Commons Attribution 2.5 Generic
    | CC_BY_3_0 -- ^ @CC-BY-3.0@, Creative Commons Attribution 3.0 Unported
    | CC_BY_4_0 -- ^ @CC-BY-4.0@, Creative Commons Attribution 4.0 International
    | CC_BY_NC_1_0 -- ^ @CC-BY-NC-1.0@, Creative Commons Attribution Non Commercial 1.0 Generic
    | CC_BY_NC_2_0 -- ^ @CC-BY-NC-2.0@, Creative Commons Attribution Non Commercial 2.0 Generic
    | CC_BY_NC_2_5 -- ^ @CC-BY-NC-2.5@, Creative Commons Attribution Non Commercial 2.5 Generic
    | CC_BY_NC_3_0 -- ^ @CC-BY-NC-3.0@, Creative Commons Attribution Non Commercial 3.0 Unported
    | CC_BY_NC_4_0 -- ^ @CC-BY-NC-4.0@, Creative Commons Attribution Non Commercial 4.0 International
    | CC_BY_NC_ND_1_0 -- ^ @CC-BY-NC-ND-1.0@, Creative Commons Attribution Non Commercial No Derivatives 1.0 Generic
    | CC_BY_NC_ND_2_0 -- ^ @CC-BY-NC-ND-2.0@, Creative Commons Attribution Non Commercial No Derivatives 2.0 Generic
    | CC_BY_NC_ND_2_5 -- ^ @CC-BY-NC-ND-2.5@, Creative Commons Attribution Non Commercial No Derivatives 2.5 Generic
    | CC_BY_NC_ND_3_0 -- ^ @CC-BY-NC-ND-3.0@, Creative Commons Attribution Non Commercial No Derivatives 3.0 Unported
    | CC_BY_NC_ND_4_0 -- ^ @CC-BY-NC-ND-4.0@, Creative Commons Attribution Non Commercial No Derivatives 4.0 International
    | CC_BY_NC_SA_1_0 -- ^ @CC-BY-NC-SA-1.0@, Creative Commons Attribution Non Commercial Share Alike 1.0 Generic
    | CC_BY_NC_SA_2_0 -- ^ @CC-BY-NC-SA-2.0@, Creative Commons Attribution Non Commercial Share Alike 2.0 Generic
    | CC_BY_NC_SA_2_5 -- ^ @CC-BY-NC-SA-2.5@, Creative Commons Attribution Non Commercial Share Alike 2.5 Generic
    | CC_BY_NC_SA_3_0 -- ^ @CC-BY-NC-SA-3.0@, Creative Commons Attribution Non Commercial Share Alike 3.0 Unported
    | CC_BY_NC_SA_4_0 -- ^ @CC-BY-NC-SA-4.0@, Creative Commons Attribution Non Commercial Share Alike 4.0 International
    | CC_BY_ND_1_0 -- ^ @CC-BY-ND-1.0@, Creative Commons Attribution No Derivatives 1.0 Generic
    | CC_BY_ND_2_0 -- ^ @CC-BY-ND-2.0@, Creative Commons Attribution No Derivatives 2.0 Generic
    | CC_BY_ND_2_5 -- ^ @CC-BY-ND-2.5@, Creative Commons Attribution No Derivatives 2.5 Generic
    | CC_BY_ND_3_0 -- ^ @CC-BY-ND-3.0@, Creative Commons Attribution No Derivatives 3.0 Unported
    | CC_BY_ND_4_0 -- ^ @CC-BY-ND-4.0@, Creative Commons Attribution No Derivatives 4.0 International
    | CC_BY_SA_1_0 -- ^ @CC-BY-SA-1.0@, Creative Commons Attribution Share Alike 1.0 Generic
    | CC_BY_SA_2_0 -- ^ @CC-BY-SA-2.0@, Creative Commons Attribution Share Alike 2.0 Generic
    | CC_BY_SA_2_5 -- ^ @CC-BY-SA-2.5@, Creative Commons Attribution Share Alike 2.5 Generic
    | CC_BY_SA_3_0 -- ^ @CC-BY-SA-3.0@, Creative Commons Attribution Share Alike 3.0 Unported
    | CC_BY_SA_4_0 -- ^ @CC-BY-SA-4.0@, Creative Commons Attribution Share Alike 4.0 International
    | CC_PDDC -- ^ @CC-PDDC@, Creative Commons Public Domain Dedication and Certification, SPDX License List 3.6
    | CC0_1_0 -- ^ @CC0-1.0@, Creative Commons Zero v1.0 Universal
    | CDDL_1_0 -- ^ @CDDL-1.0@, Common Development and Distribution License 1.0
    | CDDL_1_1 -- ^ @CDDL-1.1@, Common Development and Distribution License 1.1
    | CDLA_Permissive_1_0 -- ^ @CDLA-Permissive-1.0@, Community Data License Agreement Permissive 1.0
    | CDLA_Sharing_1_0 -- ^ @CDLA-Sharing-1.0@, Community Data License Agreement Sharing 1.0
    | CECILL_1_0 -- ^ @CECILL-1.0@, CeCILL Free Software License Agreement v1.0
    | CECILL_1_1 -- ^ @CECILL-1.1@, CeCILL Free Software License Agreement v1.1
    | CECILL_2_0 -- ^ @CECILL-2.0@, CeCILL Free Software License Agreement v2.0
    | CECILL_2_1 -- ^ @CECILL-2.1@, CeCILL Free Software License Agreement v2.1
    | CECILL_B -- ^ @CECILL-B@, CeCILL-B Free Software License Agreement
    | CECILL_C -- ^ @CECILL-C@, CeCILL-C Free Software License Agreement
    | CERN_OHL_1_1 -- ^ @CERN-OHL-1.1@, CERN Open Hardware License v1.1, SPDX License List 3.6
    | CERN_OHL_1_2 -- ^ @CERN-OHL-1.2@, CERN Open Hardware Licence v1.2, SPDX License List 3.6
    | ClArtistic -- ^ @ClArtistic@, Clarified Artistic License
    | CNRI_Jython -- ^ @CNRI-Jython@, CNRI Jython License
    | CNRI_Python_GPL_Compatible -- ^ @CNRI-Python-GPL-Compatible@, CNRI Python Open Source GPL Compatible License Agreement
    | CNRI_Python -- ^ @CNRI-Python@, CNRI Python License
    | Condor_1_1 -- ^ @Condor-1.1@, Condor Public License v1.1
    | Copyleft_next_0_3_0 -- ^ @copyleft-next-0.3.0@, copyleft-next 0.3.0, SPDX License List 3.6
    | Copyleft_next_0_3_1 -- ^ @copyleft-next-0.3.1@, copyleft-next 0.3.1, SPDX License List 3.6
    | CPAL_1_0 -- ^ @CPAL-1.0@, Common Public Attribution License 1.0
    | CPL_1_0 -- ^ @CPL-1.0@, Common Public License 1.0
    | CPOL_1_02 -- ^ @CPOL-1.02@, Code Project Open License 1.02
    | Crossword -- ^ @Crossword@, Crossword License
    | CrystalStacker -- ^ @CrystalStacker@, CrystalStacker License
    | CUA_OPL_1_0 -- ^ @CUA-OPL-1.0@, CUA Office Public License v1.0
    | Cube -- ^ @Cube@, Cube License
    | Curl -- ^ @curl@, curl License
    | D_FSL_1_0 -- ^ @D-FSL-1.0@, Deutsche Freie Software Lizenz
    | Diffmark -- ^ @diffmark@, diffmark license
    | DOC -- ^ @DOC@, DOC License
    | Dotseqn -- ^ @Dotseqn@, Dotseqn License
    | DSDP -- ^ @DSDP@, DSDP License
    | Dvipdfm -- ^ @dvipdfm@, dvipdfm License
    | ECL_1_0 -- ^ @ECL-1.0@, Educational Community License v1.0
    | ECL_2_0 -- ^ @ECL-2.0@, Educational Community License v2.0
    | EFL_1_0 -- ^ @EFL-1.0@, Eiffel Forum License v1.0
    | EFL_2_0 -- ^ @EFL-2.0@, Eiffel Forum License v2.0
    | EGenix -- ^ @eGenix@, eGenix.com Public License 1.1.0
    | Entessa -- ^ @Entessa@, Entessa Public License v1.0
    | EPL_1_0 -- ^ @EPL-1.0@, Eclipse Public License 1.0
    | EPL_2_0 -- ^ @EPL-2.0@, Eclipse Public License 2.0
    | ErlPL_1_1 -- ^ @ErlPL-1.1@, Erlang Public License v1.1
    | EUDatagrid -- ^ @EUDatagrid@, EU DataGrid Software License
    | EUPL_1_0 -- ^ @EUPL-1.0@, European Union Public License 1.0
    | EUPL_1_1 -- ^ @EUPL-1.1@, European Union Public License 1.1
    | EUPL_1_2 -- ^ @EUPL-1.2@, European Union Public License 1.2
    | Eurosym -- ^ @Eurosym@, Eurosym License
    | Fair -- ^ @Fair@, Fair License
    | Frameworx_1_0 -- ^ @Frameworx-1.0@, Frameworx Open License 1.0
    | FreeImage -- ^ @FreeImage@, FreeImage Public License v1.0
    | FSFAP -- ^ @FSFAP@, FSF All Permissive License
    | FSFULLR -- ^ @FSFULLR@, FSF Unlimited License (with License Retention)
    | FSFUL -- ^ @FSFUL@, FSF Unlimited License
    | FTL -- ^ @FTL@, Freetype Project License
    | GFDL_1_1_only -- ^ @GFDL-1.1-only@, GNU Free Documentation License v1.1 only
    | GFDL_1_1_or_later -- ^ @GFDL-1.1-or-later@, GNU Free Documentation License v1.1 or later
    | GFDL_1_2_only -- ^ @GFDL-1.2-only@, GNU Free Documentation License v1.2 only
    | GFDL_1_2_or_later -- ^ @GFDL-1.2-or-later@, GNU Free Documentation License v1.2 or later
    | GFDL_1_3_only -- ^ @GFDL-1.3-only@, GNU Free Documentation License v1.3 only
    | GFDL_1_3_or_later -- ^ @GFDL-1.3-or-later@, GNU Free Documentation License v1.3 or later
    | Giftware -- ^ @Giftware@, Giftware License
    | GL2PS -- ^ @GL2PS@, GL2PS License
    | Glide -- ^ @Glide@, 3dfx Glide License
    | Glulxe -- ^ @Glulxe@, Glulxe License
    | Gnuplot -- ^ @gnuplot@, gnuplot License
    | GPL_1_0_only -- ^ @GPL-1.0-only@, GNU General Public License v1.0 only
    | GPL_1_0_or_later -- ^ @GPL-1.0-or-later@, GNU General Public License v1.0 or later
    | GPL_2_0_only -- ^ @GPL-2.0-only@, GNU General Public License v2.0 only
    | GPL_2_0_or_later -- ^ @GPL-2.0-or-later@, GNU General Public License v2.0 or later
    | GPL_3_0_only -- ^ @GPL-3.0-only@, GNU General Public License v3.0 only
    | GPL_3_0_or_later -- ^ @GPL-3.0-or-later@, GNU General Public License v3.0 or later
    | GSOAP_1_3b -- ^ @gSOAP-1.3b@, gSOAP Public License v1.3b
    | HaskellReport -- ^ @HaskellReport@, Haskell Language Report License
    | HPND_sell_variant -- ^ @HPND-sell-variant@, Historical Permission Notice and Disclaimer - sell variant, SPDX License List 3.6
    | HPND -- ^ @HPND@, Historical Permission Notice and Disclaimer
    | IBM_pibs -- ^ @IBM-pibs@, IBM PowerPC Initialization and Boot Software
    | ICU -- ^ @ICU@, ICU License
    | IJG -- ^ @IJG@, Independent JPEG Group License
    | ImageMagick -- ^ @ImageMagick@, ImageMagick License
    | IMatix -- ^ @iMatix@, iMatix Standard Function Library Agreement
    | Imlib2 -- ^ @Imlib2@, Imlib2 License
    | Info_ZIP -- ^ @Info-ZIP@, Info-ZIP License
    | Intel_ACPI -- ^ @Intel-ACPI@, Intel ACPI Software License Agreement
    | Intel -- ^ @Intel@, Intel Open Source License
    | Interbase_1_0 -- ^ @Interbase-1.0@, Interbase Public License v1.0
    | IPA -- ^ @IPA@, IPA Font License
    | IPL_1_0 -- ^ @IPL-1.0@, IBM Public License v1.0
    | ISC -- ^ @ISC@, ISC License
    | JasPer_2_0 -- ^ @JasPer-2.0@, JasPer License
    | JPNIC -- ^ @JPNIC@, Japan Network Information Center License, SPDX License List 3.6
    | JSON -- ^ @JSON@, JSON License
    | LAL_1_2 -- ^ @LAL-1.2@, Licence Art Libre 1.2
    | LAL_1_3 -- ^ @LAL-1.3@, Licence Art Libre 1.3
    | Latex2e -- ^ @Latex2e@, Latex2e License
    | Leptonica -- ^ @Leptonica@, Leptonica License
    | LGPL_2_0_only -- ^ @LGPL-2.0-only@, GNU Library General Public License v2 only
    | LGPL_2_0_or_later -- ^ @LGPL-2.0-or-later@, GNU Library General Public License v2 or later
    | LGPL_2_1_only -- ^ @LGPL-2.1-only@, GNU Lesser General Public License v2.1 only
    | LGPL_2_1_or_later -- ^ @LGPL-2.1-or-later@, GNU Lesser General Public License v2.1 or later
    | LGPL_3_0_only -- ^ @LGPL-3.0-only@, GNU Lesser General Public License v3.0 only
    | LGPL_3_0_or_later -- ^ @LGPL-3.0-or-later@, GNU Lesser General Public License v3.0 or later
    | LGPLLR -- ^ @LGPLLR@, Lesser General Public License For Linguistic Resources
    | Libpng_2_0 -- ^ @libpng-2.0@, PNG Reference Library version 2, SPDX License List 3.6
    | Libpng -- ^ @Libpng@, libpng License
    | Libtiff -- ^ @libtiff@, libtiff License
    | LiLiQ_P_1_1 -- ^ @LiLiQ-P-1.1@, Licence Libre du Québec – Permissive version 1.1
    | LiLiQ_R_1_1 -- ^ @LiLiQ-R-1.1@, Licence Libre du Québec – Réciprocité version 1.1
    | LiLiQ_Rplus_1_1 -- ^ @LiLiQ-Rplus-1.1@, Licence Libre du Québec – Réciprocité forte version 1.1
    | Linux_OpenIB -- ^ @Linux-OpenIB@, Linux Kernel Variant of OpenIB.org license, SPDX License List 3.2, SPDX License List 3.6
    | LPL_1_02 -- ^ @LPL-1.02@, Lucent Public License v1.02
    | LPL_1_0 -- ^ @LPL-1.0@, Lucent Public License Version 1.0
    | LPPL_1_0 -- ^ @LPPL-1.0@, LaTeX Project Public License v1.0
    | LPPL_1_1 -- ^ @LPPL-1.1@, LaTeX Project Public License v1.1
    | LPPL_1_2 -- ^ @LPPL-1.2@, LaTeX Project Public License v1.2
    | LPPL_1_3a -- ^ @LPPL-1.3a@, LaTeX Project Public License v1.3a
    | LPPL_1_3c -- ^ @LPPL-1.3c@, LaTeX Project Public License v1.3c
    | MakeIndex -- ^ @MakeIndex@, MakeIndex License
    | MirOS -- ^ @MirOS@, MirOS License
    | MIT_0 -- ^ @MIT-0@, MIT No Attribution, SPDX License List 3.2, SPDX License List 3.6
    | MIT_advertising -- ^ @MIT-advertising@, Enlightenment License (e16)
    | MIT_CMU -- ^ @MIT-CMU@, CMU License
    | MIT_enna -- ^ @MIT-enna@, enna License
    | MIT_feh -- ^ @MIT-feh@, feh License
    | MITNFA -- ^ @MITNFA@, MIT +no-false-attribs license
    | MIT -- ^ @MIT@, MIT License
    | Motosoto -- ^ @Motosoto@, Motosoto License
    | Mpich2 -- ^ @mpich2@, mpich2 License
    | MPL_1_0 -- ^ @MPL-1.0@, Mozilla Public License 1.0
    | MPL_1_1 -- ^ @MPL-1.1@, Mozilla Public License 1.1
    | MPL_2_0_no_copyleft_exception -- ^ @MPL-2.0-no-copyleft-exception@, Mozilla Public License 2.0 (no copyleft exception)
    | MPL_2_0 -- ^ @MPL-2.0@, Mozilla Public License 2.0
    | MS_PL -- ^ @MS-PL@, Microsoft Public License
    | MS_RL -- ^ @MS-RL@, Microsoft Reciprocal License
    | MTLL -- ^ @MTLL@, Matrix Template Library License
    | Multics -- ^ @Multics@, Multics License
    | Mup -- ^ @Mup@, Mup License
    | NASA_1_3 -- ^ @NASA-1.3@, NASA Open Source Agreement 1.3
    | Naumen -- ^ @Naumen@, Naumen Public License
    | NBPL_1_0 -- ^ @NBPL-1.0@, Net Boolean Public License v1
    | NCSA -- ^ @NCSA@, University of Illinois/NCSA Open Source License
    | Net_SNMP -- ^ @Net-SNMP@, Net-SNMP License
    | NetCDF -- ^ @NetCDF@, NetCDF license
    | Newsletr -- ^ @Newsletr@, Newsletr License
    | NGPL -- ^ @NGPL@, Nethack General Public License
    | NLOD_1_0 -- ^ @NLOD-1.0@, Norwegian Licence for Open Government Data
    | NLPL -- ^ @NLPL@, No Limit Public License
    | Nokia -- ^ @Nokia@, Nokia Open Source License
    | NOSL -- ^ @NOSL@, Netizen Open Source License
    | Noweb -- ^ @Noweb@, Noweb License
    | NPL_1_0 -- ^ @NPL-1.0@, Netscape Public License v1.0
    | NPL_1_1 -- ^ @NPL-1.1@, Netscape Public License v1.1
    | NPOSL_3_0 -- ^ @NPOSL-3.0@, Non-Profit Open Software License 3.0
    | NRL -- ^ @NRL@, NRL License
    | NTP -- ^ @NTP@, NTP License
    | OCCT_PL -- ^ @OCCT-PL@, Open CASCADE Technology Public License
    | OCLC_2_0 -- ^ @OCLC-2.0@, OCLC Research Public License 2.0
    | ODbL_1_0 -- ^ @ODbL-1.0@, ODC Open Database License v1.0
    | ODC_By_1_0 -- ^ @ODC-By-1.0@, Open Data Commons Attribution License v1.0, SPDX License List 3.2, SPDX License List 3.6
    | OFL_1_0 -- ^ @OFL-1.0@, SIL Open Font License 1.0
    | OFL_1_1 -- ^ @OFL-1.1@, SIL Open Font License 1.1
    | OGL_UK_1_0 -- ^ @OGL-UK-1.0@, Open Government Licence v1.0, SPDX License List 3.6
    | OGL_UK_2_0 -- ^ @OGL-UK-2.0@, Open Government Licence v2.0, SPDX License List 3.6
    | OGL_UK_3_0 -- ^ @OGL-UK-3.0@, Open Government Licence v3.0, SPDX License List 3.6
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
    | OML -- ^ @OML@, Open Market License
    | OpenSSL -- ^ @OpenSSL@, OpenSSL License
    | OPL_1_0 -- ^ @OPL-1.0@, Open Public License v1.0
    | OSET_PL_2_1 -- ^ @OSET-PL-2.1@, OSET Public License version 2.1
    | OSL_1_0 -- ^ @OSL-1.0@, Open Software License 1.0
    | OSL_1_1 -- ^ @OSL-1.1@, Open Software License 1.1
    | OSL_2_0 -- ^ @OSL-2.0@, Open Software License 2.0
    | OSL_2_1 -- ^ @OSL-2.1@, Open Software License 2.1
    | OSL_3_0 -- ^ @OSL-3.0@, Open Software License 3.0
    | Parity_6_0_0 -- ^ @Parity-6.0.0@, The Parity Public License 6.0.0, SPDX License List 3.6
    | PDDL_1_0 -- ^ @PDDL-1.0@, ODC Public Domain Dedication & License 1.0
    | PHP_3_01 -- ^ @PHP-3.01@, PHP License v3.01
    | PHP_3_0 -- ^ @PHP-3.0@, PHP License v3.0
    | Plexus -- ^ @Plexus@, Plexus Classworlds License
    | PostgreSQL -- ^ @PostgreSQL@, PostgreSQL License
    | Psfrag -- ^ @psfrag@, psfrag License
    | Psutils -- ^ @psutils@, psutils License
    | Python_2_0 -- ^ @Python-2.0@, Python License 2.0
    | Qhull -- ^ @Qhull@, Qhull License
    | QPL_1_0 -- ^ @QPL-1.0@, Q Public License 1.0
    | Rdisc -- ^ @Rdisc@, Rdisc License
    | RHeCos_1_1 -- ^ @RHeCos-1.1@, Red Hat eCos Public License v1.1
    | RPL_1_1 -- ^ @RPL-1.1@, Reciprocal Public License 1.1
    | RPL_1_5 -- ^ @RPL-1.5@, Reciprocal Public License 1.5
    | RPSL_1_0 -- ^ @RPSL-1.0@, RealNetworks Public Source License v1.0
    | RSA_MD -- ^ @RSA-MD@, RSA Message-Digest License 
    | RSCPL -- ^ @RSCPL@, Ricoh Source Code Public License
    | Ruby -- ^ @Ruby@, Ruby License
    | SAX_PD -- ^ @SAX-PD@, Sax Public Domain Notice
    | Saxpath -- ^ @Saxpath@, Saxpath License
    | SCEA -- ^ @SCEA@, SCEA Shared Source License
    | Sendmail_8_23 -- ^ @Sendmail-8.23@, Sendmail License 8.23, SPDX License List 3.6
    | Sendmail -- ^ @Sendmail@, Sendmail License
    | SGI_B_1_0 -- ^ @SGI-B-1.0@, SGI Free Software License B v1.0
    | SGI_B_1_1 -- ^ @SGI-B-1.1@, SGI Free Software License B v1.1
    | SGI_B_2_0 -- ^ @SGI-B-2.0@, SGI Free Software License B v2.0
    | SHL_0_51 -- ^ @SHL-0.51@, Solderpad Hardware License, Version 0.51, SPDX License List 3.6
    | SHL_0_5 -- ^ @SHL-0.5@, Solderpad Hardware License v0.5, SPDX License List 3.6
    | SimPL_2_0 -- ^ @SimPL-2.0@, Simple Public License 2.0
    | SISSL_1_2 -- ^ @SISSL-1.2@, Sun Industry Standards Source License v1.2
    | SISSL -- ^ @SISSL@, Sun Industry Standards Source License v1.1
    | Sleepycat -- ^ @Sleepycat@, Sleepycat License
    | SMLNJ -- ^ @SMLNJ@, Standard ML of New Jersey License
    | SMPPL -- ^ @SMPPL@, Secure Messaging Protocol Public License
    | SNIA -- ^ @SNIA@, SNIA Public License 1.1
    | Spencer_86 -- ^ @Spencer-86@, Spencer License 86
    | Spencer_94 -- ^ @Spencer-94@, Spencer License 94
    | Spencer_99 -- ^ @Spencer-99@, Spencer License 99
    | SPL_1_0 -- ^ @SPL-1.0@, Sun Public License v1.0
    | SSPL_1_0 -- ^ @SSPL-1.0@, Server Side Public License, v 1, SPDX License List 3.6
    | SugarCRM_1_1_3 -- ^ @SugarCRM-1.1.3@, SugarCRM Public License v1.1.3
    | SWL -- ^ @SWL@, Scheme Widget Library (SWL) Software License Agreement
    | TAPR_OHL_1_0 -- ^ @TAPR-OHL-1.0@, TAPR Open Hardware License v1.0, SPDX License List 3.6
    | TCL -- ^ @TCL@, TCL/TK License
    | TCP_wrappers -- ^ @TCP-wrappers@, TCP Wrappers License
    | TMate -- ^ @TMate@, TMate Open Source License
    | TORQUE_1_1 -- ^ @TORQUE-1.1@, TORQUE v2.5+ Software License v1.1
    | TOSL -- ^ @TOSL@, Trusster Open Source License
    | TU_Berlin_1_0 -- ^ @TU-Berlin-1.0@, Technische Universitaet Berlin License 1.0, SPDX License List 3.2, SPDX License List 3.6
    | TU_Berlin_2_0 -- ^ @TU-Berlin-2.0@, Technische Universitaet Berlin License 2.0, SPDX License List 3.2, SPDX License List 3.6
    | Unicode_DFS_2015 -- ^ @Unicode-DFS-2015@, Unicode License Agreement - Data Files and Software (2015)
    | Unicode_DFS_2016 -- ^ @Unicode-DFS-2016@, Unicode License Agreement - Data Files and Software (2016)
    | Unicode_TOU -- ^ @Unicode-TOU@, Unicode Terms of Use
    | Unlicense -- ^ @Unlicense@, The Unlicense
    | UPL_1_0 -- ^ @UPL-1.0@, Universal Permissive License v1.0
    | Vim -- ^ @Vim@, Vim License
    | VOSTROM -- ^ @VOSTROM@, VOSTROM Public License for Open Source
    | VSL_1_0 -- ^ @VSL-1.0@, Vovida Software License v1.0
    | W3C_19980720 -- ^ @W3C-19980720@, W3C Software Notice and License (1998-07-20)
    | W3C_20150513 -- ^ @W3C-20150513@, W3C Software Notice and Document License (2015-05-13)
    | W3C -- ^ @W3C@, W3C Software Notice and License (2002-12-31)
    | Watcom_1_0 -- ^ @Watcom-1.0@, Sybase Open Watcom Public License 1.0
    | Wsuipa -- ^ @Wsuipa@, Wsuipa License
    | WTFPL -- ^ @WTFPL@, Do What The F*ck You Want To Public License
    | X11 -- ^ @X11@, X11 License
    | Xerox -- ^ @Xerox@, Xerox License
    | XFree86_1_1 -- ^ @XFree86-1.1@, XFree86 License 1.1
    | Xinetd -- ^ @xinetd@, xinetd License
    | Xnet -- ^ @Xnet@, X.Net License
    | Xpp -- ^ @xpp@, XPP License
    | XSkat -- ^ @XSkat@, XSkat License
    | YPL_1_0 -- ^ @YPL-1.0@, Yahoo! Public License v1.0
    | YPL_1_1 -- ^ @YPL-1.1@, Yahoo! Public License v1.1
    | Zed -- ^ @Zed@, Zed License
    | Zend_2_0 -- ^ @Zend-2.0@, Zend License v2.0
    | Zimbra_1_3 -- ^ @Zimbra-1.3@, Zimbra Public License v1.3
    | Zimbra_1_4 -- ^ @Zimbra-1.4@, Zimbra Public License v1.4
    | Zlib_acknowledgement -- ^ @zlib-acknowledgement@, zlib/libpng License with Acknowledgement
    | Zlib -- ^ @Zlib@, zlib License
    | ZPL_1_1 -- ^ @ZPL-1.1@, Zope Public License 1.1
    | ZPL_2_0 -- ^ @ZPL-2.0@, Zope Public License 2.0
    | ZPL_2_1 -- ^ @ZPL-2.1@, Zope Public License 2.1
  deriving (Eq, Ord, Enum, Bounded, Show, Read, Typeable, Data, Generic)

instance Binary LicenseId where
    -- Word16 is encoded in big endianess
    -- https://github.com/kolmodin/binary/blob/master/src/Data/Binary/Class.hs#L220-LL227
    put = Binary.putWord16be . fromIntegral . fromEnum
    get = do
        i <- Binary.getWord16be
        if i > fromIntegral (fromEnum (maxBound :: LicenseId))
        then fail "Too large LicenseId tag"
        else return (toEnum (fromIntegral i))

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
licenseId Adobe_2006 = "Adobe-2006"
licenseId Adobe_Glyph = "Adobe-Glyph"
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
licenseId AML = "AML"
licenseId AMPAS = "AMPAS"
licenseId ANTLR_PD = "ANTLR-PD"
licenseId Apache_1_0 = "Apache-1.0"
licenseId Apache_1_1 = "Apache-1.1"
licenseId Apache_2_0 = "Apache-2.0"
licenseId APAFML = "APAFML"
licenseId APL_1_0 = "APL-1.0"
licenseId APSL_1_0 = "APSL-1.0"
licenseId APSL_1_1 = "APSL-1.1"
licenseId APSL_1_2 = "APSL-1.2"
licenseId APSL_2_0 = "APSL-2.0"
licenseId Artistic_1_0_cl8 = "Artistic-1.0-cl8"
licenseId Artistic_1_0_Perl = "Artistic-1.0-Perl"
licenseId Artistic_1_0 = "Artistic-1.0"
licenseId Artistic_2_0 = "Artistic-2.0"
licenseId Bahyph = "Bahyph"
licenseId Barr = "Barr"
licenseId Beerware = "Beerware"
licenseId BitTorrent_1_0 = "BitTorrent-1.0"
licenseId BitTorrent_1_1 = "BitTorrent-1.1"
licenseId Blessing = "blessing"
licenseId BlueOak_1_0_0 = "BlueOak-1.0.0"
licenseId Borceux = "Borceux"
licenseId BSD_1_Clause = "BSD-1-Clause"
licenseId BSD_2_Clause_FreeBSD = "BSD-2-Clause-FreeBSD"
licenseId BSD_2_Clause_NetBSD = "BSD-2-Clause-NetBSD"
licenseId BSD_2_Clause_Patent = "BSD-2-Clause-Patent"
licenseId BSD_2_Clause = "BSD-2-Clause"
licenseId BSD_3_Clause_Attribution = "BSD-3-Clause-Attribution"
licenseId BSD_3_Clause_Clear = "BSD-3-Clause-Clear"
licenseId BSD_3_Clause_LBNL = "BSD-3-Clause-LBNL"
licenseId BSD_3_Clause_No_Nuclear_License_2014 = "BSD-3-Clause-No-Nuclear-License-2014"
licenseId BSD_3_Clause_No_Nuclear_License = "BSD-3-Clause-No-Nuclear-License"
licenseId BSD_3_Clause_No_Nuclear_Warranty = "BSD-3-Clause-No-Nuclear-Warranty"
licenseId BSD_3_Clause_Open_MPI = "BSD-3-Clause-Open-MPI"
licenseId BSD_3_Clause = "BSD-3-Clause"
licenseId BSD_4_Clause_UC = "BSD-4-Clause-UC"
licenseId BSD_4_Clause = "BSD-4-Clause"
licenseId BSD_Protection = "BSD-Protection"
licenseId BSD_Source_Code = "BSD-Source-Code"
licenseId BSL_1_0 = "BSL-1.0"
licenseId Bzip2_1_0_5 = "bzip2-1.0.5"
licenseId Bzip2_1_0_6 = "bzip2-1.0.6"
licenseId Caldera = "Caldera"
licenseId CATOSL_1_1 = "CATOSL-1.1"
licenseId CC_BY_1_0 = "CC-BY-1.0"
licenseId CC_BY_2_0 = "CC-BY-2.0"
licenseId CC_BY_2_5 = "CC-BY-2.5"
licenseId CC_BY_3_0 = "CC-BY-3.0"
licenseId CC_BY_4_0 = "CC-BY-4.0"
licenseId CC_BY_NC_1_0 = "CC-BY-NC-1.0"
licenseId CC_BY_NC_2_0 = "CC-BY-NC-2.0"
licenseId CC_BY_NC_2_5 = "CC-BY-NC-2.5"
licenseId CC_BY_NC_3_0 = "CC-BY-NC-3.0"
licenseId CC_BY_NC_4_0 = "CC-BY-NC-4.0"
licenseId CC_BY_NC_ND_1_0 = "CC-BY-NC-ND-1.0"
licenseId CC_BY_NC_ND_2_0 = "CC-BY-NC-ND-2.0"
licenseId CC_BY_NC_ND_2_5 = "CC-BY-NC-ND-2.5"
licenseId CC_BY_NC_ND_3_0 = "CC-BY-NC-ND-3.0"
licenseId CC_BY_NC_ND_4_0 = "CC-BY-NC-ND-4.0"
licenseId CC_BY_NC_SA_1_0 = "CC-BY-NC-SA-1.0"
licenseId CC_BY_NC_SA_2_0 = "CC-BY-NC-SA-2.0"
licenseId CC_BY_NC_SA_2_5 = "CC-BY-NC-SA-2.5"
licenseId CC_BY_NC_SA_3_0 = "CC-BY-NC-SA-3.0"
licenseId CC_BY_NC_SA_4_0 = "CC-BY-NC-SA-4.0"
licenseId CC_BY_ND_1_0 = "CC-BY-ND-1.0"
licenseId CC_BY_ND_2_0 = "CC-BY-ND-2.0"
licenseId CC_BY_ND_2_5 = "CC-BY-ND-2.5"
licenseId CC_BY_ND_3_0 = "CC-BY-ND-3.0"
licenseId CC_BY_ND_4_0 = "CC-BY-ND-4.0"
licenseId CC_BY_SA_1_0 = "CC-BY-SA-1.0"
licenseId CC_BY_SA_2_0 = "CC-BY-SA-2.0"
licenseId CC_BY_SA_2_5 = "CC-BY-SA-2.5"
licenseId CC_BY_SA_3_0 = "CC-BY-SA-3.0"
licenseId CC_BY_SA_4_0 = "CC-BY-SA-4.0"
licenseId CC_PDDC = "CC-PDDC"
licenseId CC0_1_0 = "CC0-1.0"
licenseId CDDL_1_0 = "CDDL-1.0"
licenseId CDDL_1_1 = "CDDL-1.1"
licenseId CDLA_Permissive_1_0 = "CDLA-Permissive-1.0"
licenseId CDLA_Sharing_1_0 = "CDLA-Sharing-1.0"
licenseId CECILL_1_0 = "CECILL-1.0"
licenseId CECILL_1_1 = "CECILL-1.1"
licenseId CECILL_2_0 = "CECILL-2.0"
licenseId CECILL_2_1 = "CECILL-2.1"
licenseId CECILL_B = "CECILL-B"
licenseId CECILL_C = "CECILL-C"
licenseId CERN_OHL_1_1 = "CERN-OHL-1.1"
licenseId CERN_OHL_1_2 = "CERN-OHL-1.2"
licenseId ClArtistic = "ClArtistic"
licenseId CNRI_Jython = "CNRI-Jython"
licenseId CNRI_Python_GPL_Compatible = "CNRI-Python-GPL-Compatible"
licenseId CNRI_Python = "CNRI-Python"
licenseId Condor_1_1 = "Condor-1.1"
licenseId Copyleft_next_0_3_0 = "copyleft-next-0.3.0"
licenseId Copyleft_next_0_3_1 = "copyleft-next-0.3.1"
licenseId CPAL_1_0 = "CPAL-1.0"
licenseId CPL_1_0 = "CPL-1.0"
licenseId CPOL_1_02 = "CPOL-1.02"
licenseId Crossword = "Crossword"
licenseId CrystalStacker = "CrystalStacker"
licenseId CUA_OPL_1_0 = "CUA-OPL-1.0"
licenseId Cube = "Cube"
licenseId Curl = "curl"
licenseId D_FSL_1_0 = "D-FSL-1.0"
licenseId Diffmark = "diffmark"
licenseId DOC = "DOC"
licenseId Dotseqn = "Dotseqn"
licenseId DSDP = "DSDP"
licenseId Dvipdfm = "dvipdfm"
licenseId ECL_1_0 = "ECL-1.0"
licenseId ECL_2_0 = "ECL-2.0"
licenseId EFL_1_0 = "EFL-1.0"
licenseId EFL_2_0 = "EFL-2.0"
licenseId EGenix = "eGenix"
licenseId Entessa = "Entessa"
licenseId EPL_1_0 = "EPL-1.0"
licenseId EPL_2_0 = "EPL-2.0"
licenseId ErlPL_1_1 = "ErlPL-1.1"
licenseId EUDatagrid = "EUDatagrid"
licenseId EUPL_1_0 = "EUPL-1.0"
licenseId EUPL_1_1 = "EUPL-1.1"
licenseId EUPL_1_2 = "EUPL-1.2"
licenseId Eurosym = "Eurosym"
licenseId Fair = "Fair"
licenseId Frameworx_1_0 = "Frameworx-1.0"
licenseId FreeImage = "FreeImage"
licenseId FSFAP = "FSFAP"
licenseId FSFULLR = "FSFULLR"
licenseId FSFUL = "FSFUL"
licenseId FTL = "FTL"
licenseId GFDL_1_1_only = "GFDL-1.1-only"
licenseId GFDL_1_1_or_later = "GFDL-1.1-or-later"
licenseId GFDL_1_2_only = "GFDL-1.2-only"
licenseId GFDL_1_2_or_later = "GFDL-1.2-or-later"
licenseId GFDL_1_3_only = "GFDL-1.3-only"
licenseId GFDL_1_3_or_later = "GFDL-1.3-or-later"
licenseId Giftware = "Giftware"
licenseId GL2PS = "GL2PS"
licenseId Glide = "Glide"
licenseId Glulxe = "Glulxe"
licenseId Gnuplot = "gnuplot"
licenseId GPL_1_0_only = "GPL-1.0-only"
licenseId GPL_1_0_or_later = "GPL-1.0-or-later"
licenseId GPL_2_0_only = "GPL-2.0-only"
licenseId GPL_2_0_or_later = "GPL-2.0-or-later"
licenseId GPL_3_0_only = "GPL-3.0-only"
licenseId GPL_3_0_or_later = "GPL-3.0-or-later"
licenseId GSOAP_1_3b = "gSOAP-1.3b"
licenseId HaskellReport = "HaskellReport"
licenseId HPND_sell_variant = "HPND-sell-variant"
licenseId HPND = "HPND"
licenseId IBM_pibs = "IBM-pibs"
licenseId ICU = "ICU"
licenseId IJG = "IJG"
licenseId ImageMagick = "ImageMagick"
licenseId IMatix = "iMatix"
licenseId Imlib2 = "Imlib2"
licenseId Info_ZIP = "Info-ZIP"
licenseId Intel_ACPI = "Intel-ACPI"
licenseId Intel = "Intel"
licenseId Interbase_1_0 = "Interbase-1.0"
licenseId IPA = "IPA"
licenseId IPL_1_0 = "IPL-1.0"
licenseId ISC = "ISC"
licenseId JasPer_2_0 = "JasPer-2.0"
licenseId JPNIC = "JPNIC"
licenseId JSON = "JSON"
licenseId LAL_1_2 = "LAL-1.2"
licenseId LAL_1_3 = "LAL-1.3"
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
licenseId Libtiff = "libtiff"
licenseId LiLiQ_P_1_1 = "LiLiQ-P-1.1"
licenseId LiLiQ_R_1_1 = "LiLiQ-R-1.1"
licenseId LiLiQ_Rplus_1_1 = "LiLiQ-Rplus-1.1"
licenseId Linux_OpenIB = "Linux-OpenIB"
licenseId LPL_1_02 = "LPL-1.02"
licenseId LPL_1_0 = "LPL-1.0"
licenseId LPPL_1_0 = "LPPL-1.0"
licenseId LPPL_1_1 = "LPPL-1.1"
licenseId LPPL_1_2 = "LPPL-1.2"
licenseId LPPL_1_3a = "LPPL-1.3a"
licenseId LPPL_1_3c = "LPPL-1.3c"
licenseId MakeIndex = "MakeIndex"
licenseId MirOS = "MirOS"
licenseId MIT_0 = "MIT-0"
licenseId MIT_advertising = "MIT-advertising"
licenseId MIT_CMU = "MIT-CMU"
licenseId MIT_enna = "MIT-enna"
licenseId MIT_feh = "MIT-feh"
licenseId MITNFA = "MITNFA"
licenseId MIT = "MIT"
licenseId Motosoto = "Motosoto"
licenseId Mpich2 = "mpich2"
licenseId MPL_1_0 = "MPL-1.0"
licenseId MPL_1_1 = "MPL-1.1"
licenseId MPL_2_0_no_copyleft_exception = "MPL-2.0-no-copyleft-exception"
licenseId MPL_2_0 = "MPL-2.0"
licenseId MS_PL = "MS-PL"
licenseId MS_RL = "MS-RL"
licenseId MTLL = "MTLL"
licenseId Multics = "Multics"
licenseId Mup = "Mup"
licenseId NASA_1_3 = "NASA-1.3"
licenseId Naumen = "Naumen"
licenseId NBPL_1_0 = "NBPL-1.0"
licenseId NCSA = "NCSA"
licenseId Net_SNMP = "Net-SNMP"
licenseId NetCDF = "NetCDF"
licenseId Newsletr = "Newsletr"
licenseId NGPL = "NGPL"
licenseId NLOD_1_0 = "NLOD-1.0"
licenseId NLPL = "NLPL"
licenseId Nokia = "Nokia"
licenseId NOSL = "NOSL"
licenseId Noweb = "Noweb"
licenseId NPL_1_0 = "NPL-1.0"
licenseId NPL_1_1 = "NPL-1.1"
licenseId NPOSL_3_0 = "NPOSL-3.0"
licenseId NRL = "NRL"
licenseId NTP = "NTP"
licenseId OCCT_PL = "OCCT-PL"
licenseId OCLC_2_0 = "OCLC-2.0"
licenseId ODbL_1_0 = "ODbL-1.0"
licenseId ODC_By_1_0 = "ODC-By-1.0"
licenseId OFL_1_0 = "OFL-1.0"
licenseId OFL_1_1 = "OFL-1.1"
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
licenseId OML = "OML"
licenseId OpenSSL = "OpenSSL"
licenseId OPL_1_0 = "OPL-1.0"
licenseId OSET_PL_2_1 = "OSET-PL-2.1"
licenseId OSL_1_0 = "OSL-1.0"
licenseId OSL_1_1 = "OSL-1.1"
licenseId OSL_2_0 = "OSL-2.0"
licenseId OSL_2_1 = "OSL-2.1"
licenseId OSL_3_0 = "OSL-3.0"
licenseId Parity_6_0_0 = "Parity-6.0.0"
licenseId PDDL_1_0 = "PDDL-1.0"
licenseId PHP_3_01 = "PHP-3.01"
licenseId PHP_3_0 = "PHP-3.0"
licenseId Plexus = "Plexus"
licenseId PostgreSQL = "PostgreSQL"
licenseId Psfrag = "psfrag"
licenseId Psutils = "psutils"
licenseId Python_2_0 = "Python-2.0"
licenseId Qhull = "Qhull"
licenseId QPL_1_0 = "QPL-1.0"
licenseId Rdisc = "Rdisc"
licenseId RHeCos_1_1 = "RHeCos-1.1"
licenseId RPL_1_1 = "RPL-1.1"
licenseId RPL_1_5 = "RPL-1.5"
licenseId RPSL_1_0 = "RPSL-1.0"
licenseId RSA_MD = "RSA-MD"
licenseId RSCPL = "RSCPL"
licenseId Ruby = "Ruby"
licenseId SAX_PD = "SAX-PD"
licenseId Saxpath = "Saxpath"
licenseId SCEA = "SCEA"
licenseId Sendmail_8_23 = "Sendmail-8.23"
licenseId Sendmail = "Sendmail"
licenseId SGI_B_1_0 = "SGI-B-1.0"
licenseId SGI_B_1_1 = "SGI-B-1.1"
licenseId SGI_B_2_0 = "SGI-B-2.0"
licenseId SHL_0_51 = "SHL-0.51"
licenseId SHL_0_5 = "SHL-0.5"
licenseId SimPL_2_0 = "SimPL-2.0"
licenseId SISSL_1_2 = "SISSL-1.2"
licenseId SISSL = "SISSL"
licenseId Sleepycat = "Sleepycat"
licenseId SMLNJ = "SMLNJ"
licenseId SMPPL = "SMPPL"
licenseId SNIA = "SNIA"
licenseId Spencer_86 = "Spencer-86"
licenseId Spencer_94 = "Spencer-94"
licenseId Spencer_99 = "Spencer-99"
licenseId SPL_1_0 = "SPL-1.0"
licenseId SSPL_1_0 = "SSPL-1.0"
licenseId SugarCRM_1_1_3 = "SugarCRM-1.1.3"
licenseId SWL = "SWL"
licenseId TAPR_OHL_1_0 = "TAPR-OHL-1.0"
licenseId TCL = "TCL"
licenseId TCP_wrappers = "TCP-wrappers"
licenseId TMate = "TMate"
licenseId TORQUE_1_1 = "TORQUE-1.1"
licenseId TOSL = "TOSL"
licenseId TU_Berlin_1_0 = "TU-Berlin-1.0"
licenseId TU_Berlin_2_0 = "TU-Berlin-2.0"
licenseId Unicode_DFS_2015 = "Unicode-DFS-2015"
licenseId Unicode_DFS_2016 = "Unicode-DFS-2016"
licenseId Unicode_TOU = "Unicode-TOU"
licenseId Unlicense = "Unlicense"
licenseId UPL_1_0 = "UPL-1.0"
licenseId Vim = "Vim"
licenseId VOSTROM = "VOSTROM"
licenseId VSL_1_0 = "VSL-1.0"
licenseId W3C_19980720 = "W3C-19980720"
licenseId W3C_20150513 = "W3C-20150513"
licenseId W3C = "W3C"
licenseId Watcom_1_0 = "Watcom-1.0"
licenseId Wsuipa = "Wsuipa"
licenseId WTFPL = "WTFPL"
licenseId X11 = "X11"
licenseId Xerox = "Xerox"
licenseId XFree86_1_1 = "XFree86-1.1"
licenseId Xinetd = "xinetd"
licenseId Xnet = "Xnet"
licenseId Xpp = "xpp"
licenseId XSkat = "XSkat"
licenseId YPL_1_0 = "YPL-1.0"
licenseId YPL_1_1 = "YPL-1.1"
licenseId Zed = "Zed"
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
licenseName Adobe_2006 = "Adobe Systems Incorporated Source Code License Agreement"
licenseName Adobe_Glyph = "Adobe Glyph List License"
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
licenseName AML = "Apple MIT License"
licenseName AMPAS = "Academy of Motion Picture Arts and Sciences BSD"
licenseName ANTLR_PD = "ANTLR Software Rights Notice"
licenseName Apache_1_0 = "Apache License 1.0"
licenseName Apache_1_1 = "Apache License 1.1"
licenseName Apache_2_0 = "Apache License 2.0"
licenseName APAFML = "Adobe Postscript AFM License"
licenseName APL_1_0 = "Adaptive Public License 1.0"
licenseName APSL_1_0 = "Apple Public Source License 1.0"
licenseName APSL_1_1 = "Apple Public Source License 1.1"
licenseName APSL_1_2 = "Apple Public Source License 1.2"
licenseName APSL_2_0 = "Apple Public Source License 2.0"
licenseName Artistic_1_0_cl8 = "Artistic License 1.0 w/clause 8"
licenseName Artistic_1_0_Perl = "Artistic License 1.0 (Perl)"
licenseName Artistic_1_0 = "Artistic License 1.0"
licenseName Artistic_2_0 = "Artistic License 2.0"
licenseName Bahyph = "Bahyph License"
licenseName Barr = "Barr License"
licenseName Beerware = "Beerware License"
licenseName BitTorrent_1_0 = "BitTorrent Open Source License v1.0"
licenseName BitTorrent_1_1 = "BitTorrent Open Source License v1.1"
licenseName Blessing = "SQLite Blessing"
licenseName BlueOak_1_0_0 = "Blue Oak Model License 1.0.0"
licenseName Borceux = "Borceux license"
licenseName BSD_1_Clause = "BSD 1-Clause License"
licenseName BSD_2_Clause_FreeBSD = "BSD 2-Clause FreeBSD License"
licenseName BSD_2_Clause_NetBSD = "BSD 2-Clause NetBSD License"
licenseName BSD_2_Clause_Patent = "BSD-2-Clause Plus Patent License"
licenseName BSD_2_Clause = "BSD 2-Clause \"Simplified\" License"
licenseName BSD_3_Clause_Attribution = "BSD with attribution"
licenseName BSD_3_Clause_Clear = "BSD 3-Clause Clear License"
licenseName BSD_3_Clause_LBNL = "Lawrence Berkeley National Labs BSD variant license"
licenseName BSD_3_Clause_No_Nuclear_License_2014 = "BSD 3-Clause No Nuclear License 2014"
licenseName BSD_3_Clause_No_Nuclear_License = "BSD 3-Clause No Nuclear License"
licenseName BSD_3_Clause_No_Nuclear_Warranty = "BSD 3-Clause No Nuclear Warranty"
licenseName BSD_3_Clause_Open_MPI = "BSD 3-Clause Open MPI variant"
licenseName BSD_3_Clause = "BSD 3-Clause \"New\" or \"Revised\" License"
licenseName BSD_4_Clause_UC = "BSD-4-Clause (University of California-Specific)"
licenseName BSD_4_Clause = "BSD 4-Clause \"Original\" or \"Old\" License"
licenseName BSD_Protection = "BSD Protection License"
licenseName BSD_Source_Code = "BSD Source Code Attribution"
licenseName BSL_1_0 = "Boost Software License 1.0"
licenseName Bzip2_1_0_5 = "bzip2 and libbzip2 License v1.0.5"
licenseName Bzip2_1_0_6 = "bzip2 and libbzip2 License v1.0.6"
licenseName Caldera = "Caldera License"
licenseName CATOSL_1_1 = "Computer Associates Trusted Open Source License 1.1"
licenseName CC_BY_1_0 = "Creative Commons Attribution 1.0 Generic"
licenseName CC_BY_2_0 = "Creative Commons Attribution 2.0 Generic"
licenseName CC_BY_2_5 = "Creative Commons Attribution 2.5 Generic"
licenseName CC_BY_3_0 = "Creative Commons Attribution 3.0 Unported"
licenseName CC_BY_4_0 = "Creative Commons Attribution 4.0 International"
licenseName CC_BY_NC_1_0 = "Creative Commons Attribution Non Commercial 1.0 Generic"
licenseName CC_BY_NC_2_0 = "Creative Commons Attribution Non Commercial 2.0 Generic"
licenseName CC_BY_NC_2_5 = "Creative Commons Attribution Non Commercial 2.5 Generic"
licenseName CC_BY_NC_3_0 = "Creative Commons Attribution Non Commercial 3.0 Unported"
licenseName CC_BY_NC_4_0 = "Creative Commons Attribution Non Commercial 4.0 International"
licenseName CC_BY_NC_ND_1_0 = "Creative Commons Attribution Non Commercial No Derivatives 1.0 Generic"
licenseName CC_BY_NC_ND_2_0 = "Creative Commons Attribution Non Commercial No Derivatives 2.0 Generic"
licenseName CC_BY_NC_ND_2_5 = "Creative Commons Attribution Non Commercial No Derivatives 2.5 Generic"
licenseName CC_BY_NC_ND_3_0 = "Creative Commons Attribution Non Commercial No Derivatives 3.0 Unported"
licenseName CC_BY_NC_ND_4_0 = "Creative Commons Attribution Non Commercial No Derivatives 4.0 International"
licenseName CC_BY_NC_SA_1_0 = "Creative Commons Attribution Non Commercial Share Alike 1.0 Generic"
licenseName CC_BY_NC_SA_2_0 = "Creative Commons Attribution Non Commercial Share Alike 2.0 Generic"
licenseName CC_BY_NC_SA_2_5 = "Creative Commons Attribution Non Commercial Share Alike 2.5 Generic"
licenseName CC_BY_NC_SA_3_0 = "Creative Commons Attribution Non Commercial Share Alike 3.0 Unported"
licenseName CC_BY_NC_SA_4_0 = "Creative Commons Attribution Non Commercial Share Alike 4.0 International"
licenseName CC_BY_ND_1_0 = "Creative Commons Attribution No Derivatives 1.0 Generic"
licenseName CC_BY_ND_2_0 = "Creative Commons Attribution No Derivatives 2.0 Generic"
licenseName CC_BY_ND_2_5 = "Creative Commons Attribution No Derivatives 2.5 Generic"
licenseName CC_BY_ND_3_0 = "Creative Commons Attribution No Derivatives 3.0 Unported"
licenseName CC_BY_ND_4_0 = "Creative Commons Attribution No Derivatives 4.0 International"
licenseName CC_BY_SA_1_0 = "Creative Commons Attribution Share Alike 1.0 Generic"
licenseName CC_BY_SA_2_0 = "Creative Commons Attribution Share Alike 2.0 Generic"
licenseName CC_BY_SA_2_5 = "Creative Commons Attribution Share Alike 2.5 Generic"
licenseName CC_BY_SA_3_0 = "Creative Commons Attribution Share Alike 3.0 Unported"
licenseName CC_BY_SA_4_0 = "Creative Commons Attribution Share Alike 4.0 International"
licenseName CC_PDDC = "Creative Commons Public Domain Dedication and Certification"
licenseName CC0_1_0 = "Creative Commons Zero v1.0 Universal"
licenseName CDDL_1_0 = "Common Development and Distribution License 1.0"
licenseName CDDL_1_1 = "Common Development and Distribution License 1.1"
licenseName CDLA_Permissive_1_0 = "Community Data License Agreement Permissive 1.0"
licenseName CDLA_Sharing_1_0 = "Community Data License Agreement Sharing 1.0"
licenseName CECILL_1_0 = "CeCILL Free Software License Agreement v1.0"
licenseName CECILL_1_1 = "CeCILL Free Software License Agreement v1.1"
licenseName CECILL_2_0 = "CeCILL Free Software License Agreement v2.0"
licenseName CECILL_2_1 = "CeCILL Free Software License Agreement v2.1"
licenseName CECILL_B = "CeCILL-B Free Software License Agreement"
licenseName CECILL_C = "CeCILL-C Free Software License Agreement"
licenseName CERN_OHL_1_1 = "CERN Open Hardware License v1.1"
licenseName CERN_OHL_1_2 = "CERN Open Hardware Licence v1.2"
licenseName ClArtistic = "Clarified Artistic License"
licenseName CNRI_Jython = "CNRI Jython License"
licenseName CNRI_Python_GPL_Compatible = "CNRI Python Open Source GPL Compatible License Agreement"
licenseName CNRI_Python = "CNRI Python License"
licenseName Condor_1_1 = "Condor Public License v1.1"
licenseName Copyleft_next_0_3_0 = "copyleft-next 0.3.0"
licenseName Copyleft_next_0_3_1 = "copyleft-next 0.3.1"
licenseName CPAL_1_0 = "Common Public Attribution License 1.0"
licenseName CPL_1_0 = "Common Public License 1.0"
licenseName CPOL_1_02 = "Code Project Open License 1.02"
licenseName Crossword = "Crossword License"
licenseName CrystalStacker = "CrystalStacker License"
licenseName CUA_OPL_1_0 = "CUA Office Public License v1.0"
licenseName Cube = "Cube License"
licenseName Curl = "curl License"
licenseName D_FSL_1_0 = "Deutsche Freie Software Lizenz"
licenseName Diffmark = "diffmark license"
licenseName DOC = "DOC License"
licenseName Dotseqn = "Dotseqn License"
licenseName DSDP = "DSDP License"
licenseName Dvipdfm = "dvipdfm License"
licenseName ECL_1_0 = "Educational Community License v1.0"
licenseName ECL_2_0 = "Educational Community License v2.0"
licenseName EFL_1_0 = "Eiffel Forum License v1.0"
licenseName EFL_2_0 = "Eiffel Forum License v2.0"
licenseName EGenix = "eGenix.com Public License 1.1.0"
licenseName Entessa = "Entessa Public License v1.0"
licenseName EPL_1_0 = "Eclipse Public License 1.0"
licenseName EPL_2_0 = "Eclipse Public License 2.0"
licenseName ErlPL_1_1 = "Erlang Public License v1.1"
licenseName EUDatagrid = "EU DataGrid Software License"
licenseName EUPL_1_0 = "European Union Public License 1.0"
licenseName EUPL_1_1 = "European Union Public License 1.1"
licenseName EUPL_1_2 = "European Union Public License 1.2"
licenseName Eurosym = "Eurosym License"
licenseName Fair = "Fair License"
licenseName Frameworx_1_0 = "Frameworx Open License 1.0"
licenseName FreeImage = "FreeImage Public License v1.0"
licenseName FSFAP = "FSF All Permissive License"
licenseName FSFULLR = "FSF Unlimited License (with License Retention)"
licenseName FSFUL = "FSF Unlimited License"
licenseName FTL = "Freetype Project License"
licenseName GFDL_1_1_only = "GNU Free Documentation License v1.1 only"
licenseName GFDL_1_1_or_later = "GNU Free Documentation License v1.1 or later"
licenseName GFDL_1_2_only = "GNU Free Documentation License v1.2 only"
licenseName GFDL_1_2_or_later = "GNU Free Documentation License v1.2 or later"
licenseName GFDL_1_3_only = "GNU Free Documentation License v1.3 only"
licenseName GFDL_1_3_or_later = "GNU Free Documentation License v1.3 or later"
licenseName Giftware = "Giftware License"
licenseName GL2PS = "GL2PS License"
licenseName Glide = "3dfx Glide License"
licenseName Glulxe = "Glulxe License"
licenseName Gnuplot = "gnuplot License"
licenseName GPL_1_0_only = "GNU General Public License v1.0 only"
licenseName GPL_1_0_or_later = "GNU General Public License v1.0 or later"
licenseName GPL_2_0_only = "GNU General Public License v2.0 only"
licenseName GPL_2_0_or_later = "GNU General Public License v2.0 or later"
licenseName GPL_3_0_only = "GNU General Public License v3.0 only"
licenseName GPL_3_0_or_later = "GNU General Public License v3.0 or later"
licenseName GSOAP_1_3b = "gSOAP Public License v1.3b"
licenseName HaskellReport = "Haskell Language Report License"
licenseName HPND_sell_variant = "Historical Permission Notice and Disclaimer - sell variant"
licenseName HPND = "Historical Permission Notice and Disclaimer"
licenseName IBM_pibs = "IBM PowerPC Initialization and Boot Software"
licenseName ICU = "ICU License"
licenseName IJG = "Independent JPEG Group License"
licenseName ImageMagick = "ImageMagick License"
licenseName IMatix = "iMatix Standard Function Library Agreement"
licenseName Imlib2 = "Imlib2 License"
licenseName Info_ZIP = "Info-ZIP License"
licenseName Intel_ACPI = "Intel ACPI Software License Agreement"
licenseName Intel = "Intel Open Source License"
licenseName Interbase_1_0 = "Interbase Public License v1.0"
licenseName IPA = "IPA Font License"
licenseName IPL_1_0 = "IBM Public License v1.0"
licenseName ISC = "ISC License"
licenseName JasPer_2_0 = "JasPer License"
licenseName JPNIC = "Japan Network Information Center License"
licenseName JSON = "JSON License"
licenseName LAL_1_2 = "Licence Art Libre 1.2"
licenseName LAL_1_3 = "Licence Art Libre 1.3"
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
licenseName Libtiff = "libtiff License"
licenseName LiLiQ_P_1_1 = "Licence Libre du Qu\233bec \8211 Permissive version 1.1"
licenseName LiLiQ_R_1_1 = "Licence Libre du Qu\233bec \8211 R\233ciprocit\233 version 1.1"
licenseName LiLiQ_Rplus_1_1 = "Licence Libre du Qu\233bec \8211 R\233ciprocit\233 forte version 1.1"
licenseName Linux_OpenIB = "Linux Kernel Variant of OpenIB.org license"
licenseName LPL_1_02 = "Lucent Public License v1.02"
licenseName LPL_1_0 = "Lucent Public License Version 1.0"
licenseName LPPL_1_0 = "LaTeX Project Public License v1.0"
licenseName LPPL_1_1 = "LaTeX Project Public License v1.1"
licenseName LPPL_1_2 = "LaTeX Project Public License v1.2"
licenseName LPPL_1_3a = "LaTeX Project Public License v1.3a"
licenseName LPPL_1_3c = "LaTeX Project Public License v1.3c"
licenseName MakeIndex = "MakeIndex License"
licenseName MirOS = "MirOS License"
licenseName MIT_0 = "MIT No Attribution"
licenseName MIT_advertising = "Enlightenment License (e16)"
licenseName MIT_CMU = "CMU License"
licenseName MIT_enna = "enna License"
licenseName MIT_feh = "feh License"
licenseName MITNFA = "MIT +no-false-attribs license"
licenseName MIT = "MIT License"
licenseName Motosoto = "Motosoto License"
licenseName Mpich2 = "mpich2 License"
licenseName MPL_1_0 = "Mozilla Public License 1.0"
licenseName MPL_1_1 = "Mozilla Public License 1.1"
licenseName MPL_2_0_no_copyleft_exception = "Mozilla Public License 2.0 (no copyleft exception)"
licenseName MPL_2_0 = "Mozilla Public License 2.0"
licenseName MS_PL = "Microsoft Public License"
licenseName MS_RL = "Microsoft Reciprocal License"
licenseName MTLL = "Matrix Template Library License"
licenseName Multics = "Multics License"
licenseName Mup = "Mup License"
licenseName NASA_1_3 = "NASA Open Source Agreement 1.3"
licenseName Naumen = "Naumen Public License"
licenseName NBPL_1_0 = "Net Boolean Public License v1"
licenseName NCSA = "University of Illinois/NCSA Open Source License"
licenseName Net_SNMP = "Net-SNMP License"
licenseName NetCDF = "NetCDF license"
licenseName Newsletr = "Newsletr License"
licenseName NGPL = "Nethack General Public License"
licenseName NLOD_1_0 = "Norwegian Licence for Open Government Data"
licenseName NLPL = "No Limit Public License"
licenseName Nokia = "Nokia Open Source License"
licenseName NOSL = "Netizen Open Source License"
licenseName Noweb = "Noweb License"
licenseName NPL_1_0 = "Netscape Public License v1.0"
licenseName NPL_1_1 = "Netscape Public License v1.1"
licenseName NPOSL_3_0 = "Non-Profit Open Software License 3.0"
licenseName NRL = "NRL License"
licenseName NTP = "NTP License"
licenseName OCCT_PL = "Open CASCADE Technology Public License"
licenseName OCLC_2_0 = "OCLC Research Public License 2.0"
licenseName ODbL_1_0 = "ODC Open Database License v1.0"
licenseName ODC_By_1_0 = "Open Data Commons Attribution License v1.0"
licenseName OFL_1_0 = "SIL Open Font License 1.0"
licenseName OFL_1_1 = "SIL Open Font License 1.1"
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
licenseName OML = "Open Market License"
licenseName OpenSSL = "OpenSSL License"
licenseName OPL_1_0 = "Open Public License v1.0"
licenseName OSET_PL_2_1 = "OSET Public License version 2.1"
licenseName OSL_1_0 = "Open Software License 1.0"
licenseName OSL_1_1 = "Open Software License 1.1"
licenseName OSL_2_0 = "Open Software License 2.0"
licenseName OSL_2_1 = "Open Software License 2.1"
licenseName OSL_3_0 = "Open Software License 3.0"
licenseName Parity_6_0_0 = "The Parity Public License 6.0.0"
licenseName PDDL_1_0 = "ODC Public Domain Dedication & License 1.0"
licenseName PHP_3_01 = "PHP License v3.01"
licenseName PHP_3_0 = "PHP License v3.0"
licenseName Plexus = "Plexus Classworlds License"
licenseName PostgreSQL = "PostgreSQL License"
licenseName Psfrag = "psfrag License"
licenseName Psutils = "psutils License"
licenseName Python_2_0 = "Python License 2.0"
licenseName Qhull = "Qhull License"
licenseName QPL_1_0 = "Q Public License 1.0"
licenseName Rdisc = "Rdisc License"
licenseName RHeCos_1_1 = "Red Hat eCos Public License v1.1"
licenseName RPL_1_1 = "Reciprocal Public License 1.1"
licenseName RPL_1_5 = "Reciprocal Public License 1.5"
licenseName RPSL_1_0 = "RealNetworks Public Source License v1.0"
licenseName RSA_MD = "RSA Message-Digest License "
licenseName RSCPL = "Ricoh Source Code Public License"
licenseName Ruby = "Ruby License"
licenseName SAX_PD = "Sax Public Domain Notice"
licenseName Saxpath = "Saxpath License"
licenseName SCEA = "SCEA Shared Source License"
licenseName Sendmail_8_23 = "Sendmail License 8.23"
licenseName Sendmail = "Sendmail License"
licenseName SGI_B_1_0 = "SGI Free Software License B v1.0"
licenseName SGI_B_1_1 = "SGI Free Software License B v1.1"
licenseName SGI_B_2_0 = "SGI Free Software License B v2.0"
licenseName SHL_0_51 = "Solderpad Hardware License, Version 0.51"
licenseName SHL_0_5 = "Solderpad Hardware License v0.5"
licenseName SimPL_2_0 = "Simple Public License 2.0"
licenseName SISSL_1_2 = "Sun Industry Standards Source License v1.2"
licenseName SISSL = "Sun Industry Standards Source License v1.1"
licenseName Sleepycat = "Sleepycat License"
licenseName SMLNJ = "Standard ML of New Jersey License"
licenseName SMPPL = "Secure Messaging Protocol Public License"
licenseName SNIA = "SNIA Public License 1.1"
licenseName Spencer_86 = "Spencer License 86"
licenseName Spencer_94 = "Spencer License 94"
licenseName Spencer_99 = "Spencer License 99"
licenseName SPL_1_0 = "Sun Public License v1.0"
licenseName SSPL_1_0 = "Server Side Public License, v 1"
licenseName SugarCRM_1_1_3 = "SugarCRM Public License v1.1.3"
licenseName SWL = "Scheme Widget Library (SWL) Software License Agreement"
licenseName TAPR_OHL_1_0 = "TAPR Open Hardware License v1.0"
licenseName TCL = "TCL/TK License"
licenseName TCP_wrappers = "TCP Wrappers License"
licenseName TMate = "TMate Open Source License"
licenseName TORQUE_1_1 = "TORQUE v2.5+ Software License v1.1"
licenseName TOSL = "Trusster Open Source License"
licenseName TU_Berlin_1_0 = "Technische Universitaet Berlin License 1.0"
licenseName TU_Berlin_2_0 = "Technische Universitaet Berlin License 2.0"
licenseName Unicode_DFS_2015 = "Unicode License Agreement - Data Files and Software (2015)"
licenseName Unicode_DFS_2016 = "Unicode License Agreement - Data Files and Software (2016)"
licenseName Unicode_TOU = "Unicode Terms of Use"
licenseName Unlicense = "The Unlicense"
licenseName UPL_1_0 = "Universal Permissive License v1.0"
licenseName Vim = "Vim License"
licenseName VOSTROM = "VOSTROM Public License for Open Source"
licenseName VSL_1_0 = "Vovida Software License v1.0"
licenseName W3C_19980720 = "W3C Software Notice and License (1998-07-20)"
licenseName W3C_20150513 = "W3C Software Notice and Document License (2015-05-13)"
licenseName W3C = "W3C Software Notice and License (2002-12-31)"
licenseName Watcom_1_0 = "Sybase Open Watcom Public License 1.0"
licenseName Wsuipa = "Wsuipa License"
licenseName WTFPL = "Do What The F*ck You Want To Public License"
licenseName X11 = "X11 License"
licenseName Xerox = "Xerox License"
licenseName XFree86_1_1 = "XFree86 License 1.1"
licenseName Xinetd = "xinetd License"
licenseName Xnet = "X.Net License"
licenseName Xpp = "XPP License"
licenseName XSkat = "XSkat License"
licenseName YPL_1_0 = "Yahoo! Public License v1.0"
licenseName YPL_1_1 = "Yahoo! Public License v1.1"
licenseName Zed = "Zed License"
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
licenseIsOsiApproved Abstyles = False
licenseIsOsiApproved Adobe_2006 = False
licenseIsOsiApproved Adobe_Glyph = False
licenseIsOsiApproved ADSL = False
licenseIsOsiApproved AFL_1_1 = True
licenseIsOsiApproved AFL_1_2 = True
licenseIsOsiApproved AFL_2_0 = True
licenseIsOsiApproved AFL_2_1 = True
licenseIsOsiApproved AFL_3_0 = True
licenseIsOsiApproved Afmparse = False
licenseIsOsiApproved AGPL_1_0 = False
licenseIsOsiApproved AGPL_1_0_only = False
licenseIsOsiApproved AGPL_1_0_or_later = False
licenseIsOsiApproved AGPL_3_0_only = True
licenseIsOsiApproved AGPL_3_0_or_later = True
licenseIsOsiApproved Aladdin = False
licenseIsOsiApproved AMDPLPA = False
licenseIsOsiApproved AML = False
licenseIsOsiApproved AMPAS = False
licenseIsOsiApproved ANTLR_PD = False
licenseIsOsiApproved Apache_1_0 = False
licenseIsOsiApproved Apache_1_1 = True
licenseIsOsiApproved Apache_2_0 = True
licenseIsOsiApproved APAFML = False
licenseIsOsiApproved APL_1_0 = True
licenseIsOsiApproved APSL_1_0 = True
licenseIsOsiApproved APSL_1_1 = True
licenseIsOsiApproved APSL_1_2 = True
licenseIsOsiApproved APSL_2_0 = True
licenseIsOsiApproved Artistic_1_0_cl8 = True
licenseIsOsiApproved Artistic_1_0_Perl = True
licenseIsOsiApproved Artistic_1_0 = True
licenseIsOsiApproved Artistic_2_0 = True
licenseIsOsiApproved Bahyph = False
licenseIsOsiApproved Barr = False
licenseIsOsiApproved Beerware = False
licenseIsOsiApproved BitTorrent_1_0 = False
licenseIsOsiApproved BitTorrent_1_1 = False
licenseIsOsiApproved Blessing = False
licenseIsOsiApproved BlueOak_1_0_0 = False
licenseIsOsiApproved Borceux = False
licenseIsOsiApproved BSD_1_Clause = False
licenseIsOsiApproved BSD_2_Clause_FreeBSD = False
licenseIsOsiApproved BSD_2_Clause_NetBSD = False
licenseIsOsiApproved BSD_2_Clause_Patent = True
licenseIsOsiApproved BSD_2_Clause = True
licenseIsOsiApproved BSD_3_Clause_Attribution = False
licenseIsOsiApproved BSD_3_Clause_Clear = False
licenseIsOsiApproved BSD_3_Clause_LBNL = True
licenseIsOsiApproved BSD_3_Clause_No_Nuclear_License_2014 = False
licenseIsOsiApproved BSD_3_Clause_No_Nuclear_License = False
licenseIsOsiApproved BSD_3_Clause_No_Nuclear_Warranty = False
licenseIsOsiApproved BSD_3_Clause_Open_MPI = False
licenseIsOsiApproved BSD_3_Clause = True
licenseIsOsiApproved BSD_4_Clause_UC = False
licenseIsOsiApproved BSD_4_Clause = False
licenseIsOsiApproved BSD_Protection = False
licenseIsOsiApproved BSD_Source_Code = False
licenseIsOsiApproved BSL_1_0 = True
licenseIsOsiApproved Bzip2_1_0_5 = False
licenseIsOsiApproved Bzip2_1_0_6 = False
licenseIsOsiApproved Caldera = False
licenseIsOsiApproved CATOSL_1_1 = True
licenseIsOsiApproved CC_BY_1_0 = False
licenseIsOsiApproved CC_BY_2_0 = False
licenseIsOsiApproved CC_BY_2_5 = False
licenseIsOsiApproved CC_BY_3_0 = False
licenseIsOsiApproved CC_BY_4_0 = False
licenseIsOsiApproved CC_BY_NC_1_0 = False
licenseIsOsiApproved CC_BY_NC_2_0 = False
licenseIsOsiApproved CC_BY_NC_2_5 = False
licenseIsOsiApproved CC_BY_NC_3_0 = False
licenseIsOsiApproved CC_BY_NC_4_0 = False
licenseIsOsiApproved CC_BY_NC_ND_1_0 = False
licenseIsOsiApproved CC_BY_NC_ND_2_0 = False
licenseIsOsiApproved CC_BY_NC_ND_2_5 = False
licenseIsOsiApproved CC_BY_NC_ND_3_0 = False
licenseIsOsiApproved CC_BY_NC_ND_4_0 = False
licenseIsOsiApproved CC_BY_NC_SA_1_0 = False
licenseIsOsiApproved CC_BY_NC_SA_2_0 = False
licenseIsOsiApproved CC_BY_NC_SA_2_5 = False
licenseIsOsiApproved CC_BY_NC_SA_3_0 = False
licenseIsOsiApproved CC_BY_NC_SA_4_0 = False
licenseIsOsiApproved CC_BY_ND_1_0 = False
licenseIsOsiApproved CC_BY_ND_2_0 = False
licenseIsOsiApproved CC_BY_ND_2_5 = False
licenseIsOsiApproved CC_BY_ND_3_0 = False
licenseIsOsiApproved CC_BY_ND_4_0 = False
licenseIsOsiApproved CC_BY_SA_1_0 = False
licenseIsOsiApproved CC_BY_SA_2_0 = False
licenseIsOsiApproved CC_BY_SA_2_5 = False
licenseIsOsiApproved CC_BY_SA_3_0 = False
licenseIsOsiApproved CC_BY_SA_4_0 = False
licenseIsOsiApproved CC_PDDC = False
licenseIsOsiApproved CC0_1_0 = False
licenseIsOsiApproved CDDL_1_0 = True
licenseIsOsiApproved CDDL_1_1 = False
licenseIsOsiApproved CDLA_Permissive_1_0 = False
licenseIsOsiApproved CDLA_Sharing_1_0 = False
licenseIsOsiApproved CECILL_1_0 = False
licenseIsOsiApproved CECILL_1_1 = False
licenseIsOsiApproved CECILL_2_0 = False
licenseIsOsiApproved CECILL_2_1 = True
licenseIsOsiApproved CECILL_B = False
licenseIsOsiApproved CECILL_C = False
licenseIsOsiApproved CERN_OHL_1_1 = False
licenseIsOsiApproved CERN_OHL_1_2 = False
licenseIsOsiApproved ClArtistic = False
licenseIsOsiApproved CNRI_Jython = False
licenseIsOsiApproved CNRI_Python_GPL_Compatible = False
licenseIsOsiApproved CNRI_Python = True
licenseIsOsiApproved Condor_1_1 = False
licenseIsOsiApproved Copyleft_next_0_3_0 = False
licenseIsOsiApproved Copyleft_next_0_3_1 = False
licenseIsOsiApproved CPAL_1_0 = True
licenseIsOsiApproved CPL_1_0 = True
licenseIsOsiApproved CPOL_1_02 = False
licenseIsOsiApproved Crossword = False
licenseIsOsiApproved CrystalStacker = False
licenseIsOsiApproved CUA_OPL_1_0 = True
licenseIsOsiApproved Cube = False
licenseIsOsiApproved Curl = False
licenseIsOsiApproved D_FSL_1_0 = False
licenseIsOsiApproved Diffmark = False
licenseIsOsiApproved DOC = False
licenseIsOsiApproved Dotseqn = False
licenseIsOsiApproved DSDP = False
licenseIsOsiApproved Dvipdfm = False
licenseIsOsiApproved ECL_1_0 = True
licenseIsOsiApproved ECL_2_0 = True
licenseIsOsiApproved EFL_1_0 = True
licenseIsOsiApproved EFL_2_0 = True
licenseIsOsiApproved EGenix = False
licenseIsOsiApproved Entessa = True
licenseIsOsiApproved EPL_1_0 = True
licenseIsOsiApproved EPL_2_0 = True
licenseIsOsiApproved ErlPL_1_1 = False
licenseIsOsiApproved EUDatagrid = True
licenseIsOsiApproved EUPL_1_0 = False
licenseIsOsiApproved EUPL_1_1 = True
licenseIsOsiApproved EUPL_1_2 = True
licenseIsOsiApproved Eurosym = False
licenseIsOsiApproved Fair = True
licenseIsOsiApproved Frameworx_1_0 = True
licenseIsOsiApproved FreeImage = False
licenseIsOsiApproved FSFAP = False
licenseIsOsiApproved FSFULLR = False
licenseIsOsiApproved FSFUL = False
licenseIsOsiApproved FTL = False
licenseIsOsiApproved GFDL_1_1_only = False
licenseIsOsiApproved GFDL_1_1_or_later = False
licenseIsOsiApproved GFDL_1_2_only = False
licenseIsOsiApproved GFDL_1_2_or_later = False
licenseIsOsiApproved GFDL_1_3_only = False
licenseIsOsiApproved GFDL_1_3_or_later = False
licenseIsOsiApproved Giftware = False
licenseIsOsiApproved GL2PS = False
licenseIsOsiApproved Glide = False
licenseIsOsiApproved Glulxe = False
licenseIsOsiApproved Gnuplot = False
licenseIsOsiApproved GPL_1_0_only = False
licenseIsOsiApproved GPL_1_0_or_later = False
licenseIsOsiApproved GPL_2_0_only = True
licenseIsOsiApproved GPL_2_0_or_later = True
licenseIsOsiApproved GPL_3_0_only = True
licenseIsOsiApproved GPL_3_0_or_later = True
licenseIsOsiApproved GSOAP_1_3b = False
licenseIsOsiApproved HaskellReport = False
licenseIsOsiApproved HPND_sell_variant = False
licenseIsOsiApproved HPND = True
licenseIsOsiApproved IBM_pibs = False
licenseIsOsiApproved ICU = False
licenseIsOsiApproved IJG = False
licenseIsOsiApproved ImageMagick = False
licenseIsOsiApproved IMatix = False
licenseIsOsiApproved Imlib2 = False
licenseIsOsiApproved Info_ZIP = False
licenseIsOsiApproved Intel_ACPI = False
licenseIsOsiApproved Intel = True
licenseIsOsiApproved Interbase_1_0 = False
licenseIsOsiApproved IPA = True
licenseIsOsiApproved IPL_1_0 = True
licenseIsOsiApproved ISC = True
licenseIsOsiApproved JasPer_2_0 = False
licenseIsOsiApproved JPNIC = False
licenseIsOsiApproved JSON = False
licenseIsOsiApproved LAL_1_2 = False
licenseIsOsiApproved LAL_1_3 = False
licenseIsOsiApproved Latex2e = False
licenseIsOsiApproved Leptonica = False
licenseIsOsiApproved LGPL_2_0_only = True
licenseIsOsiApproved LGPL_2_0_or_later = True
licenseIsOsiApproved LGPL_2_1_only = True
licenseIsOsiApproved LGPL_2_1_or_later = True
licenseIsOsiApproved LGPL_3_0_only = True
licenseIsOsiApproved LGPL_3_0_or_later = True
licenseIsOsiApproved LGPLLR = False
licenseIsOsiApproved Libpng_2_0 = False
licenseIsOsiApproved Libpng = False
licenseIsOsiApproved Libtiff = False
licenseIsOsiApproved LiLiQ_P_1_1 = True
licenseIsOsiApproved LiLiQ_R_1_1 = True
licenseIsOsiApproved LiLiQ_Rplus_1_1 = True
licenseIsOsiApproved Linux_OpenIB = False
licenseIsOsiApproved LPL_1_02 = True
licenseIsOsiApproved LPL_1_0 = True
licenseIsOsiApproved LPPL_1_0 = False
licenseIsOsiApproved LPPL_1_1 = False
licenseIsOsiApproved LPPL_1_2 = False
licenseIsOsiApproved LPPL_1_3a = False
licenseIsOsiApproved LPPL_1_3c = True
licenseIsOsiApproved MakeIndex = False
licenseIsOsiApproved MirOS = True
licenseIsOsiApproved MIT_0 = True
licenseIsOsiApproved MIT_advertising = False
licenseIsOsiApproved MIT_CMU = False
licenseIsOsiApproved MIT_enna = False
licenseIsOsiApproved MIT_feh = False
licenseIsOsiApproved MITNFA = False
licenseIsOsiApproved MIT = True
licenseIsOsiApproved Motosoto = True
licenseIsOsiApproved Mpich2 = False
licenseIsOsiApproved MPL_1_0 = True
licenseIsOsiApproved MPL_1_1 = True
licenseIsOsiApproved MPL_2_0_no_copyleft_exception = True
licenseIsOsiApproved MPL_2_0 = True
licenseIsOsiApproved MS_PL = True
licenseIsOsiApproved MS_RL = True
licenseIsOsiApproved MTLL = False
licenseIsOsiApproved Multics = True
licenseIsOsiApproved Mup = False
licenseIsOsiApproved NASA_1_3 = True
licenseIsOsiApproved Naumen = True
licenseIsOsiApproved NBPL_1_0 = False
licenseIsOsiApproved NCSA = True
licenseIsOsiApproved Net_SNMP = False
licenseIsOsiApproved NetCDF = False
licenseIsOsiApproved Newsletr = False
licenseIsOsiApproved NGPL = True
licenseIsOsiApproved NLOD_1_0 = False
licenseIsOsiApproved NLPL = False
licenseIsOsiApproved Nokia = True
licenseIsOsiApproved NOSL = False
licenseIsOsiApproved Noweb = False
licenseIsOsiApproved NPL_1_0 = False
licenseIsOsiApproved NPL_1_1 = False
licenseIsOsiApproved NPOSL_3_0 = True
licenseIsOsiApproved NRL = False
licenseIsOsiApproved NTP = True
licenseIsOsiApproved OCCT_PL = False
licenseIsOsiApproved OCLC_2_0 = True
licenseIsOsiApproved ODbL_1_0 = False
licenseIsOsiApproved ODC_By_1_0 = False
licenseIsOsiApproved OFL_1_0 = False
licenseIsOsiApproved OFL_1_1 = True
licenseIsOsiApproved OGL_UK_1_0 = False
licenseIsOsiApproved OGL_UK_2_0 = False
licenseIsOsiApproved OGL_UK_3_0 = False
licenseIsOsiApproved OGTSL = True
licenseIsOsiApproved OLDAP_1_1 = False
licenseIsOsiApproved OLDAP_1_2 = False
licenseIsOsiApproved OLDAP_1_3 = False
licenseIsOsiApproved OLDAP_1_4 = False
licenseIsOsiApproved OLDAP_2_0_1 = False
licenseIsOsiApproved OLDAP_2_0 = False
licenseIsOsiApproved OLDAP_2_1 = False
licenseIsOsiApproved OLDAP_2_2_1 = False
licenseIsOsiApproved OLDAP_2_2_2 = False
licenseIsOsiApproved OLDAP_2_2 = False
licenseIsOsiApproved OLDAP_2_3 = False
licenseIsOsiApproved OLDAP_2_4 = False
licenseIsOsiApproved OLDAP_2_5 = False
licenseIsOsiApproved OLDAP_2_6 = False
licenseIsOsiApproved OLDAP_2_7 = False
licenseIsOsiApproved OLDAP_2_8 = False
licenseIsOsiApproved OML = False
licenseIsOsiApproved OpenSSL = False
licenseIsOsiApproved OPL_1_0 = False
licenseIsOsiApproved OSET_PL_2_1 = True
licenseIsOsiApproved OSL_1_0 = True
licenseIsOsiApproved OSL_1_1 = False
licenseIsOsiApproved OSL_2_0 = True
licenseIsOsiApproved OSL_2_1 = True
licenseIsOsiApproved OSL_3_0 = True
licenseIsOsiApproved Parity_6_0_0 = False
licenseIsOsiApproved PDDL_1_0 = False
licenseIsOsiApproved PHP_3_01 = False
licenseIsOsiApproved PHP_3_0 = True
licenseIsOsiApproved Plexus = False
licenseIsOsiApproved PostgreSQL = True
licenseIsOsiApproved Psfrag = False
licenseIsOsiApproved Psutils = False
licenseIsOsiApproved Python_2_0 = True
licenseIsOsiApproved Qhull = False
licenseIsOsiApproved QPL_1_0 = True
licenseIsOsiApproved Rdisc = False
licenseIsOsiApproved RHeCos_1_1 = False
licenseIsOsiApproved RPL_1_1 = True
licenseIsOsiApproved RPL_1_5 = True
licenseIsOsiApproved RPSL_1_0 = True
licenseIsOsiApproved RSA_MD = False
licenseIsOsiApproved RSCPL = True
licenseIsOsiApproved Ruby = False
licenseIsOsiApproved SAX_PD = False
licenseIsOsiApproved Saxpath = False
licenseIsOsiApproved SCEA = False
licenseIsOsiApproved Sendmail_8_23 = False
licenseIsOsiApproved Sendmail = False
licenseIsOsiApproved SGI_B_1_0 = False
licenseIsOsiApproved SGI_B_1_1 = False
licenseIsOsiApproved SGI_B_2_0 = False
licenseIsOsiApproved SHL_0_51 = False
licenseIsOsiApproved SHL_0_5 = False
licenseIsOsiApproved SimPL_2_0 = True
licenseIsOsiApproved SISSL_1_2 = False
licenseIsOsiApproved SISSL = True
licenseIsOsiApproved Sleepycat = True
licenseIsOsiApproved SMLNJ = False
licenseIsOsiApproved SMPPL = False
licenseIsOsiApproved SNIA = False
licenseIsOsiApproved Spencer_86 = False
licenseIsOsiApproved Spencer_94 = False
licenseIsOsiApproved Spencer_99 = False
licenseIsOsiApproved SPL_1_0 = True
licenseIsOsiApproved SSPL_1_0 = False
licenseIsOsiApproved SugarCRM_1_1_3 = False
licenseIsOsiApproved SWL = False
licenseIsOsiApproved TAPR_OHL_1_0 = False
licenseIsOsiApproved TCL = False
licenseIsOsiApproved TCP_wrappers = False
licenseIsOsiApproved TMate = False
licenseIsOsiApproved TORQUE_1_1 = False
licenseIsOsiApproved TOSL = False
licenseIsOsiApproved TU_Berlin_1_0 = False
licenseIsOsiApproved TU_Berlin_2_0 = False
licenseIsOsiApproved Unicode_DFS_2015 = False
licenseIsOsiApproved Unicode_DFS_2016 = False
licenseIsOsiApproved Unicode_TOU = False
licenseIsOsiApproved Unlicense = False
licenseIsOsiApproved UPL_1_0 = True
licenseIsOsiApproved Vim = False
licenseIsOsiApproved VOSTROM = False
licenseIsOsiApproved VSL_1_0 = True
licenseIsOsiApproved W3C_19980720 = False
licenseIsOsiApproved W3C_20150513 = False
licenseIsOsiApproved W3C = True
licenseIsOsiApproved Watcom_1_0 = True
licenseIsOsiApproved Wsuipa = False
licenseIsOsiApproved WTFPL = False
licenseIsOsiApproved X11 = False
licenseIsOsiApproved Xerox = False
licenseIsOsiApproved XFree86_1_1 = False
licenseIsOsiApproved Xinetd = False
licenseIsOsiApproved Xnet = True
licenseIsOsiApproved Xpp = False
licenseIsOsiApproved XSkat = False
licenseIsOsiApproved YPL_1_0 = False
licenseIsOsiApproved YPL_1_1 = False
licenseIsOsiApproved Zed = False
licenseIsOsiApproved Zend_2_0 = False
licenseIsOsiApproved Zimbra_1_3 = False
licenseIsOsiApproved Zimbra_1_4 = False
licenseIsOsiApproved Zlib_acknowledgement = False
licenseIsOsiApproved Zlib = True
licenseIsOsiApproved ZPL_1_1 = False
licenseIsOsiApproved ZPL_2_0 = True
licenseIsOsiApproved ZPL_2_1 = False

-------------------------------------------------------------------------------
-- Creation
-------------------------------------------------------------------------------

licenseIdList :: LicenseListVersion -> [LicenseId]
licenseIdList LicenseListVersion_3_0 =
    [ AGPL_1_0
    ]
    ++ bulkOfLicenses
licenseIdList LicenseListVersion_3_2 =
    [ AGPL_1_0_only
    , AGPL_1_0_or_later
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
    , BSD_3_Clause_Open_MPI
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

-- | Create a 'LicenseId' from a 'String'.
mkLicenseId :: LicenseListVersion -> String -> Maybe LicenseId
mkLicenseId LicenseListVersion_3_0 s = Map.lookup s stringLookup_3_0
mkLicenseId LicenseListVersion_3_2 s = Map.lookup s stringLookup_3_2
mkLicenseId LicenseListVersion_3_6 s = Map.lookup s stringLookup_3_6

stringLookup_3_0 :: Map String LicenseId
stringLookup_3_0 = Map.fromList $ map (\i -> (licenseId i, i)) $
    licenseIdList LicenseListVersion_3_0

stringLookup_3_2 :: Map String LicenseId
stringLookup_3_2 = Map.fromList $ map (\i -> (licenseId i, i)) $
    licenseIdList LicenseListVersion_3_2

stringLookup_3_6 :: Map String LicenseId
stringLookup_3_6 = Map.fromList $ map (\i -> (licenseId i, i)) $
    licenseIdList LicenseListVersion_3_6

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
    , BSD_2_Clause_FreeBSD
    , BSD_2_Clause_NetBSD
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
    , Bzip2_1_0_5
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
