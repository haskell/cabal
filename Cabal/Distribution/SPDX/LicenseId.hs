-- This file is generated. See Makefile's spdx rule
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
module Distribution.SPDX.LicenseId (
    LicenseId (..),
    licenseId,
    licenseName,
    licenseIsOsiApproved,
    mkLicenseId,
    -- * Helpers
    licenseIdMigrationMessage,
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
-- LicenseId
-------------------------------------------------------------------------------

-- | SPDX License identifier
data LicenseId
    = Glide -- ^ @Glide@, 3dfx Glide License
    | Abstyles -- ^ @Abstyles@, Abstyles License
    | AFL_1_1 -- ^ @AFL-1.1@, Academic Free License v1.1
    | AFL_1_2 -- ^ @AFL-1.2@, Academic Free License v1.2
    | AFL_2_0 -- ^ @AFL-2.0@, Academic Free License v2.0
    | AFL_2_1 -- ^ @AFL-2.1@, Academic Free License v2.1
    | AFL_3_0 -- ^ @AFL-3.0@, Academic Free License v3.0
    | AMPAS -- ^ @AMPAS@, Academy of Motion Picture Arts and Sciences BSD
    | APL_1_0 -- ^ @APL-1.0@, Adaptive Public License 1.0
    | Adobe_Glyph -- ^ @Adobe-Glyph@, Adobe Glyph List License
    | APAFML -- ^ @APAFML@, Adobe Postscript AFM License
    | Adobe_2006 -- ^ @Adobe-2006@, Adobe Systems Incorporated Source Code License Agreement
    | AGPL_1_0 -- ^ @AGPL-1.0@, Affero General Public License v1.0
    | Afmparse -- ^ @Afmparse@, Afmparse License
    | Aladdin -- ^ @Aladdin@, Aladdin Free Public License
    | ADSL -- ^ @ADSL@, Amazon Digital Services License
    | AMDPLPA -- ^ @AMDPLPA@, AMD's plpa_map.c License
    | ANTLR_PD -- ^ @ANTLR-PD@, ANTLR Software Rights Notice
    | Apache_1_0 -- ^ @Apache-1.0@, Apache License 1.0
    | Apache_1_1 -- ^ @Apache-1.1@, Apache License 1.1
    | Apache_2_0 -- ^ @Apache-2.0@, Apache License 2.0
    | AML -- ^ @AML@, Apple MIT License
    | APSL_1_0 -- ^ @APSL-1.0@, Apple Public Source License 1.0
    | APSL_1_1 -- ^ @APSL-1.1@, Apple Public Source License 1.1
    | APSL_1_2 -- ^ @APSL-1.2@, Apple Public Source License 1.2
    | APSL_2_0 -- ^ @APSL-2.0@, Apple Public Source License 2.0
    | Artistic_1_0 -- ^ @Artistic-1.0@, Artistic License 1.0
    | Artistic_1_0_Perl -- ^ @Artistic-1.0-Perl@, Artistic License 1.0 (Perl)
    | Artistic_1_0_cl8 -- ^ @Artistic-1.0-cl8@, Artistic License 1.0 w/clause 8
    | Artistic_2_0 -- ^ @Artistic-2.0@, Artistic License 2.0
    | AAL -- ^ @AAL@, Attribution Assurance License
    | Bahyph -- ^ @Bahyph@, Bahyph License
    | Barr -- ^ @Barr@, Barr License
    | Beerware -- ^ @Beerware@, Beerware License
    | BitTorrent_1_0 -- ^ @BitTorrent-1.0@, BitTorrent Open Source License v1.0
    | BitTorrent_1_1 -- ^ @BitTorrent-1.1@, BitTorrent Open Source License v1.1
    | BSL_1_0 -- ^ @BSL-1.0@, Boost Software License 1.0
    | Borceux -- ^ @Borceux@, Borceux license
    | BSD_2_Clause -- ^ @BSD-2-Clause@, BSD 2-clause "Simplified" License
    | BSD_2_Clause_FreeBSD -- ^ @BSD-2-Clause-FreeBSD@, BSD 2-clause FreeBSD License
    | BSD_2_Clause_NetBSD -- ^ @BSD-2-Clause-NetBSD@, BSD 2-clause NetBSD License
    | BSD_3_Clause -- ^ @BSD-3-Clause@, BSD 3-clause "New" or "Revised" License
    | BSD_3_Clause_Clear -- ^ @BSD-3-Clause-Clear@, BSD 3-clause Clear License
    | BSD_3_Clause_No_Nuclear_License -- ^ @BSD-3-Clause-No-Nuclear-License@, BSD 3-Clause No Nuclear License
    | BSD_3_Clause_No_Nuclear_License_2014 -- ^ @BSD-3-Clause-No-Nuclear-License-2014@, BSD 3-Clause No Nuclear License 2014
    | BSD_3_Clause_No_Nuclear_Warranty -- ^ @BSD-3-Clause-No-Nuclear-Warranty@, BSD 3-Clause No Nuclear Warranty
    | BSD_4_Clause -- ^ @BSD-4-Clause@, BSD 4-clause "Original" or "Old" License
    | BSD_Protection -- ^ @BSD-Protection@, BSD Protection License
    | BSD_Source_Code -- ^ @BSD-Source-Code@, BSD Source Code Attribution
    | BSD_3_Clause_Attribution -- ^ @BSD-3-Clause-Attribution@, BSD with attribution
    | NullBSD -- ^ @0BSD@, BSD Zero Clause License
    | BSD_4_Clause_UC -- ^ @BSD-4-Clause-UC@, BSD-4-Clause (University of California-Specific)
    | Bzip2_1_0_5 -- ^ @bzip2-1.0.5@, bzip2 and libbzip2 License v1.0.5
    | Bzip2_1_0_6 -- ^ @bzip2-1.0.6@, bzip2 and libbzip2 License v1.0.6
    | Caldera -- ^ @Caldera@, Caldera License
    | CECILL_1_0 -- ^ @CECILL-1.0@, CeCILL Free Software License Agreement v1.0
    | CECILL_1_1 -- ^ @CECILL-1.1@, CeCILL Free Software License Agreement v1.1
    | CECILL_2_0 -- ^ @CECILL-2.0@, CeCILL Free Software License Agreement v2.0
    | CECILL_2_1 -- ^ @CECILL-2.1@, CeCILL Free Software License Agreement v2.1
    | CECILL_B -- ^ @CECILL-B@, CeCILL-B Free Software License Agreement
    | CECILL_C -- ^ @CECILL-C@, CeCILL-C Free Software License Agreement
    | ClArtistic -- ^ @ClArtistic@, Clarified Artistic License
    | MIT_CMU -- ^ @MIT-CMU@, CMU License
    | CNRI_Jython -- ^ @CNRI-Jython@, CNRI Jython License
    | CNRI_Python -- ^ @CNRI-Python@, CNRI Python License
    | CNRI_Python_GPL_Compatible -- ^ @CNRI-Python-GPL-Compatible@, CNRI Python Open Source GPL Compatible License Agreement
    | CPOL_1_02 -- ^ @CPOL-1.02@, Code Project Open License 1.02
    | CDDL_1_0 -- ^ @CDDL-1.0@, Common Development and Distribution License 1.0
    | CDDL_1_1 -- ^ @CDDL-1.1@, Common Development and Distribution License 1.1
    | CPAL_1_0 -- ^ @CPAL-1.0@, Common Public Attribution License 1.0
    | CPL_1_0 -- ^ @CPL-1.0@, Common Public License 1.0
    | CATOSL_1_1 -- ^ @CATOSL-1.1@, Computer Associates Trusted Open Source License 1.1
    | Condor_1_1 -- ^ @Condor-1.1@, Condor Public License v1.1
    | CC_BY_1_0 -- ^ @CC-BY-1.0@, Creative Commons Attribution 1.0
    | CC_BY_2_0 -- ^ @CC-BY-2.0@, Creative Commons Attribution 2.0
    | CC_BY_2_5 -- ^ @CC-BY-2.5@, Creative Commons Attribution 2.5
    | CC_BY_3_0 -- ^ @CC-BY-3.0@, Creative Commons Attribution 3.0
    | CC_BY_4_0 -- ^ @CC-BY-4.0@, Creative Commons Attribution 4.0
    | CC_BY_ND_1_0 -- ^ @CC-BY-ND-1.0@, Creative Commons Attribution No Derivatives 1.0
    | CC_BY_ND_2_0 -- ^ @CC-BY-ND-2.0@, Creative Commons Attribution No Derivatives 2.0
    | CC_BY_ND_2_5 -- ^ @CC-BY-ND-2.5@, Creative Commons Attribution No Derivatives 2.5
    | CC_BY_ND_3_0 -- ^ @CC-BY-ND-3.0@, Creative Commons Attribution No Derivatives 3.0
    | CC_BY_ND_4_0 -- ^ @CC-BY-ND-4.0@, Creative Commons Attribution No Derivatives 4.0
    | CC_BY_NC_1_0 -- ^ @CC-BY-NC-1.0@, Creative Commons Attribution Non Commercial 1.0
    | CC_BY_NC_2_0 -- ^ @CC-BY-NC-2.0@, Creative Commons Attribution Non Commercial 2.0
    | CC_BY_NC_2_5 -- ^ @CC-BY-NC-2.5@, Creative Commons Attribution Non Commercial 2.5
    | CC_BY_NC_3_0 -- ^ @CC-BY-NC-3.0@, Creative Commons Attribution Non Commercial 3.0
    | CC_BY_NC_4_0 -- ^ @CC-BY-NC-4.0@, Creative Commons Attribution Non Commercial 4.0
    | CC_BY_NC_ND_1_0 -- ^ @CC-BY-NC-ND-1.0@, Creative Commons Attribution Non Commercial No Derivatives 1.0
    | CC_BY_NC_ND_2_0 -- ^ @CC-BY-NC-ND-2.0@, Creative Commons Attribution Non Commercial No Derivatives 2.0
    | CC_BY_NC_ND_2_5 -- ^ @CC-BY-NC-ND-2.5@, Creative Commons Attribution Non Commercial No Derivatives 2.5
    | CC_BY_NC_ND_3_0 -- ^ @CC-BY-NC-ND-3.0@, Creative Commons Attribution Non Commercial No Derivatives 3.0
    | CC_BY_NC_ND_4_0 -- ^ @CC-BY-NC-ND-4.0@, Creative Commons Attribution Non Commercial No Derivatives 4.0
    | CC_BY_NC_SA_1_0 -- ^ @CC-BY-NC-SA-1.0@, Creative Commons Attribution Non Commercial Share Alike 1.0
    | CC_BY_NC_SA_2_0 -- ^ @CC-BY-NC-SA-2.0@, Creative Commons Attribution Non Commercial Share Alike 2.0
    | CC_BY_NC_SA_2_5 -- ^ @CC-BY-NC-SA-2.5@, Creative Commons Attribution Non Commercial Share Alike 2.5
    | CC_BY_NC_SA_3_0 -- ^ @CC-BY-NC-SA-3.0@, Creative Commons Attribution Non Commercial Share Alike 3.0
    | CC_BY_NC_SA_4_0 -- ^ @CC-BY-NC-SA-4.0@, Creative Commons Attribution Non Commercial Share Alike 4.0
    | CC_BY_SA_1_0 -- ^ @CC-BY-SA-1.0@, Creative Commons Attribution Share Alike 1.0
    | CC_BY_SA_2_0 -- ^ @CC-BY-SA-2.0@, Creative Commons Attribution Share Alike 2.0
    | CC_BY_SA_2_5 -- ^ @CC-BY-SA-2.5@, Creative Commons Attribution Share Alike 2.5
    | CC_BY_SA_3_0 -- ^ @CC-BY-SA-3.0@, Creative Commons Attribution Share Alike 3.0
    | CC_BY_SA_4_0 -- ^ @CC-BY-SA-4.0@, Creative Commons Attribution Share Alike 4.0
    | CC0_1_0 -- ^ @CC0-1.0@, Creative Commons Zero v1.0 Universal
    | Crossword -- ^ @Crossword@, Crossword License
    | CrystalStacker -- ^ @CrystalStacker@, CrystalStacker License
    | CUA_OPL_1_0 -- ^ @CUA-OPL-1.0@, CUA Office Public License v1.0
    | Cube -- ^ @Cube@, Cube License
    | Curl -- ^ @curl@, curl License
    | D_FSL_1_0 -- ^ @D-FSL-1.0@, Deutsche Freie Software Lizenz
    | Diffmark -- ^ @diffmark@, diffmark license
    | WTFPL -- ^ @WTFPL@, Do What The F*ck You Want To Public License
    | DOC -- ^ @DOC@, DOC License
    | Dotseqn -- ^ @Dotseqn@, Dotseqn License
    | DSDP -- ^ @DSDP@, DSDP License
    | Dvipdfm -- ^ @dvipdfm@, dvipdfm License
    | EPL_1_0 -- ^ @EPL-1.0@, Eclipse Public License 1.0
    | ECL_1_0 -- ^ @ECL-1.0@, Educational Community License v1.0
    | ECL_2_0 -- ^ @ECL-2.0@, Educational Community License v2.0
    | EGenix -- ^ @eGenix@, eGenix.com Public License 1.1.0
    | EFL_1_0 -- ^ @EFL-1.0@, Eiffel Forum License v1.0
    | EFL_2_0 -- ^ @EFL-2.0@, Eiffel Forum License v2.0
    | MIT_advertising -- ^ @MIT-advertising@, Enlightenment License (e16)
    | MIT_enna -- ^ @MIT-enna@, enna License
    | Entessa -- ^ @Entessa@, Entessa Public License v1.0
    | ErlPL_1_1 -- ^ @ErlPL-1.1@, Erlang Public License v1.1
    | EUDatagrid -- ^ @EUDatagrid@, EU DataGrid Software License
    | EUPL_1_0 -- ^ @EUPL-1.0@, European Union Public License 1.0
    | EUPL_1_1 -- ^ @EUPL-1.1@, European Union Public License 1.1
    | Eurosym -- ^ @Eurosym@, Eurosym License
    | Fair -- ^ @Fair@, Fair License
    | MIT_feh -- ^ @MIT-feh@, feh License
    | Frameworx_1_0 -- ^ @Frameworx-1.0@, Frameworx Open License 1.0
    | FreeImage -- ^ @FreeImage@, FreeImage Public License v1.0
    | FTL -- ^ @FTL@, Freetype Project License
    | FSFAP -- ^ @FSFAP@, FSF All Permissive License
    | FSFUL -- ^ @FSFUL@, FSF Unlimited License
    | FSFULLR -- ^ @FSFULLR@, FSF Unlimited License (with License Retention)
    | Giftware -- ^ @Giftware@, Giftware License
    | GL2PS -- ^ @GL2PS@, GL2PS License
    | Glulxe -- ^ @Glulxe@, Glulxe License
    | AGPL_3_0 -- ^ @AGPL-3.0@, GNU Affero General Public License v3.0
    | GFDL_1_1 -- ^ @GFDL-1.1@, GNU Free Documentation License v1.1
    | GFDL_1_2 -- ^ @GFDL-1.2@, GNU Free Documentation License v1.2
    | GFDL_1_3 -- ^ @GFDL-1.3@, GNU Free Documentation License v1.3
    | GPL_1_0 -- ^ @GPL-1.0@, GNU General Public License v1.0 only
    | GPL_2_0 -- ^ @GPL-2.0@, GNU General Public License v2.0 only
    | GPL_3_0 -- ^ @GPL-3.0@, GNU General Public License v3.0 only
    | LGPL_2_1 -- ^ @LGPL-2.1@, GNU Lesser General Public License v2.1 only
    | LGPL_3_0 -- ^ @LGPL-3.0@, GNU Lesser General Public License v3.0 only
    | LGPL_2_0 -- ^ @LGPL-2.0@, GNU Library General Public License v2 only
    | Gnuplot -- ^ @gnuplot@, gnuplot License
    | GSOAP_1_3b -- ^ @gSOAP-1.3b@, gSOAP Public License v1.3b
    | HaskellReport -- ^ @HaskellReport@, Haskell Language Report License
    | HPND -- ^ @HPND@, Historic Permission Notice and Disclaimer
    | IBM_pibs -- ^ @IBM-pibs@, IBM PowerPC Initialization and Boot Software
    | IPL_1_0 -- ^ @IPL-1.0@, IBM Public License v1.0
    | ICU -- ^ @ICU@, ICU License
    | ImageMagick -- ^ @ImageMagick@, ImageMagick License
    | IMatix -- ^ @iMatix@, iMatix Standard Function Library Agreement
    | Imlib2 -- ^ @Imlib2@, Imlib2 License
    | IJG -- ^ @IJG@, Independent JPEG Group License
    | Info_ZIP -- ^ @Info-ZIP@, Info-ZIP License
    | Intel_ACPI -- ^ @Intel-ACPI@, Intel ACPI Software License Agreement
    | Intel -- ^ @Intel@, Intel Open Source License
    | Interbase_1_0 -- ^ @Interbase-1.0@, Interbase Public License v1.0
    | IPA -- ^ @IPA@, IPA Font License
    | ISC -- ^ @ISC@, ISC License
    | JasPer_2_0 -- ^ @JasPer-2.0@, JasPer License
    | JSON -- ^ @JSON@, JSON License
    | LPPL_1_0 -- ^ @LPPL-1.0@, LaTeX Project Public License v1.0
    | LPPL_1_1 -- ^ @LPPL-1.1@, LaTeX Project Public License v1.1
    | LPPL_1_2 -- ^ @LPPL-1.2@, LaTeX Project Public License v1.2
    | LPPL_1_3a -- ^ @LPPL-1.3a@, LaTeX Project Public License v1.3a
    | LPPL_1_3c -- ^ @LPPL-1.3c@, LaTeX Project Public License v1.3c
    | Latex2e -- ^ @Latex2e@, Latex2e License
    | BSD_3_Clause_LBNL -- ^ @BSD-3-Clause-LBNL@, Lawrence Berkeley National Labs BSD variant license
    | Leptonica -- ^ @Leptonica@, Leptonica License
    | LGPLLR -- ^ @LGPLLR@, Lesser General Public License For Linguistic Resources
    | Libpng -- ^ @Libpng@, libpng License
    | Libtiff -- ^ @libtiff@, libtiff License
    | LAL_1_2 -- ^ @LAL-1.2@, Licence Art Libre 1.2
    | LAL_1_3 -- ^ @LAL-1.3@, Licence Art Libre 1.3
    | LiLiQ_P_1_1 -- ^ @LiLiQ-P-1.1@, Licence Libre du Québec – Permissive version 1.1
    | LiLiQ_Rplus_1_1 -- ^ @LiLiQ-Rplus-1.1@, Licence Libre du Québec – Réciprocité forte version 1.1
    | LiLiQ_R_1_1 -- ^ @LiLiQ-R-1.1@, Licence Libre du Québec – Réciprocité version 1.1
    | LPL_1_02 -- ^ @LPL-1.02@, Lucent Public License v1.02
    | LPL_1_0 -- ^ @LPL-1.0@, Lucent Public License Version 1.0
    | MakeIndex -- ^ @MakeIndex@, MakeIndex License
    | MTLL -- ^ @MTLL@, Matrix Template Library License
    | MS_PL -- ^ @MS-PL@, Microsoft Public License
    | MS_RL -- ^ @MS-RL@, Microsoft Reciprocal License
    | MirOS -- ^ @MirOS@, MirOS Licence
    | MITNFA -- ^ @MITNFA@, MIT +no-false-attribs license
    | MIT -- ^ @MIT@, MIT License
    | Motosoto -- ^ @Motosoto@, Motosoto License
    | MPL_1_0 -- ^ @MPL-1.0@, Mozilla Public License 1.0
    | MPL_1_1 -- ^ @MPL-1.1@, Mozilla Public License 1.1
    | MPL_2_0 -- ^ @MPL-2.0@, Mozilla Public License 2.0
    | MPL_2_0_no_copyleft_exception -- ^ @MPL-2.0-no-copyleft-exception@, Mozilla Public License 2.0 (no copyleft exception)
    | Mpich2 -- ^ @mpich2@, mpich2 License
    | Multics -- ^ @Multics@, Multics License
    | Mup -- ^ @Mup@, Mup License
    | NASA_1_3 -- ^ @NASA-1.3@, NASA Open Source Agreement 1.3
    | Naumen -- ^ @Naumen@, Naumen Public License
    | NBPL_1_0 -- ^ @NBPL-1.0@, Net Boolean Public License v1
    | Net_SNMP -- ^ @Net-SNMP@, Net-SNMP License
    | NetCDF -- ^ @NetCDF@, NetCDF license
    | NGPL -- ^ @NGPL@, Nethack General Public License
    | NOSL -- ^ @NOSL@, Netizen Open Source License
    | NPL_1_0 -- ^ @NPL-1.0@, Netscape Public License v1.0
    | NPL_1_1 -- ^ @NPL-1.1@, Netscape Public License v1.1
    | Newsletr -- ^ @Newsletr@, Newsletr License
    | NLPL -- ^ @NLPL@, No Limit Public License
    | Nokia -- ^ @Nokia@, Nokia Open Source License
    | NPOSL_3_0 -- ^ @NPOSL-3.0@, Non-Profit Open Software License 3.0
    | NLOD_1_0 -- ^ @NLOD-1.0@, Norwegian Licence for Open Government Data
    | Noweb -- ^ @Noweb@, Noweb License
    | NRL -- ^ @NRL@, NRL License
    | NTP -- ^ @NTP@, NTP License
    | Nunit -- ^ @Nunit@, Nunit License
    | OCLC_2_0 -- ^ @OCLC-2.0@, OCLC Research Public License 2.0
    | ODbL_1_0 -- ^ @ODbL-1.0@, ODC Open Database License v1.0
    | PDDL_1_0 -- ^ @PDDL-1.0@, ODC Public Domain Dedication & License 1.0
    | OCCT_PL -- ^ @OCCT-PL@, Open CASCADE Technology Public License
    | OGTSL -- ^ @OGTSL@, Open Group Test Suite License
    | OLDAP_2_2_2 -- ^ @OLDAP-2.2.2@, Open LDAP Public License  2.2.2
    | OLDAP_1_1 -- ^ @OLDAP-1.1@, Open LDAP Public License v1.1
    | OLDAP_1_2 -- ^ @OLDAP-1.2@, Open LDAP Public License v1.2
    | OLDAP_1_3 -- ^ @OLDAP-1.3@, Open LDAP Public License v1.3
    | OLDAP_1_4 -- ^ @OLDAP-1.4@, Open LDAP Public License v1.4
    | OLDAP_2_0 -- ^ @OLDAP-2.0@, Open LDAP Public License v2.0 (or possibly 2.0A and 2.0B)
    | OLDAP_2_0_1 -- ^ @OLDAP-2.0.1@, Open LDAP Public License v2.0.1
    | OLDAP_2_1 -- ^ @OLDAP-2.1@, Open LDAP Public License v2.1
    | OLDAP_2_2 -- ^ @OLDAP-2.2@, Open LDAP Public License v2.2
    | OLDAP_2_2_1 -- ^ @OLDAP-2.2.1@, Open LDAP Public License v2.2.1
    | OLDAP_2_3 -- ^ @OLDAP-2.3@, Open LDAP Public License v2.3
    | OLDAP_2_4 -- ^ @OLDAP-2.4@, Open LDAP Public License v2.4
    | OLDAP_2_5 -- ^ @OLDAP-2.5@, Open LDAP Public License v2.5
    | OLDAP_2_6 -- ^ @OLDAP-2.6@, Open LDAP Public License v2.6
    | OLDAP_2_7 -- ^ @OLDAP-2.7@, Open LDAP Public License v2.7
    | OLDAP_2_8 -- ^ @OLDAP-2.8@, Open LDAP Public License v2.8
    | OML -- ^ @OML@, Open Market License
    | OPL_1_0 -- ^ @OPL-1.0@, Open Public License v1.0
    | OSL_1_0 -- ^ @OSL-1.0@, Open Software License 1.0
    | OSL_1_1 -- ^ @OSL-1.1@, Open Software License 1.1
    | OSL_2_0 -- ^ @OSL-2.0@, Open Software License 2.0
    | OSL_2_1 -- ^ @OSL-2.1@, Open Software License 2.1
    | OSL_3_0 -- ^ @OSL-3.0@, Open Software License 3.0
    | OpenSSL -- ^ @OpenSSL@, OpenSSL License
    | OSET_PL_2_1 -- ^ @OSET-PL-2.1@, OSET Public License version 2.1
    | PHP_3_0 -- ^ @PHP-3.0@, PHP License v3.0
    | PHP_3_01 -- ^ @PHP-3.01@, PHP License v3.01
    | Plexus -- ^ @Plexus@, Plexus Classworlds License
    | PostgreSQL -- ^ @PostgreSQL@, PostgreSQL License
    | Psfrag -- ^ @psfrag@, psfrag License
    | Psutils -- ^ @psutils@, psutils License
    | Python_2_0 -- ^ @Python-2.0@, Python License 2.0
    | QPL_1_0 -- ^ @QPL-1.0@, Q Public License 1.0
    | Qhull -- ^ @Qhull@, Qhull License
    | Rdisc -- ^ @Rdisc@, Rdisc License
    | RPSL_1_0 -- ^ @RPSL-1.0@, RealNetworks Public Source License v1.0
    | RPL_1_1 -- ^ @RPL-1.1@, Reciprocal Public License 1.1
    | RPL_1_5 -- ^ @RPL-1.5@, Reciprocal Public License 1.5
    | RHeCos_1_1 -- ^ @RHeCos-1.1@, Red Hat eCos Public License v1.1
    | RSCPL -- ^ @RSCPL@, Ricoh Source Code Public License
    | RSA_MD -- ^ @RSA-MD@, RSA Message-Digest License 
    | Ruby -- ^ @Ruby@, Ruby License
    | SAX_PD -- ^ @SAX-PD@, Sax Public Domain Notice
    | Saxpath -- ^ @Saxpath@, Saxpath License
    | SCEA -- ^ @SCEA@, SCEA Shared Source License
    | SWL -- ^ @SWL@, Scheme Widget Library (SWL) Software License Agreement
    | SMPPL -- ^ @SMPPL@, Secure Messaging Protocol Public License
    | Sendmail -- ^ @Sendmail@, Sendmail License
    | SGI_B_1_0 -- ^ @SGI-B-1.0@, SGI Free Software License B v1.0
    | SGI_B_1_1 -- ^ @SGI-B-1.1@, SGI Free Software License B v1.1
    | SGI_B_2_0 -- ^ @SGI-B-2.0@, SGI Free Software License B v2.0
    | OFL_1_0 -- ^ @OFL-1.0@, SIL Open Font License 1.0
    | OFL_1_1 -- ^ @OFL-1.1@, SIL Open Font License 1.1
    | SimPL_2_0 -- ^ @SimPL-2.0@, Simple Public License 2.0
    | Sleepycat -- ^ @Sleepycat@, Sleepycat License
    | SNIA -- ^ @SNIA@, SNIA Public License 1.1
    | Spencer_86 -- ^ @Spencer-86@, Spencer License 86
    | Spencer_94 -- ^ @Spencer-94@, Spencer License 94
    | Spencer_99 -- ^ @Spencer-99@, Spencer License 99
    | SMLNJ -- ^ @SMLNJ@, Standard ML of New Jersey License
    | SugarCRM_1_1_3 -- ^ @SugarCRM-1.1.3@, SugarCRM Public License v1.1.3
    | SISSL -- ^ @SISSL@, Sun Industry Standards Source License v1.1
    | SISSL_1_2 -- ^ @SISSL-1.2@, Sun Industry Standards Source License v1.2
    | SPL_1_0 -- ^ @SPL-1.0@, Sun Public License v1.0
    | Watcom_1_0 -- ^ @Watcom-1.0@, Sybase Open Watcom Public License 1.0
    | TCL -- ^ @TCL@, TCL/TK License
    | TCP_wrappers -- ^ @TCP-wrappers@, TCP Wrappers License
    | Unlicense -- ^ @Unlicense@, The Unlicense
    | TMate -- ^ @TMate@, TMate Open Source License
    | TORQUE_1_1 -- ^ @TORQUE-1.1@, TORQUE v2.5+ Software License v1.1
    | TOSL -- ^ @TOSL@, Trusster Open Source License
    | Unicode_DFS_2015 -- ^ @Unicode-DFS-2015@, Unicode License Agreement - Data Files and Software (2015)
    | Unicode_DFS_2016 -- ^ @Unicode-DFS-2016@, Unicode License Agreement - Data Files and Software (2016)
    | Unicode_TOU -- ^ @Unicode-TOU@, Unicode Terms of Use
    | UPL_1_0 -- ^ @UPL-1.0@, Universal Permissive License v1.0
    | NCSA -- ^ @NCSA@, University of Illinois/NCSA Open Source License
    | Vim -- ^ @Vim@, Vim License
    | VOSTROM -- ^ @VOSTROM@, VOSTROM Public License for Open Source
    | VSL_1_0 -- ^ @VSL-1.0@, Vovida Software License v1.0
    | W3C_20150513 -- ^ @W3C-20150513@, W3C Software Notice and Document License (2015-05-13)
    | W3C_19980720 -- ^ @W3C-19980720@, W3C Software Notice and License (1998-07-20)
    | W3C -- ^ @W3C@, W3C Software Notice and License (2002-12-31)
    | Wsuipa -- ^ @Wsuipa@, Wsuipa License
    | Xnet -- ^ @Xnet@, X.Net License
    | X11 -- ^ @X11@, X11 License
    | Xerox -- ^ @Xerox@, Xerox License
    | XFree86_1_1 -- ^ @XFree86-1.1@, XFree86 License 1.1
    | Xinetd -- ^ @xinetd@, xinetd License
    | Xpp -- ^ @xpp@, XPP License
    | XSkat -- ^ @XSkat@, XSkat License
    | YPL_1_0 -- ^ @YPL-1.0@, Yahoo! Public License v1.0
    | YPL_1_1 -- ^ @YPL-1.1@, Yahoo! Public License v1.1
    | Zed -- ^ @Zed@, Zed License
    | Zend_2_0 -- ^ @Zend-2.0@, Zend License v2.0
    | Zimbra_1_3 -- ^ @Zimbra-1.3@, Zimbra Public License v1.3
    | Zimbra_1_4 -- ^ @Zimbra-1.4@, Zimbra Public License v1.4
    | Zlib -- ^ @Zlib@, zlib License
    | Zlib_acknowledgement -- ^ @zlib-acknowledgement@, zlib/libpng License with Acknowledgement
    | ZPL_1_1 -- ^ @ZPL-1.1@, Zope Public License 1.1
    | ZPL_2_0 -- ^ @ZPL-2.0@, Zope Public License 2.0
    | ZPL_2_1 -- ^ @ZPL-2.1@, Zope Public License 2.1
  deriving (Eq, Ord, Enum, Bounded, Show, Read, Typeable, Data, Generic)

instance Binary LicenseId

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
        maybe (fail $ "Unknown SPDX license identifier: '" ++  n ++ "' " ++ licenseIdMigrationMessage n) return $ mkLicenseId n

instance NFData LicenseId where
    rnf l = l `seq` ()

-- | Help message for migrating from non-SDPX license identifiers.
--
-- Old 'License' is almost SDPX, except for 'BSD2', 'BSD3'. This function
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
-- For other common licenses their old license format coincides with the SPDX identifiers:
--
-- >>> traverse eitherParsec ["GPL-2.0", "GPL-3.0", "LGPL-2.1", "MIT", "ISC", "MPL-2.0", "Apache-2.0"] :: Either String [LicenseId]
-- Right [GPL_2_0,GPL_3_0,LGPL_2_1,MIT,ISC,MPL_2_0,Apache_2_0]
--
licenseIdMigrationMessage :: String -> String
licenseIdMigrationMessage = go where
    go "BSD3"              = "Do you mean BSD-3-Clause?"
    go "BSD2"              = "Do you mean BSD-2-Clause?"
    go "AllRightsReserved" = "You can use NONE as a value of license field."
    go "OtherLicense"      = "SPDX license list contains plenty of licenses. See https://spdx.org/licenses/. Also they can be combined into complex expressions with AND and OR."
    go "PublicDomain"      = "Public Domain is a complex matter. See https://wiki.spdx.org/view/Legal_Team/Decisions/Dealing_with_Public_Domain_within_SPDX_Files. Consider using a proper license."

    -- otherwise, we don't know
    go _ = ""

-------------------------------------------------------------------------------
-- License Data
-------------------------------------------------------------------------------

-- | License SPDX identifier, e.g. @"BSD-3-Clause"@.
licenseId :: LicenseId -> String
licenseId Glide = "Glide"
licenseId Abstyles = "Abstyles"
licenseId AFL_1_1 = "AFL-1.1"
licenseId AFL_1_2 = "AFL-1.2"
licenseId AFL_2_0 = "AFL-2.0"
licenseId AFL_2_1 = "AFL-2.1"
licenseId AFL_3_0 = "AFL-3.0"
licenseId AMPAS = "AMPAS"
licenseId APL_1_0 = "APL-1.0"
licenseId Adobe_Glyph = "Adobe-Glyph"
licenseId APAFML = "APAFML"
licenseId Adobe_2006 = "Adobe-2006"
licenseId AGPL_1_0 = "AGPL-1.0"
licenseId Afmparse = "Afmparse"
licenseId Aladdin = "Aladdin"
licenseId ADSL = "ADSL"
licenseId AMDPLPA = "AMDPLPA"
licenseId ANTLR_PD = "ANTLR-PD"
licenseId Apache_1_0 = "Apache-1.0"
licenseId Apache_1_1 = "Apache-1.1"
licenseId Apache_2_0 = "Apache-2.0"
licenseId AML = "AML"
licenseId APSL_1_0 = "APSL-1.0"
licenseId APSL_1_1 = "APSL-1.1"
licenseId APSL_1_2 = "APSL-1.2"
licenseId APSL_2_0 = "APSL-2.0"
licenseId Artistic_1_0 = "Artistic-1.0"
licenseId Artistic_1_0_Perl = "Artistic-1.0-Perl"
licenseId Artistic_1_0_cl8 = "Artistic-1.0-cl8"
licenseId Artistic_2_0 = "Artistic-2.0"
licenseId AAL = "AAL"
licenseId Bahyph = "Bahyph"
licenseId Barr = "Barr"
licenseId Beerware = "Beerware"
licenseId BitTorrent_1_0 = "BitTorrent-1.0"
licenseId BitTorrent_1_1 = "BitTorrent-1.1"
licenseId BSL_1_0 = "BSL-1.0"
licenseId Borceux = "Borceux"
licenseId BSD_2_Clause = "BSD-2-Clause"
licenseId BSD_2_Clause_FreeBSD = "BSD-2-Clause-FreeBSD"
licenseId BSD_2_Clause_NetBSD = "BSD-2-Clause-NetBSD"
licenseId BSD_3_Clause = "BSD-3-Clause"
licenseId BSD_3_Clause_Clear = "BSD-3-Clause-Clear"
licenseId BSD_3_Clause_No_Nuclear_License = "BSD-3-Clause-No-Nuclear-License"
licenseId BSD_3_Clause_No_Nuclear_License_2014 = "BSD-3-Clause-No-Nuclear-License-2014"
licenseId BSD_3_Clause_No_Nuclear_Warranty = "BSD-3-Clause-No-Nuclear-Warranty"
licenseId BSD_4_Clause = "BSD-4-Clause"
licenseId BSD_Protection = "BSD-Protection"
licenseId BSD_Source_Code = "BSD-Source-Code"
licenseId BSD_3_Clause_Attribution = "BSD-3-Clause-Attribution"
licenseId NullBSD = "0BSD"
licenseId BSD_4_Clause_UC = "BSD-4-Clause-UC"
licenseId Bzip2_1_0_5 = "bzip2-1.0.5"
licenseId Bzip2_1_0_6 = "bzip2-1.0.6"
licenseId Caldera = "Caldera"
licenseId CECILL_1_0 = "CECILL-1.0"
licenseId CECILL_1_1 = "CECILL-1.1"
licenseId CECILL_2_0 = "CECILL-2.0"
licenseId CECILL_2_1 = "CECILL-2.1"
licenseId CECILL_B = "CECILL-B"
licenseId CECILL_C = "CECILL-C"
licenseId ClArtistic = "ClArtistic"
licenseId MIT_CMU = "MIT-CMU"
licenseId CNRI_Jython = "CNRI-Jython"
licenseId CNRI_Python = "CNRI-Python"
licenseId CNRI_Python_GPL_Compatible = "CNRI-Python-GPL-Compatible"
licenseId CPOL_1_02 = "CPOL-1.02"
licenseId CDDL_1_0 = "CDDL-1.0"
licenseId CDDL_1_1 = "CDDL-1.1"
licenseId CPAL_1_0 = "CPAL-1.0"
licenseId CPL_1_0 = "CPL-1.0"
licenseId CATOSL_1_1 = "CATOSL-1.1"
licenseId Condor_1_1 = "Condor-1.1"
licenseId CC_BY_1_0 = "CC-BY-1.0"
licenseId CC_BY_2_0 = "CC-BY-2.0"
licenseId CC_BY_2_5 = "CC-BY-2.5"
licenseId CC_BY_3_0 = "CC-BY-3.0"
licenseId CC_BY_4_0 = "CC-BY-4.0"
licenseId CC_BY_ND_1_0 = "CC-BY-ND-1.0"
licenseId CC_BY_ND_2_0 = "CC-BY-ND-2.0"
licenseId CC_BY_ND_2_5 = "CC-BY-ND-2.5"
licenseId CC_BY_ND_3_0 = "CC-BY-ND-3.0"
licenseId CC_BY_ND_4_0 = "CC-BY-ND-4.0"
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
licenseId CC_BY_SA_1_0 = "CC-BY-SA-1.0"
licenseId CC_BY_SA_2_0 = "CC-BY-SA-2.0"
licenseId CC_BY_SA_2_5 = "CC-BY-SA-2.5"
licenseId CC_BY_SA_3_0 = "CC-BY-SA-3.0"
licenseId CC_BY_SA_4_0 = "CC-BY-SA-4.0"
licenseId CC0_1_0 = "CC0-1.0"
licenseId Crossword = "Crossword"
licenseId CrystalStacker = "CrystalStacker"
licenseId CUA_OPL_1_0 = "CUA-OPL-1.0"
licenseId Cube = "Cube"
licenseId Curl = "curl"
licenseId D_FSL_1_0 = "D-FSL-1.0"
licenseId Diffmark = "diffmark"
licenseId WTFPL = "WTFPL"
licenseId DOC = "DOC"
licenseId Dotseqn = "Dotseqn"
licenseId DSDP = "DSDP"
licenseId Dvipdfm = "dvipdfm"
licenseId EPL_1_0 = "EPL-1.0"
licenseId ECL_1_0 = "ECL-1.0"
licenseId ECL_2_0 = "ECL-2.0"
licenseId EGenix = "eGenix"
licenseId EFL_1_0 = "EFL-1.0"
licenseId EFL_2_0 = "EFL-2.0"
licenseId MIT_advertising = "MIT-advertising"
licenseId MIT_enna = "MIT-enna"
licenseId Entessa = "Entessa"
licenseId ErlPL_1_1 = "ErlPL-1.1"
licenseId EUDatagrid = "EUDatagrid"
licenseId EUPL_1_0 = "EUPL-1.0"
licenseId EUPL_1_1 = "EUPL-1.1"
licenseId Eurosym = "Eurosym"
licenseId Fair = "Fair"
licenseId MIT_feh = "MIT-feh"
licenseId Frameworx_1_0 = "Frameworx-1.0"
licenseId FreeImage = "FreeImage"
licenseId FTL = "FTL"
licenseId FSFAP = "FSFAP"
licenseId FSFUL = "FSFUL"
licenseId FSFULLR = "FSFULLR"
licenseId Giftware = "Giftware"
licenseId GL2PS = "GL2PS"
licenseId Glulxe = "Glulxe"
licenseId AGPL_3_0 = "AGPL-3.0"
licenseId GFDL_1_1 = "GFDL-1.1"
licenseId GFDL_1_2 = "GFDL-1.2"
licenseId GFDL_1_3 = "GFDL-1.3"
licenseId GPL_1_0 = "GPL-1.0"
licenseId GPL_2_0 = "GPL-2.0"
licenseId GPL_3_0 = "GPL-3.0"
licenseId LGPL_2_1 = "LGPL-2.1"
licenseId LGPL_3_0 = "LGPL-3.0"
licenseId LGPL_2_0 = "LGPL-2.0"
licenseId Gnuplot = "gnuplot"
licenseId GSOAP_1_3b = "gSOAP-1.3b"
licenseId HaskellReport = "HaskellReport"
licenseId HPND = "HPND"
licenseId IBM_pibs = "IBM-pibs"
licenseId IPL_1_0 = "IPL-1.0"
licenseId ICU = "ICU"
licenseId ImageMagick = "ImageMagick"
licenseId IMatix = "iMatix"
licenseId Imlib2 = "Imlib2"
licenseId IJG = "IJG"
licenseId Info_ZIP = "Info-ZIP"
licenseId Intel_ACPI = "Intel-ACPI"
licenseId Intel = "Intel"
licenseId Interbase_1_0 = "Interbase-1.0"
licenseId IPA = "IPA"
licenseId ISC = "ISC"
licenseId JasPer_2_0 = "JasPer-2.0"
licenseId JSON = "JSON"
licenseId LPPL_1_0 = "LPPL-1.0"
licenseId LPPL_1_1 = "LPPL-1.1"
licenseId LPPL_1_2 = "LPPL-1.2"
licenseId LPPL_1_3a = "LPPL-1.3a"
licenseId LPPL_1_3c = "LPPL-1.3c"
licenseId Latex2e = "Latex2e"
licenseId BSD_3_Clause_LBNL = "BSD-3-Clause-LBNL"
licenseId Leptonica = "Leptonica"
licenseId LGPLLR = "LGPLLR"
licenseId Libpng = "Libpng"
licenseId Libtiff = "libtiff"
licenseId LAL_1_2 = "LAL-1.2"
licenseId LAL_1_3 = "LAL-1.3"
licenseId LiLiQ_P_1_1 = "LiLiQ-P-1.1"
licenseId LiLiQ_Rplus_1_1 = "LiLiQ-Rplus-1.1"
licenseId LiLiQ_R_1_1 = "LiLiQ-R-1.1"
licenseId LPL_1_02 = "LPL-1.02"
licenseId LPL_1_0 = "LPL-1.0"
licenseId MakeIndex = "MakeIndex"
licenseId MTLL = "MTLL"
licenseId MS_PL = "MS-PL"
licenseId MS_RL = "MS-RL"
licenseId MirOS = "MirOS"
licenseId MITNFA = "MITNFA"
licenseId MIT = "MIT"
licenseId Motosoto = "Motosoto"
licenseId MPL_1_0 = "MPL-1.0"
licenseId MPL_1_1 = "MPL-1.1"
licenseId MPL_2_0 = "MPL-2.0"
licenseId MPL_2_0_no_copyleft_exception = "MPL-2.0-no-copyleft-exception"
licenseId Mpich2 = "mpich2"
licenseId Multics = "Multics"
licenseId Mup = "Mup"
licenseId NASA_1_3 = "NASA-1.3"
licenseId Naumen = "Naumen"
licenseId NBPL_1_0 = "NBPL-1.0"
licenseId Net_SNMP = "Net-SNMP"
licenseId NetCDF = "NetCDF"
licenseId NGPL = "NGPL"
licenseId NOSL = "NOSL"
licenseId NPL_1_0 = "NPL-1.0"
licenseId NPL_1_1 = "NPL-1.1"
licenseId Newsletr = "Newsletr"
licenseId NLPL = "NLPL"
licenseId Nokia = "Nokia"
licenseId NPOSL_3_0 = "NPOSL-3.0"
licenseId NLOD_1_0 = "NLOD-1.0"
licenseId Noweb = "Noweb"
licenseId NRL = "NRL"
licenseId NTP = "NTP"
licenseId Nunit = "Nunit"
licenseId OCLC_2_0 = "OCLC-2.0"
licenseId ODbL_1_0 = "ODbL-1.0"
licenseId PDDL_1_0 = "PDDL-1.0"
licenseId OCCT_PL = "OCCT-PL"
licenseId OGTSL = "OGTSL"
licenseId OLDAP_2_2_2 = "OLDAP-2.2.2"
licenseId OLDAP_1_1 = "OLDAP-1.1"
licenseId OLDAP_1_2 = "OLDAP-1.2"
licenseId OLDAP_1_3 = "OLDAP-1.3"
licenseId OLDAP_1_4 = "OLDAP-1.4"
licenseId OLDAP_2_0 = "OLDAP-2.0"
licenseId OLDAP_2_0_1 = "OLDAP-2.0.1"
licenseId OLDAP_2_1 = "OLDAP-2.1"
licenseId OLDAP_2_2 = "OLDAP-2.2"
licenseId OLDAP_2_2_1 = "OLDAP-2.2.1"
licenseId OLDAP_2_3 = "OLDAP-2.3"
licenseId OLDAP_2_4 = "OLDAP-2.4"
licenseId OLDAP_2_5 = "OLDAP-2.5"
licenseId OLDAP_2_6 = "OLDAP-2.6"
licenseId OLDAP_2_7 = "OLDAP-2.7"
licenseId OLDAP_2_8 = "OLDAP-2.8"
licenseId OML = "OML"
licenseId OPL_1_0 = "OPL-1.0"
licenseId OSL_1_0 = "OSL-1.0"
licenseId OSL_1_1 = "OSL-1.1"
licenseId OSL_2_0 = "OSL-2.0"
licenseId OSL_2_1 = "OSL-2.1"
licenseId OSL_3_0 = "OSL-3.0"
licenseId OpenSSL = "OpenSSL"
licenseId OSET_PL_2_1 = "OSET-PL-2.1"
licenseId PHP_3_0 = "PHP-3.0"
licenseId PHP_3_01 = "PHP-3.01"
licenseId Plexus = "Plexus"
licenseId PostgreSQL = "PostgreSQL"
licenseId Psfrag = "psfrag"
licenseId Psutils = "psutils"
licenseId Python_2_0 = "Python-2.0"
licenseId QPL_1_0 = "QPL-1.0"
licenseId Qhull = "Qhull"
licenseId Rdisc = "Rdisc"
licenseId RPSL_1_0 = "RPSL-1.0"
licenseId RPL_1_1 = "RPL-1.1"
licenseId RPL_1_5 = "RPL-1.5"
licenseId RHeCos_1_1 = "RHeCos-1.1"
licenseId RSCPL = "RSCPL"
licenseId RSA_MD = "RSA-MD"
licenseId Ruby = "Ruby"
licenseId SAX_PD = "SAX-PD"
licenseId Saxpath = "Saxpath"
licenseId SCEA = "SCEA"
licenseId SWL = "SWL"
licenseId SMPPL = "SMPPL"
licenseId Sendmail = "Sendmail"
licenseId SGI_B_1_0 = "SGI-B-1.0"
licenseId SGI_B_1_1 = "SGI-B-1.1"
licenseId SGI_B_2_0 = "SGI-B-2.0"
licenseId OFL_1_0 = "OFL-1.0"
licenseId OFL_1_1 = "OFL-1.1"
licenseId SimPL_2_0 = "SimPL-2.0"
licenseId Sleepycat = "Sleepycat"
licenseId SNIA = "SNIA"
licenseId Spencer_86 = "Spencer-86"
licenseId Spencer_94 = "Spencer-94"
licenseId Spencer_99 = "Spencer-99"
licenseId SMLNJ = "SMLNJ"
licenseId SugarCRM_1_1_3 = "SugarCRM-1.1.3"
licenseId SISSL = "SISSL"
licenseId SISSL_1_2 = "SISSL-1.2"
licenseId SPL_1_0 = "SPL-1.0"
licenseId Watcom_1_0 = "Watcom-1.0"
licenseId TCL = "TCL"
licenseId TCP_wrappers = "TCP-wrappers"
licenseId Unlicense = "Unlicense"
licenseId TMate = "TMate"
licenseId TORQUE_1_1 = "TORQUE-1.1"
licenseId TOSL = "TOSL"
licenseId Unicode_DFS_2015 = "Unicode-DFS-2015"
licenseId Unicode_DFS_2016 = "Unicode-DFS-2016"
licenseId Unicode_TOU = "Unicode-TOU"
licenseId UPL_1_0 = "UPL-1.0"
licenseId NCSA = "NCSA"
licenseId Vim = "Vim"
licenseId VOSTROM = "VOSTROM"
licenseId VSL_1_0 = "VSL-1.0"
licenseId W3C_20150513 = "W3C-20150513"
licenseId W3C_19980720 = "W3C-19980720"
licenseId W3C = "W3C"
licenseId Wsuipa = "Wsuipa"
licenseId Xnet = "Xnet"
licenseId X11 = "X11"
licenseId Xerox = "Xerox"
licenseId XFree86_1_1 = "XFree86-1.1"
licenseId Xinetd = "xinetd"
licenseId Xpp = "xpp"
licenseId XSkat = "XSkat"
licenseId YPL_1_0 = "YPL-1.0"
licenseId YPL_1_1 = "YPL-1.1"
licenseId Zed = "Zed"
licenseId Zend_2_0 = "Zend-2.0"
licenseId Zimbra_1_3 = "Zimbra-1.3"
licenseId Zimbra_1_4 = "Zimbra-1.4"
licenseId Zlib = "Zlib"
licenseId Zlib_acknowledgement = "zlib-acknowledgement"
licenseId ZPL_1_1 = "ZPL-1.1"
licenseId ZPL_2_0 = "ZPL-2.0"
licenseId ZPL_2_1 = "ZPL-2.1"

-- | License name, e.g. @"GNU General Public License v2.0 only"@
licenseName :: LicenseId -> String
licenseName Glide = "3dfx Glide License"
licenseName Abstyles = "Abstyles License"
licenseName AFL_1_1 = "Academic Free License v1.1"
licenseName AFL_1_2 = "Academic Free License v1.2"
licenseName AFL_2_0 = "Academic Free License v2.0"
licenseName AFL_2_1 = "Academic Free License v2.1"
licenseName AFL_3_0 = "Academic Free License v3.0"
licenseName AMPAS = "Academy of Motion Picture Arts and Sciences BSD"
licenseName APL_1_0 = "Adaptive Public License 1.0"
licenseName Adobe_Glyph = "Adobe Glyph List License"
licenseName APAFML = "Adobe Postscript AFM License"
licenseName Adobe_2006 = "Adobe Systems Incorporated Source Code License Agreement"
licenseName AGPL_1_0 = "Affero General Public License v1.0"
licenseName Afmparse = "Afmparse License"
licenseName Aladdin = "Aladdin Free Public License"
licenseName ADSL = "Amazon Digital Services License"
licenseName AMDPLPA = "AMD's plpa_map.c License"
licenseName ANTLR_PD = "ANTLR Software Rights Notice"
licenseName Apache_1_0 = "Apache License 1.0"
licenseName Apache_1_1 = "Apache License 1.1"
licenseName Apache_2_0 = "Apache License 2.0"
licenseName AML = "Apple MIT License"
licenseName APSL_1_0 = "Apple Public Source License 1.0"
licenseName APSL_1_1 = "Apple Public Source License 1.1"
licenseName APSL_1_2 = "Apple Public Source License 1.2"
licenseName APSL_2_0 = "Apple Public Source License 2.0"
licenseName Artistic_1_0 = "Artistic License 1.0"
licenseName Artistic_1_0_Perl = "Artistic License 1.0 (Perl)"
licenseName Artistic_1_0_cl8 = "Artistic License 1.0 w/clause 8"
licenseName Artistic_2_0 = "Artistic License 2.0"
licenseName AAL = "Attribution Assurance License"
licenseName Bahyph = "Bahyph License"
licenseName Barr = "Barr License"
licenseName Beerware = "Beerware License"
licenseName BitTorrent_1_0 = "BitTorrent Open Source License v1.0"
licenseName BitTorrent_1_1 = "BitTorrent Open Source License v1.1"
licenseName BSL_1_0 = "Boost Software License 1.0"
licenseName Borceux = "Borceux license"
licenseName BSD_2_Clause = "BSD 2-clause \"Simplified\" License"
licenseName BSD_2_Clause_FreeBSD = "BSD 2-clause FreeBSD License"
licenseName BSD_2_Clause_NetBSD = "BSD 2-clause NetBSD License"
licenseName BSD_3_Clause = "BSD 3-clause \"New\" or \"Revised\" License"
licenseName BSD_3_Clause_Clear = "BSD 3-clause Clear License"
licenseName BSD_3_Clause_No_Nuclear_License = "BSD 3-Clause No Nuclear License"
licenseName BSD_3_Clause_No_Nuclear_License_2014 = "BSD 3-Clause No Nuclear License 2014"
licenseName BSD_3_Clause_No_Nuclear_Warranty = "BSD 3-Clause No Nuclear Warranty"
licenseName BSD_4_Clause = "BSD 4-clause \"Original\" or \"Old\" License"
licenseName BSD_Protection = "BSD Protection License"
licenseName BSD_Source_Code = "BSD Source Code Attribution"
licenseName BSD_3_Clause_Attribution = "BSD with attribution"
licenseName NullBSD = "BSD Zero Clause License"
licenseName BSD_4_Clause_UC = "BSD-4-Clause (University of California-Specific)"
licenseName Bzip2_1_0_5 = "bzip2 and libbzip2 License v1.0.5"
licenseName Bzip2_1_0_6 = "bzip2 and libbzip2 License v1.0.6"
licenseName Caldera = "Caldera License"
licenseName CECILL_1_0 = "CeCILL Free Software License Agreement v1.0"
licenseName CECILL_1_1 = "CeCILL Free Software License Agreement v1.1"
licenseName CECILL_2_0 = "CeCILL Free Software License Agreement v2.0"
licenseName CECILL_2_1 = "CeCILL Free Software License Agreement v2.1"
licenseName CECILL_B = "CeCILL-B Free Software License Agreement"
licenseName CECILL_C = "CeCILL-C Free Software License Agreement"
licenseName ClArtistic = "Clarified Artistic License"
licenseName MIT_CMU = "CMU License"
licenseName CNRI_Jython = "CNRI Jython License"
licenseName CNRI_Python = "CNRI Python License"
licenseName CNRI_Python_GPL_Compatible = "CNRI Python Open Source GPL Compatible License Agreement"
licenseName CPOL_1_02 = "Code Project Open License 1.02"
licenseName CDDL_1_0 = "Common Development and Distribution License 1.0"
licenseName CDDL_1_1 = "Common Development and Distribution License 1.1"
licenseName CPAL_1_0 = "Common Public Attribution License 1.0"
licenseName CPL_1_0 = "Common Public License 1.0"
licenseName CATOSL_1_1 = "Computer Associates Trusted Open Source License 1.1"
licenseName Condor_1_1 = "Condor Public License v1.1"
licenseName CC_BY_1_0 = "Creative Commons Attribution 1.0"
licenseName CC_BY_2_0 = "Creative Commons Attribution 2.0"
licenseName CC_BY_2_5 = "Creative Commons Attribution 2.5"
licenseName CC_BY_3_0 = "Creative Commons Attribution 3.0"
licenseName CC_BY_4_0 = "Creative Commons Attribution 4.0"
licenseName CC_BY_ND_1_0 = "Creative Commons Attribution No Derivatives 1.0"
licenseName CC_BY_ND_2_0 = "Creative Commons Attribution No Derivatives 2.0"
licenseName CC_BY_ND_2_5 = "Creative Commons Attribution No Derivatives 2.5"
licenseName CC_BY_ND_3_0 = "Creative Commons Attribution No Derivatives 3.0"
licenseName CC_BY_ND_4_0 = "Creative Commons Attribution No Derivatives 4.0"
licenseName CC_BY_NC_1_0 = "Creative Commons Attribution Non Commercial 1.0"
licenseName CC_BY_NC_2_0 = "Creative Commons Attribution Non Commercial 2.0"
licenseName CC_BY_NC_2_5 = "Creative Commons Attribution Non Commercial 2.5"
licenseName CC_BY_NC_3_0 = "Creative Commons Attribution Non Commercial 3.0"
licenseName CC_BY_NC_4_0 = "Creative Commons Attribution Non Commercial 4.0"
licenseName CC_BY_NC_ND_1_0 = "Creative Commons Attribution Non Commercial No Derivatives 1.0"
licenseName CC_BY_NC_ND_2_0 = "Creative Commons Attribution Non Commercial No Derivatives 2.0"
licenseName CC_BY_NC_ND_2_5 = "Creative Commons Attribution Non Commercial No Derivatives 2.5"
licenseName CC_BY_NC_ND_3_0 = "Creative Commons Attribution Non Commercial No Derivatives 3.0"
licenseName CC_BY_NC_ND_4_0 = "Creative Commons Attribution Non Commercial No Derivatives 4.0"
licenseName CC_BY_NC_SA_1_0 = "Creative Commons Attribution Non Commercial Share Alike 1.0"
licenseName CC_BY_NC_SA_2_0 = "Creative Commons Attribution Non Commercial Share Alike 2.0"
licenseName CC_BY_NC_SA_2_5 = "Creative Commons Attribution Non Commercial Share Alike 2.5"
licenseName CC_BY_NC_SA_3_0 = "Creative Commons Attribution Non Commercial Share Alike 3.0"
licenseName CC_BY_NC_SA_4_0 = "Creative Commons Attribution Non Commercial Share Alike 4.0"
licenseName CC_BY_SA_1_0 = "Creative Commons Attribution Share Alike 1.0"
licenseName CC_BY_SA_2_0 = "Creative Commons Attribution Share Alike 2.0"
licenseName CC_BY_SA_2_5 = "Creative Commons Attribution Share Alike 2.5"
licenseName CC_BY_SA_3_0 = "Creative Commons Attribution Share Alike 3.0"
licenseName CC_BY_SA_4_0 = "Creative Commons Attribution Share Alike 4.0"
licenseName CC0_1_0 = "Creative Commons Zero v1.0 Universal"
licenseName Crossword = "Crossword License"
licenseName CrystalStacker = "CrystalStacker License"
licenseName CUA_OPL_1_0 = "CUA Office Public License v1.0"
licenseName Cube = "Cube License"
licenseName Curl = "curl License"
licenseName D_FSL_1_0 = "Deutsche Freie Software Lizenz"
licenseName Diffmark = "diffmark license"
licenseName WTFPL = "Do What The F*ck You Want To Public License"
licenseName DOC = "DOC License"
licenseName Dotseqn = "Dotseqn License"
licenseName DSDP = "DSDP License"
licenseName Dvipdfm = "dvipdfm License"
licenseName EPL_1_0 = "Eclipse Public License 1.0"
licenseName ECL_1_0 = "Educational Community License v1.0"
licenseName ECL_2_0 = "Educational Community License v2.0"
licenseName EGenix = "eGenix.com Public License 1.1.0"
licenseName EFL_1_0 = "Eiffel Forum License v1.0"
licenseName EFL_2_0 = "Eiffel Forum License v2.0"
licenseName MIT_advertising = "Enlightenment License (e16)"
licenseName MIT_enna = "enna License"
licenseName Entessa = "Entessa Public License v1.0"
licenseName ErlPL_1_1 = "Erlang Public License v1.1"
licenseName EUDatagrid = "EU DataGrid Software License"
licenseName EUPL_1_0 = "European Union Public License 1.0"
licenseName EUPL_1_1 = "European Union Public License 1.1"
licenseName Eurosym = "Eurosym License"
licenseName Fair = "Fair License"
licenseName MIT_feh = "feh License"
licenseName Frameworx_1_0 = "Frameworx Open License 1.0"
licenseName FreeImage = "FreeImage Public License v1.0"
licenseName FTL = "Freetype Project License"
licenseName FSFAP = "FSF All Permissive License"
licenseName FSFUL = "FSF Unlimited License"
licenseName FSFULLR = "FSF Unlimited License (with License Retention)"
licenseName Giftware = "Giftware License"
licenseName GL2PS = "GL2PS License"
licenseName Glulxe = "Glulxe License"
licenseName AGPL_3_0 = "GNU Affero General Public License v3.0"
licenseName GFDL_1_1 = "GNU Free Documentation License v1.1"
licenseName GFDL_1_2 = "GNU Free Documentation License v1.2"
licenseName GFDL_1_3 = "GNU Free Documentation License v1.3"
licenseName GPL_1_0 = "GNU General Public License v1.0 only"
licenseName GPL_2_0 = "GNU General Public License v2.0 only"
licenseName GPL_3_0 = "GNU General Public License v3.0 only"
licenseName LGPL_2_1 = "GNU Lesser General Public License v2.1 only"
licenseName LGPL_3_0 = "GNU Lesser General Public License v3.0 only"
licenseName LGPL_2_0 = "GNU Library General Public License v2 only"
licenseName Gnuplot = "gnuplot License"
licenseName GSOAP_1_3b = "gSOAP Public License v1.3b"
licenseName HaskellReport = "Haskell Language Report License"
licenseName HPND = "Historic Permission Notice and Disclaimer"
licenseName IBM_pibs = "IBM PowerPC Initialization and Boot Software"
licenseName IPL_1_0 = "IBM Public License v1.0"
licenseName ICU = "ICU License"
licenseName ImageMagick = "ImageMagick License"
licenseName IMatix = "iMatix Standard Function Library Agreement"
licenseName Imlib2 = "Imlib2 License"
licenseName IJG = "Independent JPEG Group License"
licenseName Info_ZIP = "Info-ZIP License"
licenseName Intel_ACPI = "Intel ACPI Software License Agreement"
licenseName Intel = "Intel Open Source License"
licenseName Interbase_1_0 = "Interbase Public License v1.0"
licenseName IPA = "IPA Font License"
licenseName ISC = "ISC License"
licenseName JasPer_2_0 = "JasPer License"
licenseName JSON = "JSON License"
licenseName LPPL_1_0 = "LaTeX Project Public License v1.0"
licenseName LPPL_1_1 = "LaTeX Project Public License v1.1"
licenseName LPPL_1_2 = "LaTeX Project Public License v1.2"
licenseName LPPL_1_3a = "LaTeX Project Public License v1.3a"
licenseName LPPL_1_3c = "LaTeX Project Public License v1.3c"
licenseName Latex2e = "Latex2e License"
licenseName BSD_3_Clause_LBNL = "Lawrence Berkeley National Labs BSD variant license"
licenseName Leptonica = "Leptonica License"
licenseName LGPLLR = "Lesser General Public License For Linguistic Resources"
licenseName Libpng = "libpng License"
licenseName Libtiff = "libtiff License"
licenseName LAL_1_2 = "Licence Art Libre 1.2"
licenseName LAL_1_3 = "Licence Art Libre 1.3"
licenseName LiLiQ_P_1_1 = "Licence Libre du Qu\233bec \8211 Permissive version 1.1"
licenseName LiLiQ_Rplus_1_1 = "Licence Libre du Qu\233bec \8211 R\233ciprocit\233 forte version 1.1"
licenseName LiLiQ_R_1_1 = "Licence Libre du Qu\233bec \8211 R\233ciprocit\233 version 1.1"
licenseName LPL_1_02 = "Lucent Public License v1.02"
licenseName LPL_1_0 = "Lucent Public License Version 1.0"
licenseName MakeIndex = "MakeIndex License"
licenseName MTLL = "Matrix Template Library License"
licenseName MS_PL = "Microsoft Public License"
licenseName MS_RL = "Microsoft Reciprocal License"
licenseName MirOS = "MirOS Licence"
licenseName MITNFA = "MIT +no-false-attribs license"
licenseName MIT = "MIT License"
licenseName Motosoto = "Motosoto License"
licenseName MPL_1_0 = "Mozilla Public License 1.0"
licenseName MPL_1_1 = "Mozilla Public License 1.1"
licenseName MPL_2_0 = "Mozilla Public License 2.0"
licenseName MPL_2_0_no_copyleft_exception = "Mozilla Public License 2.0 (no copyleft exception)"
licenseName Mpich2 = "mpich2 License"
licenseName Multics = "Multics License"
licenseName Mup = "Mup License"
licenseName NASA_1_3 = "NASA Open Source Agreement 1.3"
licenseName Naumen = "Naumen Public License"
licenseName NBPL_1_0 = "Net Boolean Public License v1"
licenseName Net_SNMP = "Net-SNMP License"
licenseName NetCDF = "NetCDF license"
licenseName NGPL = "Nethack General Public License"
licenseName NOSL = "Netizen Open Source License"
licenseName NPL_1_0 = "Netscape Public License v1.0"
licenseName NPL_1_1 = "Netscape Public License v1.1"
licenseName Newsletr = "Newsletr License"
licenseName NLPL = "No Limit Public License"
licenseName Nokia = "Nokia Open Source License"
licenseName NPOSL_3_0 = "Non-Profit Open Software License 3.0"
licenseName NLOD_1_0 = "Norwegian Licence for Open Government Data"
licenseName Noweb = "Noweb License"
licenseName NRL = "NRL License"
licenseName NTP = "NTP License"
licenseName Nunit = "Nunit License"
licenseName OCLC_2_0 = "OCLC Research Public License 2.0"
licenseName ODbL_1_0 = "ODC Open Database License v1.0"
licenseName PDDL_1_0 = "ODC Public Domain Dedication & License 1.0"
licenseName OCCT_PL = "Open CASCADE Technology Public License"
licenseName OGTSL = "Open Group Test Suite License"
licenseName OLDAP_2_2_2 = "Open LDAP Public License  2.2.2"
licenseName OLDAP_1_1 = "Open LDAP Public License v1.1"
licenseName OLDAP_1_2 = "Open LDAP Public License v1.2"
licenseName OLDAP_1_3 = "Open LDAP Public License v1.3"
licenseName OLDAP_1_4 = "Open LDAP Public License v1.4"
licenseName OLDAP_2_0 = "Open LDAP Public License v2.0 (or possibly 2.0A and 2.0B)"
licenseName OLDAP_2_0_1 = "Open LDAP Public License v2.0.1"
licenseName OLDAP_2_1 = "Open LDAP Public License v2.1"
licenseName OLDAP_2_2 = "Open LDAP Public License v2.2"
licenseName OLDAP_2_2_1 = "Open LDAP Public License v2.2.1"
licenseName OLDAP_2_3 = "Open LDAP Public License v2.3"
licenseName OLDAP_2_4 = "Open LDAP Public License v2.4"
licenseName OLDAP_2_5 = "Open LDAP Public License v2.5"
licenseName OLDAP_2_6 = "Open LDAP Public License v2.6"
licenseName OLDAP_2_7 = "Open LDAP Public License v2.7"
licenseName OLDAP_2_8 = "Open LDAP Public License v2.8"
licenseName OML = "Open Market License"
licenseName OPL_1_0 = "Open Public License v1.0"
licenseName OSL_1_0 = "Open Software License 1.0"
licenseName OSL_1_1 = "Open Software License 1.1"
licenseName OSL_2_0 = "Open Software License 2.0"
licenseName OSL_2_1 = "Open Software License 2.1"
licenseName OSL_3_0 = "Open Software License 3.0"
licenseName OpenSSL = "OpenSSL License"
licenseName OSET_PL_2_1 = "OSET Public License version 2.1"
licenseName PHP_3_0 = "PHP License v3.0"
licenseName PHP_3_01 = "PHP License v3.01"
licenseName Plexus = "Plexus Classworlds License"
licenseName PostgreSQL = "PostgreSQL License"
licenseName Psfrag = "psfrag License"
licenseName Psutils = "psutils License"
licenseName Python_2_0 = "Python License 2.0"
licenseName QPL_1_0 = "Q Public License 1.0"
licenseName Qhull = "Qhull License"
licenseName Rdisc = "Rdisc License"
licenseName RPSL_1_0 = "RealNetworks Public Source License v1.0"
licenseName RPL_1_1 = "Reciprocal Public License 1.1"
licenseName RPL_1_5 = "Reciprocal Public License 1.5"
licenseName RHeCos_1_1 = "Red Hat eCos Public License v1.1"
licenseName RSCPL = "Ricoh Source Code Public License"
licenseName RSA_MD = "RSA Message-Digest License "
licenseName Ruby = "Ruby License"
licenseName SAX_PD = "Sax Public Domain Notice"
licenseName Saxpath = "Saxpath License"
licenseName SCEA = "SCEA Shared Source License"
licenseName SWL = "Scheme Widget Library (SWL) Software License Agreement"
licenseName SMPPL = "Secure Messaging Protocol Public License"
licenseName Sendmail = "Sendmail License"
licenseName SGI_B_1_0 = "SGI Free Software License B v1.0"
licenseName SGI_B_1_1 = "SGI Free Software License B v1.1"
licenseName SGI_B_2_0 = "SGI Free Software License B v2.0"
licenseName OFL_1_0 = "SIL Open Font License 1.0"
licenseName OFL_1_1 = "SIL Open Font License 1.1"
licenseName SimPL_2_0 = "Simple Public License 2.0"
licenseName Sleepycat = "Sleepycat License"
licenseName SNIA = "SNIA Public License 1.1"
licenseName Spencer_86 = "Spencer License 86"
licenseName Spencer_94 = "Spencer License 94"
licenseName Spencer_99 = "Spencer License 99"
licenseName SMLNJ = "Standard ML of New Jersey License"
licenseName SugarCRM_1_1_3 = "SugarCRM Public License v1.1.3"
licenseName SISSL = "Sun Industry Standards Source License v1.1"
licenseName SISSL_1_2 = "Sun Industry Standards Source License v1.2"
licenseName SPL_1_0 = "Sun Public License v1.0"
licenseName Watcom_1_0 = "Sybase Open Watcom Public License 1.0"
licenseName TCL = "TCL/TK License"
licenseName TCP_wrappers = "TCP Wrappers License"
licenseName Unlicense = "The Unlicense"
licenseName TMate = "TMate Open Source License"
licenseName TORQUE_1_1 = "TORQUE v2.5+ Software License v1.1"
licenseName TOSL = "Trusster Open Source License"
licenseName Unicode_DFS_2015 = "Unicode License Agreement - Data Files and Software (2015)"
licenseName Unicode_DFS_2016 = "Unicode License Agreement - Data Files and Software (2016)"
licenseName Unicode_TOU = "Unicode Terms of Use"
licenseName UPL_1_0 = "Universal Permissive License v1.0"
licenseName NCSA = "University of Illinois/NCSA Open Source License"
licenseName Vim = "Vim License"
licenseName VOSTROM = "VOSTROM Public License for Open Source"
licenseName VSL_1_0 = "Vovida Software License v1.0"
licenseName W3C_20150513 = "W3C Software Notice and Document License (2015-05-13)"
licenseName W3C_19980720 = "W3C Software Notice and License (1998-07-20)"
licenseName W3C = "W3C Software Notice and License (2002-12-31)"
licenseName Wsuipa = "Wsuipa License"
licenseName Xnet = "X.Net License"
licenseName X11 = "X11 License"
licenseName Xerox = "Xerox License"
licenseName XFree86_1_1 = "XFree86 License 1.1"
licenseName Xinetd = "xinetd License"
licenseName Xpp = "XPP License"
licenseName XSkat = "XSkat License"
licenseName YPL_1_0 = "Yahoo! Public License v1.0"
licenseName YPL_1_1 = "Yahoo! Public License v1.1"
licenseName Zed = "Zed License"
licenseName Zend_2_0 = "Zend License v2.0"
licenseName Zimbra_1_3 = "Zimbra Public License v1.3"
licenseName Zimbra_1_4 = "Zimbra Public License v1.4"
licenseName Zlib = "zlib License"
licenseName Zlib_acknowledgement = "zlib/libpng License with Acknowledgement"
licenseName ZPL_1_1 = "Zope Public License 1.1"
licenseName ZPL_2_0 = "Zope Public License 2.0"
licenseName ZPL_2_1 = "Zope Public License 2.1"

-- | Whether the license is approved by Open Source Initiative (OSI).
--
-- See <https://opensource.org/licenses/alphabetical>.
licenseIsOsiApproved :: LicenseId -> Bool
licenseIsOsiApproved Glide = False
licenseIsOsiApproved Abstyles = False
licenseIsOsiApproved AFL_1_1 = True
licenseIsOsiApproved AFL_1_2 = True
licenseIsOsiApproved AFL_2_0 = True
licenseIsOsiApproved AFL_2_1 = True
licenseIsOsiApproved AFL_3_0 = True
licenseIsOsiApproved AMPAS = False
licenseIsOsiApproved APL_1_0 = True
licenseIsOsiApproved Adobe_Glyph = False
licenseIsOsiApproved APAFML = False
licenseIsOsiApproved Adobe_2006 = False
licenseIsOsiApproved AGPL_1_0 = False
licenseIsOsiApproved Afmparse = False
licenseIsOsiApproved Aladdin = False
licenseIsOsiApproved ADSL = False
licenseIsOsiApproved AMDPLPA = False
licenseIsOsiApproved ANTLR_PD = False
licenseIsOsiApproved Apache_1_0 = False
licenseIsOsiApproved Apache_1_1 = True
licenseIsOsiApproved Apache_2_0 = True
licenseIsOsiApproved AML = False
licenseIsOsiApproved APSL_1_0 = True
licenseIsOsiApproved APSL_1_1 = True
licenseIsOsiApproved APSL_1_2 = True
licenseIsOsiApproved APSL_2_0 = True
licenseIsOsiApproved Artistic_1_0 = True
licenseIsOsiApproved Artistic_1_0_Perl = True
licenseIsOsiApproved Artistic_1_0_cl8 = True
licenseIsOsiApproved Artistic_2_0 = True
licenseIsOsiApproved AAL = True
licenseIsOsiApproved Bahyph = False
licenseIsOsiApproved Barr = False
licenseIsOsiApproved Beerware = False
licenseIsOsiApproved BitTorrent_1_0 = False
licenseIsOsiApproved BitTorrent_1_1 = False
licenseIsOsiApproved BSL_1_0 = True
licenseIsOsiApproved Borceux = False
licenseIsOsiApproved BSD_2_Clause = True
licenseIsOsiApproved BSD_2_Clause_FreeBSD = False
licenseIsOsiApproved BSD_2_Clause_NetBSD = False
licenseIsOsiApproved BSD_3_Clause = True
licenseIsOsiApproved BSD_3_Clause_Clear = False
licenseIsOsiApproved BSD_3_Clause_No_Nuclear_License = False
licenseIsOsiApproved BSD_3_Clause_No_Nuclear_License_2014 = False
licenseIsOsiApproved BSD_3_Clause_No_Nuclear_Warranty = False
licenseIsOsiApproved BSD_4_Clause = False
licenseIsOsiApproved BSD_Protection = False
licenseIsOsiApproved BSD_Source_Code = False
licenseIsOsiApproved BSD_3_Clause_Attribution = False
licenseIsOsiApproved NullBSD = True
licenseIsOsiApproved BSD_4_Clause_UC = False
licenseIsOsiApproved Bzip2_1_0_5 = False
licenseIsOsiApproved Bzip2_1_0_6 = False
licenseIsOsiApproved Caldera = False
licenseIsOsiApproved CECILL_1_0 = False
licenseIsOsiApproved CECILL_1_1 = False
licenseIsOsiApproved CECILL_2_0 = False
licenseIsOsiApproved CECILL_2_1 = True
licenseIsOsiApproved CECILL_B = False
licenseIsOsiApproved CECILL_C = False
licenseIsOsiApproved ClArtistic = False
licenseIsOsiApproved MIT_CMU = False
licenseIsOsiApproved CNRI_Jython = False
licenseIsOsiApproved CNRI_Python = True
licenseIsOsiApproved CNRI_Python_GPL_Compatible = False
licenseIsOsiApproved CPOL_1_02 = False
licenseIsOsiApproved CDDL_1_0 = True
licenseIsOsiApproved CDDL_1_1 = False
licenseIsOsiApproved CPAL_1_0 = True
licenseIsOsiApproved CPL_1_0 = True
licenseIsOsiApproved CATOSL_1_1 = True
licenseIsOsiApproved Condor_1_1 = False
licenseIsOsiApproved CC_BY_1_0 = False
licenseIsOsiApproved CC_BY_2_0 = False
licenseIsOsiApproved CC_BY_2_5 = False
licenseIsOsiApproved CC_BY_3_0 = False
licenseIsOsiApproved CC_BY_4_0 = False
licenseIsOsiApproved CC_BY_ND_1_0 = False
licenseIsOsiApproved CC_BY_ND_2_0 = False
licenseIsOsiApproved CC_BY_ND_2_5 = False
licenseIsOsiApproved CC_BY_ND_3_0 = False
licenseIsOsiApproved CC_BY_ND_4_0 = False
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
licenseIsOsiApproved CC_BY_SA_1_0 = False
licenseIsOsiApproved CC_BY_SA_2_0 = False
licenseIsOsiApproved CC_BY_SA_2_5 = False
licenseIsOsiApproved CC_BY_SA_3_0 = False
licenseIsOsiApproved CC_BY_SA_4_0 = False
licenseIsOsiApproved CC0_1_0 = False
licenseIsOsiApproved Crossword = False
licenseIsOsiApproved CrystalStacker = False
licenseIsOsiApproved CUA_OPL_1_0 = True
licenseIsOsiApproved Cube = False
licenseIsOsiApproved Curl = False
licenseIsOsiApproved D_FSL_1_0 = False
licenseIsOsiApproved Diffmark = False
licenseIsOsiApproved WTFPL = False
licenseIsOsiApproved DOC = False
licenseIsOsiApproved Dotseqn = False
licenseIsOsiApproved DSDP = False
licenseIsOsiApproved Dvipdfm = False
licenseIsOsiApproved EPL_1_0 = True
licenseIsOsiApproved ECL_1_0 = True
licenseIsOsiApproved ECL_2_0 = True
licenseIsOsiApproved EGenix = False
licenseIsOsiApproved EFL_1_0 = True
licenseIsOsiApproved EFL_2_0 = True
licenseIsOsiApproved MIT_advertising = False
licenseIsOsiApproved MIT_enna = False
licenseIsOsiApproved Entessa = True
licenseIsOsiApproved ErlPL_1_1 = False
licenseIsOsiApproved EUDatagrid = True
licenseIsOsiApproved EUPL_1_0 = False
licenseIsOsiApproved EUPL_1_1 = True
licenseIsOsiApproved Eurosym = False
licenseIsOsiApproved Fair = True
licenseIsOsiApproved MIT_feh = False
licenseIsOsiApproved Frameworx_1_0 = True
licenseIsOsiApproved FreeImage = False
licenseIsOsiApproved FTL = False
licenseIsOsiApproved FSFAP = False
licenseIsOsiApproved FSFUL = False
licenseIsOsiApproved FSFULLR = False
licenseIsOsiApproved Giftware = False
licenseIsOsiApproved GL2PS = False
licenseIsOsiApproved Glulxe = False
licenseIsOsiApproved AGPL_3_0 = True
licenseIsOsiApproved GFDL_1_1 = False
licenseIsOsiApproved GFDL_1_2 = False
licenseIsOsiApproved GFDL_1_3 = False
licenseIsOsiApproved GPL_1_0 = False
licenseIsOsiApproved GPL_2_0 = True
licenseIsOsiApproved GPL_3_0 = True
licenseIsOsiApproved LGPL_2_1 = True
licenseIsOsiApproved LGPL_3_0 = True
licenseIsOsiApproved LGPL_2_0 = True
licenseIsOsiApproved Gnuplot = False
licenseIsOsiApproved GSOAP_1_3b = False
licenseIsOsiApproved HaskellReport = False
licenseIsOsiApproved HPND = True
licenseIsOsiApproved IBM_pibs = False
licenseIsOsiApproved IPL_1_0 = True
licenseIsOsiApproved ICU = False
licenseIsOsiApproved ImageMagick = False
licenseIsOsiApproved IMatix = False
licenseIsOsiApproved Imlib2 = False
licenseIsOsiApproved IJG = False
licenseIsOsiApproved Info_ZIP = False
licenseIsOsiApproved Intel_ACPI = False
licenseIsOsiApproved Intel = True
licenseIsOsiApproved Interbase_1_0 = False
licenseIsOsiApproved IPA = True
licenseIsOsiApproved ISC = True
licenseIsOsiApproved JasPer_2_0 = False
licenseIsOsiApproved JSON = False
licenseIsOsiApproved LPPL_1_0 = False
licenseIsOsiApproved LPPL_1_1 = False
licenseIsOsiApproved LPPL_1_2 = False
licenseIsOsiApproved LPPL_1_3a = False
licenseIsOsiApproved LPPL_1_3c = True
licenseIsOsiApproved Latex2e = False
licenseIsOsiApproved BSD_3_Clause_LBNL = False
licenseIsOsiApproved Leptonica = False
licenseIsOsiApproved LGPLLR = False
licenseIsOsiApproved Libpng = False
licenseIsOsiApproved Libtiff = False
licenseIsOsiApproved LAL_1_2 = False
licenseIsOsiApproved LAL_1_3 = False
licenseIsOsiApproved LiLiQ_P_1_1 = True
licenseIsOsiApproved LiLiQ_Rplus_1_1 = True
licenseIsOsiApproved LiLiQ_R_1_1 = True
licenseIsOsiApproved LPL_1_02 = True
licenseIsOsiApproved LPL_1_0 = True
licenseIsOsiApproved MakeIndex = False
licenseIsOsiApproved MTLL = False
licenseIsOsiApproved MS_PL = True
licenseIsOsiApproved MS_RL = True
licenseIsOsiApproved MirOS = True
licenseIsOsiApproved MITNFA = False
licenseIsOsiApproved MIT = True
licenseIsOsiApproved Motosoto = True
licenseIsOsiApproved MPL_1_0 = True
licenseIsOsiApproved MPL_1_1 = True
licenseIsOsiApproved MPL_2_0 = True
licenseIsOsiApproved MPL_2_0_no_copyleft_exception = True
licenseIsOsiApproved Mpich2 = False
licenseIsOsiApproved Multics = True
licenseIsOsiApproved Mup = False
licenseIsOsiApproved NASA_1_3 = True
licenseIsOsiApproved Naumen = True
licenseIsOsiApproved NBPL_1_0 = False
licenseIsOsiApproved Net_SNMP = False
licenseIsOsiApproved NetCDF = False
licenseIsOsiApproved NGPL = True
licenseIsOsiApproved NOSL = False
licenseIsOsiApproved NPL_1_0 = False
licenseIsOsiApproved NPL_1_1 = False
licenseIsOsiApproved Newsletr = False
licenseIsOsiApproved NLPL = False
licenseIsOsiApproved Nokia = True
licenseIsOsiApproved NPOSL_3_0 = True
licenseIsOsiApproved NLOD_1_0 = False
licenseIsOsiApproved Noweb = False
licenseIsOsiApproved NRL = False
licenseIsOsiApproved NTP = True
licenseIsOsiApproved Nunit = False
licenseIsOsiApproved OCLC_2_0 = True
licenseIsOsiApproved ODbL_1_0 = False
licenseIsOsiApproved PDDL_1_0 = False
licenseIsOsiApproved OCCT_PL = False
licenseIsOsiApproved OGTSL = True
licenseIsOsiApproved OLDAP_2_2_2 = False
licenseIsOsiApproved OLDAP_1_1 = False
licenseIsOsiApproved OLDAP_1_2 = False
licenseIsOsiApproved OLDAP_1_3 = False
licenseIsOsiApproved OLDAP_1_4 = False
licenseIsOsiApproved OLDAP_2_0 = False
licenseIsOsiApproved OLDAP_2_0_1 = False
licenseIsOsiApproved OLDAP_2_1 = False
licenseIsOsiApproved OLDAP_2_2 = False
licenseIsOsiApproved OLDAP_2_2_1 = False
licenseIsOsiApproved OLDAP_2_3 = False
licenseIsOsiApproved OLDAP_2_4 = False
licenseIsOsiApproved OLDAP_2_5 = False
licenseIsOsiApproved OLDAP_2_6 = False
licenseIsOsiApproved OLDAP_2_7 = False
licenseIsOsiApproved OLDAP_2_8 = False
licenseIsOsiApproved OML = False
licenseIsOsiApproved OPL_1_0 = False
licenseIsOsiApproved OSL_1_0 = True
licenseIsOsiApproved OSL_1_1 = False
licenseIsOsiApproved OSL_2_0 = True
licenseIsOsiApproved OSL_2_1 = True
licenseIsOsiApproved OSL_3_0 = True
licenseIsOsiApproved OpenSSL = False
licenseIsOsiApproved OSET_PL_2_1 = True
licenseIsOsiApproved PHP_3_0 = True
licenseIsOsiApproved PHP_3_01 = False
licenseIsOsiApproved Plexus = False
licenseIsOsiApproved PostgreSQL = True
licenseIsOsiApproved Psfrag = False
licenseIsOsiApproved Psutils = False
licenseIsOsiApproved Python_2_0 = True
licenseIsOsiApproved QPL_1_0 = True
licenseIsOsiApproved Qhull = False
licenseIsOsiApproved Rdisc = False
licenseIsOsiApproved RPSL_1_0 = True
licenseIsOsiApproved RPL_1_1 = True
licenseIsOsiApproved RPL_1_5 = True
licenseIsOsiApproved RHeCos_1_1 = False
licenseIsOsiApproved RSCPL = True
licenseIsOsiApproved RSA_MD = False
licenseIsOsiApproved Ruby = False
licenseIsOsiApproved SAX_PD = False
licenseIsOsiApproved Saxpath = False
licenseIsOsiApproved SCEA = False
licenseIsOsiApproved SWL = False
licenseIsOsiApproved SMPPL = False
licenseIsOsiApproved Sendmail = False
licenseIsOsiApproved SGI_B_1_0 = False
licenseIsOsiApproved SGI_B_1_1 = False
licenseIsOsiApproved SGI_B_2_0 = False
licenseIsOsiApproved OFL_1_0 = False
licenseIsOsiApproved OFL_1_1 = True
licenseIsOsiApproved SimPL_2_0 = True
licenseIsOsiApproved Sleepycat = True
licenseIsOsiApproved SNIA = False
licenseIsOsiApproved Spencer_86 = False
licenseIsOsiApproved Spencer_94 = False
licenseIsOsiApproved Spencer_99 = False
licenseIsOsiApproved SMLNJ = False
licenseIsOsiApproved SugarCRM_1_1_3 = False
licenseIsOsiApproved SISSL = True
licenseIsOsiApproved SISSL_1_2 = False
licenseIsOsiApproved SPL_1_0 = True
licenseIsOsiApproved Watcom_1_0 = True
licenseIsOsiApproved TCL = False
licenseIsOsiApproved TCP_wrappers = False
licenseIsOsiApproved Unlicense = False
licenseIsOsiApproved TMate = False
licenseIsOsiApproved TORQUE_1_1 = False
licenseIsOsiApproved TOSL = False
licenseIsOsiApproved Unicode_DFS_2015 = False
licenseIsOsiApproved Unicode_DFS_2016 = False
licenseIsOsiApproved Unicode_TOU = False
licenseIsOsiApproved UPL_1_0 = True
licenseIsOsiApproved NCSA = True
licenseIsOsiApproved Vim = False
licenseIsOsiApproved VOSTROM = False
licenseIsOsiApproved VSL_1_0 = True
licenseIsOsiApproved W3C_20150513 = False
licenseIsOsiApproved W3C_19980720 = False
licenseIsOsiApproved W3C = True
licenseIsOsiApproved Wsuipa = False
licenseIsOsiApproved Xnet = True
licenseIsOsiApproved X11 = False
licenseIsOsiApproved Xerox = False
licenseIsOsiApproved XFree86_1_1 = False
licenseIsOsiApproved Xinetd = False
licenseIsOsiApproved Xpp = False
licenseIsOsiApproved XSkat = False
licenseIsOsiApproved YPL_1_0 = False
licenseIsOsiApproved YPL_1_1 = False
licenseIsOsiApproved Zed = False
licenseIsOsiApproved Zend_2_0 = False
licenseIsOsiApproved Zimbra_1_3 = False
licenseIsOsiApproved Zimbra_1_4 = False
licenseIsOsiApproved Zlib = True
licenseIsOsiApproved Zlib_acknowledgement = False
licenseIsOsiApproved ZPL_1_1 = False
licenseIsOsiApproved ZPL_2_0 = True
licenseIsOsiApproved ZPL_2_1 = False

-------------------------------------------------------------------------------
-- Creation
-------------------------------------------------------------------------------

-- | Create a 'LicenseId' from a 'String'.
mkLicenseId :: String -> Maybe LicenseId
mkLicenseId s = Map.lookup s stringLookup

stringLookup :: Map String LicenseId
stringLookup = Map.fromList $ map (\i -> (licenseId i, i)) $ [minBound .. maxBound]
