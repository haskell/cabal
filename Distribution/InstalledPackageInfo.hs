-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.InstalledPackageInfo
-- Copyright   :  (c) The University of Glasgow 2004
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  alpha
-- Portability :  portable
--
-- Explanation: Information on how to USE an installed package,
-- consumed by HC-PKG (ghc-pkg, for instance).  THIS MODULE IS NOT YET
-- USED.  HC-PKG is not yet implemented, and ghc-pkg has its own file
-- format, so this module is very much subject to change once HC-PKG
-- is implemented.


{- All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of the University nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. -}

-- This module is meant to be local-only to Distribution...

module Distribution.InstalledPackageInfo (
	InstalledPackageInfo(..),
	ParseResult(..),
	emptyInstalledPackageInfo,
	parseInstalledPackageInfo,
	showInstalledPackageInfo,
	showInstalledPackageInfoField,
  ) where

import Distribution.ParseUtils (
	StanzaField(..), singleStanza, ParseResult(..),
	simpleField, listField, parseLicenseQ,
	parseFilePathQ, parseLibNameQ, parseModuleNameQ, parsePackageNameQ,
	showFilePath, parseReadS, parseOptVersion, parseQuoted,
	showFreeText)
import Distribution.License 	( License(..) )
import Distribution.Extension 	( Opt )
import Distribution.Package	( PackageIdentifier(..), showPackageId,
				  parsePackageId )
import Distribution.Version	( Version(..), showVersion )
import Distribution.Compat.ReadP as ReadP

import Control.Monad	( foldM )
import Text.PrettyPrint

-- -----------------------------------------------------------------------------
-- The InstalledPackageInfo type

data InstalledPackageInfo
   = InstalledPackageInfo {
	-- these parts are exactly the same as PackageDescription
	package           :: PackageIdentifier,
        license           :: License,
        copyright         :: String,
        maintainer        :: String,
	author            :: String,
        stability         :: String,
	homepage          :: String,
	pkgUrl            :: String,
	description       :: String,
	category          :: String,
	-- these parts are required by an installed package only:
        exposed           :: Bool,
	exposedModules	  :: [String],
	hiddenModules     :: [String],
        importDirs        :: [FilePath],  -- contain sources in case of Hugs
        libraryDirs       :: [FilePath],
        hsLibraries       :: [String],
        extraLibraries    :: [String],
        includeDirs       :: [FilePath],
        includes          :: [String],
        depends           :: [PackageIdentifier],
        extraHugsOpts     :: [Opt],
        extraCcOpts       :: [Opt],
        extraLdOpts       :: [Opt],
        frameworkDirs     :: [FilePath],
        extraFrameworks   :: [String],
	haddockInterfaces :: [FilePath],
	haddockHTMLs      :: [FilePath]
    }
    deriving (Read, Show)

emptyInstalledPackageInfo :: InstalledPackageInfo
emptyInstalledPackageInfo
   = InstalledPackageInfo {
        package          = PackageIdentifier "" noVersion,
        license          = AllRightsReserved,
        copyright        = "",
        maintainer       = "",
	author		 = "",
        stability        = "",
	homepage	 = "",
	pkgUrl		 = "",
	description	 = "",
	category	 = "",
        exposed          = False,
	exposedModules	 = [],
	hiddenModules    = [],
        importDirs       = [],
        libraryDirs      = [],
        hsLibraries      = [],
        extraLibraries   = [],
        includeDirs      = [],
        includes	 = [],
        depends          = [],
        extraHugsOpts    = [],
        extraCcOpts      = [],
        extraLdOpts      = [],
        frameworkDirs    = [],
        extraFrameworks  = [],
	haddockInterfaces = [],
	haddockHTMLs      = []
    }

noVersion :: Version
noVersion = Version{ versionBranch=[], versionTags=[] }

-- -----------------------------------------------------------------------------
-- Parsing

parseInstalledPackageInfo :: String -> ParseResult InstalledPackageInfo
parseInstalledPackageInfo inp = do
  stLines <- singleStanza inp
	-- not interested in stanzas, so just allow blank lines in
	-- the package info.
  foldM (parseBasicStanza fields) emptyInstalledPackageInfo stLines

parseBasicStanza ((StanzaField name _ _ set):fields) pkg (lineNo, f, val)
  | name == f = set lineNo val pkg
  | otherwise = parseBasicStanza fields pkg (lineNo, f, val)
parseBasicStanza [] pkg (lineNo, f, val) = return pkg


-- -----------------------------------------------------------------------------
-- Pretty-printing

showInstalledPackageInfo :: InstalledPackageInfo -> String
showInstalledPackageInfo pkg = render (ppFields fields)
  where
    ppFields [] = empty
    ppFields ((StanzaField _ get' _ _):flds) = get' pkg $$ ppFields flds

showInstalledPackageInfoField
	:: String
	-> Maybe (InstalledPackageInfo -> String)
showInstalledPackageInfoField field
  = case [ get' | (StanzaField f get' _ _) <- fields, f == field ] of
	[]      -> Nothing
	(get':_) -> Just (render . get')

-- -----------------------------------------------------------------------------
-- Description of the fields, for parsing/printing

fields :: [StanzaField InstalledPackageInfo]
fields = basicStanzaFields ++ installedStanzaFields

basicStanzaFields :: [StanzaField InstalledPackageInfo]
basicStanzaFields =
 [ simpleField "name"
                           text                   parsePackageNameQ
                           (pkgName . package)    (\name pkg -> pkg{package=(package pkg){pkgName=name}})
 , simpleField "version"
                           (text . showVersion)   parseOptVersion 
                           (pkgVersion . package) (\ver pkg -> pkg{package=(package pkg){pkgVersion=ver}})
 , simpleField "license"
                           (text . show)          parseLicenseQ
                           license                (\l pkg -> pkg{license=l})
 , simpleField "copyright"
                           showFreeText           (munch (const True))
                           copyright              (\val pkg -> pkg{copyright=val})
 , simpleField "maintainer"
                           showFreeText           (munch (const True))
                           maintainer             (\val pkg -> pkg{maintainer=val})
 , simpleField "stability"
                           showFreeText           (munch (const True))
                           stability              (\val pkg -> pkg{stability=val})
 , simpleField "homepage"
                           showFreeText           (munch (const True))
                           homepage               (\val pkg -> pkg{homepage=val})
 , simpleField "package-url"
                           showFreeText           (munch (const True))
                           pkgUrl                 (\val pkg -> pkg{pkgUrl=val})
 , simpleField "description"
                           showFreeText           (munch (const True))
                           description            (\val pkg -> pkg{description=val})
 , simpleField "category"
                           showFreeText           (munch (const True))
                           category               (\val pkg -> pkg{category=val})
 , simpleField "author"
                           showFreeText           (munch (const True))
                           author                 (\val pkg -> pkg{author=val})
 ]

installedStanzaFields :: [StanzaField InstalledPackageInfo]
installedStanzaFields = [
   simpleField "exposed"
	(text.show) 	   parseReadS
	exposed     	   (\val pkg -> pkg{exposed=val})
 , listField   "exposed-modules"
	text               parseModuleNameQ
	exposedModules     (\xs    pkg -> pkg{exposedModules=xs})
 , listField   "hidden-modules"
	text               parseModuleNameQ
	hiddenModules      (\xs    pkg -> pkg{hiddenModules=xs})
 , listField   "import-dirs"
	showFilePath       parseFilePathQ
	importDirs         (\xs pkg -> pkg{importDirs=xs})
 , listField   "library-dirs"
	showFilePath       parseFilePathQ
	libraryDirs        (\xs pkg -> pkg{libraryDirs=xs})
 , listField   "hs-libraries"
	showFilePath       parseLibNameQ
	hsLibraries        (\xs pkg -> pkg{hsLibraries=xs})
 , listField   "extra-libs"
	text               parseLibNameQ
	extraLibraries     (\xs pkg -> pkg{extraLibraries=xs})
 , listField   "include-dirs"
	showFilePath       parseFilePathQ
	includeDirs        (\xs pkg -> pkg{includeDirs=xs})
 , listField   "includes"
	showFilePath       parseFilePathQ
	includes           (\xs pkg -> pkg{includes=xs})
 , listField   "depends"
	(text.showPackageId)  parsePackageId'
	depends            (\xs pkg -> pkg{depends=xs})
 , listField   "extra-hugs-opts"
	text		   parseFilePathQ
	extraHugsOpts      (\path  pkg -> pkg{extraHugsOpts=path})
 , listField   "extra-cc-opts"
	text		   parseFilePathQ
	extraCcOpts        (\path  pkg -> pkg{extraCcOpts=path})
 , listField   "extra-ld-opts"
	text		   parseFilePathQ
	extraLdOpts        (\path  pkg -> pkg{extraLdOpts=path})
 , listField   "framework-dirs"
	showFilePath       parseFilePathQ
	frameworkDirs      (\xs pkg -> pkg{frameworkDirs=xs})
 , listField   "extra-frameworks"
	showFilePath       parseFilePathQ
	extraFrameworks    (\xs pkg -> pkg{extraFrameworks=xs})
 , listField   "haddock-interfaces"
	showFilePath       parseFilePathQ
	haddockInterfaces  (\xs pkg -> pkg{haddockInterfaces=xs})
 , listField   "haddock-html"
	showFilePath       parseFilePathQ
	haddockHTMLs       (\xs pkg -> pkg{haddockHTMLs=xs})
 ]

parsePackageId' = parseQuoted parsePackageId <++ parsePackageId
