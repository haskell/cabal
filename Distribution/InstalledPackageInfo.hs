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
	emptyInstalledPackageInfo,
	parseInstalledPackageInfo,
	showInstalledPackageInfo,
	showInstalledPackageInfoField,
  ) where

import Distribution.ParseUtils (
	StanzaField(..), singleStanza, PError(..),
	simpleField, listField, licenseField,
	parseFilePath, parseLibName, parseModuleName,
	showFilePath, parseReadS, parseOptVersion )
import Distribution.License 	( License(..) )
import Distribution.Extension 	( Opt )
import Distribution.Package	( PackageIdentifier(..), showPackageId,
				  parsePackageName, parsePackageId )
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

noVersion = Version{ versionBranch=[], versionTags=[] }

-- -----------------------------------------------------------------------------
-- Parsing

parseInstalledPackageInfo :: String -> Either PError InstalledPackageInfo
parseInstalledPackageInfo inp = do
  lines <- singleStanza inp
	-- not interested in stanzas, so just allow blank lines in
	-- the package info.
  foldM (parseBasicStanza fields) emptyInstalledPackageInfo lines

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
    ppFields ((StanzaField _ get _ _):flds) = get pkg $$ ppFields flds

showInstalledPackageInfoField
	:: String
	-> Maybe (InstalledPackageInfo -> String)
showInstalledPackageInfoField field
  = case [ get | (StanzaField f get _ _) <- fields, f == field ] of
	[]      -> Nothing
	(get:_) -> Just (render . get)

-- -----------------------------------------------------------------------------
-- Description of the fields, for parsing/printing

fields = basicStanzaFields ++ installedStanzaFields

basicStanzaFields :: [StanzaField InstalledPackageInfo]
basicStanzaFields =
 [ simpleField "name"
                           text                   parsePackageName
                           (pkgName . package)    (\name pkg -> pkg{package=(package pkg){pkgName=name}})
 , simpleField "version"
                           (text . showVersion)   parseOptVersion 
                           (pkgVersion . package) (\ver pkg -> pkg{package=(package pkg){pkgVersion=ver}})
 , licenseField "license" False
                           license                (\l pkg -> pkg{license=l})
 , licenseField "license-file" True
                           license                (\l pkg -> pkg{license=l})
 , simpleField "copyright"
                           text                   (munch (const True))
                           copyright              (\val pkg -> pkg{copyright=val})
 , simpleField "maintainer"
                           text                   (munch (const True))
                           maintainer             (\val pkg -> pkg{maintainer=val})
 , simpleField "stability"
                           text                   (munch (const True))
                           stability              (\val pkg -> pkg{stability=val})
 , simpleField "homepage"
                           text                   (munch (const True))
                           homepage               (\val pkg -> pkg{homepage=val})
 , simpleField "package-url"
                           text                   (munch (const True))
                           pkgUrl                 (\val pkg -> pkg{pkgUrl=val})
 , simpleField "description"
                           text                   (munch (const True))
                           description            (\val pkg -> pkg{description=val})
 , simpleField "category"
                           text                   (munch (const True))
                           category               (\val pkg -> pkg{category=val})
 , simpleField "author"
                           text                   (munch (const True))
                           author                 (\val pkg -> pkg{author=val})
 ]

installedStanzaFields :: [StanzaField InstalledPackageInfo]
installedStanzaFields = [
   simpleField "exposed"
	(text.show) 	   parseReadS
	exposed     	   (\val pkg -> pkg{exposed=val})
 , listField   "exposed-modules"
	text               parseModuleName
	exposedModules     (\xs    pkg -> pkg{exposedModules=xs})
 , listField   "hidden-modules"
	text               parseModuleName
	hiddenModules      (\xs    pkg -> pkg{hiddenModules=xs})
 , listField   "import-dirs"
	showFilePath       parseFilePath
	importDirs         (\xs pkg -> pkg{importDirs=xs})
 , listField   "library-dirs"
	showFilePath       parseFilePath
	libraryDirs        (\xs pkg -> pkg{libraryDirs=xs})
 , listField   "hs-libraries"
	showFilePath       parseLibName
	hsLibraries        (\xs pkg -> pkg{hsLibraries=xs})
 , listField   "extra-libs"
	text               parseLibName
	extraLibraries     (\xs pkg -> pkg{extraLibraries=xs})
 , listField   "include-dirs"
	showFilePath       parseFilePath
	includeDirs        (\xs pkg -> pkg{includeDirs=xs})
 , listField   "includes"
	showFilePath       parseFilePath
	includes           (\xs pkg -> pkg{includes=xs})
 , listField   "depends"
	(text.showPackageId)  parsePackageId
	depends            (\xs pkg -> pkg{depends=xs})
 , listField   "extra-hugs-opts"
	text		   parseFilePath
	extraHugsOpts      (\path  pkg -> pkg{extraHugsOpts=path})
 , listField   "extra-cc-opts"
	text		   parseFilePath
	extraCcOpts        (\path  pkg -> pkg{extraCcOpts=path})
 , listField   "extra-ld-opts"
	text		   parseFilePath
	extraLdOpts        (\path  pkg -> pkg{extraLdOpts=path})
 , listField   "framework-dirs"
	showFilePath       parseFilePath
	frameworkDirs      (\xs pkg -> pkg{frameworkDirs=xs})
 , listField   "extra-frameworks"
	showFilePath       parseFilePath
	extraFrameworks    (\xs pkg -> pkg{extraFrameworks=xs})
 , listField   "haddock-interfaces"
	showFilePath       parseFilePath
	haddockInterfaces  (\xs pkg -> pkg{haddockInterfaces=xs})
 , listField   "haddock-html"
	showFilePath       parseFilePath
	haddockHTMLs       (\xs pkg -> pkg{haddockHTMLs=xs})
 ]

