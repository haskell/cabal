-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Package
-- Copyright   :  Isaac Jones 2003-2004
-- 
-- Maintainer  :  Isaac Jones <ijones@syntaxpolice.org>
-- Stability   :  alpha
-- Portability :  portable
--
-- Packages are fundamentally just a name and a version.

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

    * Neither the name of Isaac Jones nor the names of other
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

module Distribution.Package (
	-- * Package ids
	PackageIdentifier(..),
	showPackageId, parsePackageId, parsePackageName,

        -- * Package dependencies
        Dependency(..),
        showDependency, parseDependency,

	-- * Package classes
	Package(..), packageName, packageVersion,
	PackageFixedDeps(..),
  ) where

import Distribution.Version
import Distribution.Compat.ReadP as ReadP
import qualified Text.PrettyPrint as Pretty
import Text.PrettyPrint ((<+>))
import Data.Char ( isDigit, isAlphaNum )
import Data.List ( intersperse )

-- | The name and version of a package.
data PackageIdentifier
    = PackageIdentifier {
	pkgName    :: String, -- ^The name of this package, eg. foo
	pkgVersion :: Version -- ^the version of this package, eg 1.2
     }
     deriving (Read, Show, Eq, Ord)

-- |Creates a string like foo-1.2
showPackageId :: PackageIdentifier -> String
showPackageId (PackageIdentifier n (Version [] _)) = n -- if no version, don't show version.
showPackageId pkgid = 
  pkgName pkgid ++ '-': showVersion (packageVersion pkgid)

parsePackageName :: ReadP r String
parsePackageName = do ns <- sepBy1 component (char '-')
                      return (concat (intersperse "-" ns))
  where component = do 
	   cs <- munch1 isAlphaNum
	   if all isDigit cs then pfail else return cs
	-- each component must contain an alphabetic character, to avoid
	-- ambiguity in identifiers like foo-1 (the 1 is the version number).

-- |A package ID looks like foo-1.2.
parsePackageId :: ReadP r PackageIdentifier
parsePackageId = do 
  n <- parsePackageName
  v <- (ReadP.char '-' >> parseVersion) <++ return (Version [] [])
  return PackageIdentifier{pkgName=n,pkgVersion=v}

-- ------------------------------------------------------------
-- * Package dependencies
-- ------------------------------------------------------------

data Dependency = Dependency String VersionRange
                  deriving (Read, Show, Eq)

showDependency :: Dependency -> Pretty.Doc
showDependency (Dependency name ver) =
  Pretty.text name <+> Pretty.text (showVersionRange ver)

parseDependency :: ReadP r Dependency
parseDependency = do name <- parsePackageName
                     skipSpaces
                     ver <- parseVersionRange <++ return AnyVersion
                     skipSpaces
                     return $ Dependency name ver

-- | Class of things that can be identified by a 'PackageIdentifier'
--
-- Types in this class are all notions of a package. This allows us to have
-- different types for the different phases that packages go though, from
-- simple name\/id, package description, configured or installed packages.
--
class Package pkg where
  packageId :: pkg -> PackageIdentifier

packageName    :: Package pkg => pkg -> String
packageName     = pkgName    . packageId

packageVersion :: Package pkg => pkg -> Version
packageVersion  = pkgVersion . packageId

instance Package PackageIdentifier where
  packageId = id

-- | Subclass of packages that have specific versioned dependencies.
--
-- So for example a not-yet-configured package has dependencies on version
-- ranges, not specific versions. A configured or an already installed package
-- depends on exact versions. Some operations or data structures (like
--  dependency graphs) only make sense on this subclass of package types.
--
class Package pkg => PackageFixedDeps pkg where
  depends :: pkg -> [PackageIdentifier]
