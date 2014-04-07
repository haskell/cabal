{-# LANGUAGE DeriveDataTypeable #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.License
-- Copyright   :  Isaac Jones 2003-2005
-- License     :  BSD3
--                Duncan Coutts 2008
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- The License datatype.  For more information about these and other
-- open-source licenses, you may visit <http://www.opensource.org/>.
--
-- The @.cabal@ file allows you to specify a license file. Of course you can
-- use any license you like but people often pick common open source licenses
-- and it's useful if we can automatically recognise that (eg so we can display
-- it on the hackage web pages). So you can also specify the license itself in
-- the @.cabal@ file from a short enumeration defined in this module. It
-- includes 'GPL', 'AGPL', 'LGPL', 'Apache 2.0', 'MIT' and 'BSD3' licenses.

module Distribution.License (
    License(..),
    knownLicenses,
  ) where

import Distribution.Version (Version(Version))

import Distribution.Text (Text(..), display)
import qualified Distribution.Compat.ReadP as Parse
import qualified Text.PrettyPrint as Disp
import Text.PrettyPrint ((<>))
import qualified Data.Char as Char (isAlphaNum)
import Data.Data (Data)
import Data.Typeable (Typeable)

-- |This datatype indicates the license under which your package is
-- released.  It is also wise to add your license to each source file
-- using the license-file field.  The 'AllRightsReserved' constructor
-- is not actually a license, but states that you are not giving
-- anyone else a license to use or distribute your work.  The comments
-- below are general guidelines.  Please read the licenses themselves
-- and consult a lawyer if you are unsure of your rights to release
-- the software.
--
data License =

--TODO: * remove BSD4

    -- | GNU Public License. Source code must accompany alterations.
    GPL (Maybe Version)

    -- | GNU Affero General Public License
  | AGPL (Maybe Version)

    -- | Lesser GPL, Less restrictive than GPL, useful for libraries.
  | LGPL (Maybe Version)

    -- | 2-clause BSD license, used by FreeBSD, et al. Omits non-endorsement
    -- clause.
  | BSD2

    -- | 3-clause BSD license, newer, no advertising clause. Very free license.
  | BSD3

    -- | 4-clause BSD license, older, with advertising clause. You almost
    -- certainly want to use the BSD3 license instead.
  | BSD4

    -- | The MIT license, similar to the BSD3. Very free license.
  | MIT

    -- | Mozilla Public License, a weak copyleft license.
  | MPL Version

    -- | The Apache License. Version 2.0 is the current version,
    -- previous versions are considered historical.
  | Apache (Maybe Version)

    -- | Holder makes no claim to ownership, least restrictive license.
  | PublicDomain

    -- | No rights are granted to others. Undistributable. Most restrictive.
  | AllRightsReserved

    -- | Some other license.
  | OtherLicense

    -- | Not a recognised license.
    -- Allows us to deal with future extensions more gracefully.
  | UnknownLicense String
  deriving (Read, Show, Eq, Typeable, Data)

knownLicenses :: [License]
knownLicenses = [ GPL  unversioned, GPL  (version [2]),    GPL  (version [3])
                , LGPL unversioned, LGPL (version [2, 1]), LGPL (version [3])
                , AGPL unversioned,                        AGPL (version [3])
                , BSD2, BSD3, MIT
                , MPL (Version [2, 0] [])
                , Apache unversioned, Apache (version [2, 0])
                , PublicDomain, AllRightsReserved, OtherLicense]
 where
   unversioned = Nothing
   version   v = Just (Version v [])

instance Text License where
  disp (GPL  version)         = Disp.text "GPL"    <> dispOptVersion version
  disp (LGPL version)         = Disp.text "LGPL"   <> dispOptVersion version
  disp (AGPL version)         = Disp.text "AGPL"   <> dispOptVersion version
  disp (MPL  version)         = Disp.text "MPL"    <> dispVersion    version
  disp (Apache version)       = Disp.text "Apache" <> dispOptVersion version
  disp (UnknownLicense other) = Disp.text other
  disp other                  = Disp.text (show other)

  parse = do
    name    <- Parse.munch1 (\c -> Char.isAlphaNum c && c /= '-')
    version <- Parse.option Nothing (Parse.char '-' >> fmap Just parse)
    return $! case (name, version :: Maybe Version) of
      ("GPL",               _      ) -> GPL  version
      ("LGPL",              _      ) -> LGPL version
      ("AGPL",              _      ) -> AGPL version
      ("BSD2",              Nothing) -> BSD2
      ("BSD3",              Nothing) -> BSD3
      ("BSD4",              Nothing) -> BSD4
      ("MIT",               Nothing) -> MIT
      ("MPL",         Just version') -> MPL version'
      ("Apache",            _      ) -> Apache version
      ("PublicDomain",      Nothing) -> PublicDomain
      ("AllRightsReserved", Nothing) -> AllRightsReserved
      ("OtherLicense",      Nothing) -> OtherLicense
      _                              -> UnknownLicense $ name ++
                                        maybe "" (('-':) . display) version

dispOptVersion :: Maybe Version -> Disp.Doc
dispOptVersion Nothing  = Disp.empty
dispOptVersion (Just v) = dispVersion v

dispVersion :: Version -> Disp.Doc
dispVersion v = Disp.char '-' <> disp v
