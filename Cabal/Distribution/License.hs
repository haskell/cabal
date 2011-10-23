-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.License
-- Copyright   :  Isaac Jones 2003-2005
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
-- includes 'GPL', 'LGPL' and 'BSD3' licenses.

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

    -- | Lesser GPL, Less restrictive than GPL, useful for libraries.
  | LGPL (Maybe Version)

    -- | 3-clause BSD license, newer, no advertising clause. Very free license.
  | BSD3

    -- | 4-clause BSD license, older, with advertising clause. You almost
    -- certainly want to use the BSD3 license instead.
  | BSD4

    -- | The MIT license, similar to the BSD3. Very free license.
  | MIT

    -- | Holder makes no claim to ownership, least restrictive license.
  | PublicDomain

    -- | No rights are granted to others. Undistributable. Most restrictive.
  | AllRightsReserved

    -- | Some other license.
  | OtherLicense

    -- | Not a recognised license.
    -- Allows us to deal with future extensions more gracefully.
  | UnknownLicense String
  deriving (Read, Show, Eq)

knownLicenses :: [License]
knownLicenses = [ GPL  unversioned, GPL  (version [2]),   GPL  (version [3])
                , LGPL unversioned, LGPL (version [2,1]), LGPL (version [3])
                , BSD3, MIT
                , PublicDomain, AllRightsReserved, OtherLicense]
 where
   unversioned = Nothing
   version   v = Just (Version v [])

instance Text License where
  disp (GPL  version)         = Disp.text "GPL"  <> dispOptVersion version
  disp (LGPL version)         = Disp.text "LGPL" <> dispOptVersion version
  disp (UnknownLicense other) = Disp.text other
  disp other                  = Disp.text (show other)

  parse = do
    name    <- Parse.munch1 (\c -> Char.isAlphaNum c && c /= '-')
    version <- Parse.option Nothing (Parse.char '-' >> fmap Just parse)
    return $! case (name, version :: Maybe Version) of
      ("GPL",               _      ) -> GPL  version
      ("LGPL",              _      ) -> LGPL version
      ("BSD3",              Nothing) -> BSD3
      ("BSD4",              Nothing) -> BSD4
      ("MIT",               Nothing) -> MIT
      ("PublicDomain",      Nothing) -> PublicDomain
      ("AllRightsReserved", Nothing) -> AllRightsReserved
      ("OtherLicense",      Nothing) -> OtherLicense
      _                              -> UnknownLicense $ name
                                     ++ maybe "" (('-':) . display) version

dispOptVersion :: Maybe Version -> Disp.Doc
dispOptVersion Nothing  = Disp.empty
dispOptVersion (Just v) = Disp.char '-' <> disp v
