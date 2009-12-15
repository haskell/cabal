-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Compiler
-- Copyright   :  Isaac Jones 2003-2004
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This has an enumeration of the various compilers that Cabal knows about. It
-- also specifies the default compiler. Sadly you'll often see code that does
-- case analysis on this compiler flavour enumeration like:
--
-- > case compilerFlavor comp of
-- >   GHC -> GHC.getInstalledPackages verbosity packageDb progconf
-- >   JHC -> JHC.getInstalledPackages verbosity packageDb progconf
--
-- Obviously it would be better to use the proper 'Compiler' abstraction
-- because that would keep all the compiler-specific code together.
-- Unfortunately we cannot make this change yet without breaking the
-- 'UserHooks' api, which would break all custom @Setup.hs@ files, so for the
-- moment we just have to live with this deficiency. If you're interested, see
-- ticket #50.

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

module Distribution.Compiler (
  -- * Compiler flavor
  CompilerFlavor(..),
  buildCompilerFlavor,
  defaultCompilerFlavor,
  parseCompilerFlavorCompat,

  -- * Compiler id
  CompilerId(..),
  ) where

import Distribution.Version (Version(..))

import qualified System.Info (compilerName)
import Distribution.Text (Text(..), display)
import qualified Distribution.Compat.ReadP as Parse
import qualified Text.PrettyPrint as Disp
import Text.PrettyPrint ((<>))

import qualified Data.Char as Char (toLower, isDigit, isAlphaNum)
import Control.Monad (when)

data CompilerFlavor = GHC | NHC | YHC | Hugs | HBC | Helium | JHC | LHC | UHC
                    | OtherCompiler String
  deriving (Show, Read, Eq, Ord)

knownCompilerFlavors :: [CompilerFlavor]
knownCompilerFlavors = [GHC, NHC, YHC, Hugs, HBC, Helium, JHC, LHC, UHC]

instance Text CompilerFlavor where
  disp (OtherCompiler name) = Disp.text name
  disp NHC                  = Disp.text "nhc98"
  disp other                = Disp.text (lowercase (show other))

  parse = do
    comp <- Parse.munch1 Char.isAlphaNum
    when (all Char.isDigit comp) Parse.pfail
    return (classifyCompilerFlavor comp)

classifyCompilerFlavor :: String -> CompilerFlavor
classifyCompilerFlavor s =
  case lookup (lowercase s) compilerMap of
    Just compiler -> compiler
    Nothing       -> OtherCompiler s
  where
    compilerMap = [ (display compiler, compiler)
                  | compiler <- knownCompilerFlavors ]


--TODO: In some future release, remove 'parseCompilerFlavorCompat' and use
-- ordinary 'parse'. Also add ("nhc", NHC) to the above 'compilerMap'.

-- | Like 'classifyCompilerFlavor' but compatible with the old ReadS parser.
--
-- It is compatible in the sense that it accepts only the same strings,
-- eg "GHC" but not "ghc". However other strings get mapped to 'OtherCompiler'.
-- The point of this is that we do not allow extra valid values that would
-- upset older Cabal versions that had a stricter parser however we cope with
-- new values more gracefully so that we'll be able to introduce new value in
-- future without breaking things so much.
--
parseCompilerFlavorCompat :: Parse.ReadP r CompilerFlavor
parseCompilerFlavorCompat = do
  comp <- Parse.munch1 Char.isAlphaNum
  when (all Char.isDigit comp) Parse.pfail
  case lookup comp compilerMap of
    Just compiler -> return compiler
    Nothing       -> return (OtherCompiler comp)
  where
    compilerMap = [ (show compiler, compiler)
                  | compiler <- knownCompilerFlavors
                  , compiler /= YHC ]

buildCompilerFlavor :: CompilerFlavor
buildCompilerFlavor = classifyCompilerFlavor System.Info.compilerName

-- | The default compiler flavour to pick when compiling stuff. This defaults
-- to the compiler used to build the Cabal lib.
--
-- However if it's not a recognised compiler then it's 'Nothing' and the user
-- will have to specify which compiler they want.
--
defaultCompilerFlavor :: Maybe CompilerFlavor
defaultCompilerFlavor = case buildCompilerFlavor of
  OtherCompiler _ -> Nothing
  _               -> Just buildCompilerFlavor

-- ------------------------------------------------------------
-- * Compiler Id
-- ------------------------------------------------------------

data CompilerId = CompilerId CompilerFlavor Version
  deriving (Eq, Ord, Read, Show)

instance Text CompilerId where
  disp (CompilerId f (Version [] _)) = disp f
  disp (CompilerId f v) = disp f <> Disp.char '-' <> disp v

  parse = do
    flavour <- parse
    version <- (Parse.char '-' >> parse) Parse.<++ return (Version [] [])
    return (CompilerId flavour version)

lowercase :: String -> String
lowercase = map Char.toLower
