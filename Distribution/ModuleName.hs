-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.ModuleName
-- Copyright   :  Duncan Coutts 2008
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Data type for Haskell module names.

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

module Distribution.ModuleName (
        ModuleName,
        fromString,
        components,
        toFilePath,
        main,
        simple,
  ) where

import Distribution.Text
         ( Text(..) )

import qualified Distribution.Compat.ReadP as Parse
import qualified Text.PrettyPrint as Disp
import qualified Data.Char as Char
         ( isAlphaNum, isUpper )
import System.FilePath
         ( pathSeparator )
import Data.List
         ( intersperse )

-- | A valid Haskell module name.
--
newtype ModuleName = ModuleName [String]
  deriving (Eq, Ord, Read, Show)

instance Text ModuleName where
  disp (ModuleName ms) =
    Disp.hcat (intersperse (Disp.char '.') (map Disp.text ms))

  parse = do
    ms <- Parse.sepBy1 component (Parse.char '.')
    return (ModuleName ms)

    where
      component = do
        c  <- Parse.satisfy Char.isUpper
        cs <- Parse.munch validModuleChar
        return (c:cs)

validModuleChar :: Char -> Bool
validModuleChar c = Char.isAlphaNum c || c == '_' || c == '\''

validModuleComponent :: String -> Bool
validModuleComponent []     = False
validModuleComponent (c:cs) = Char.isUpper c
                           && all validModuleChar cs

{-# DEPRECATED simple "use ModuleName.fromString instead" #-}
simple :: String -> ModuleName
simple str = ModuleName [str]

-- | Construct a 'ModuleName' from a valid module name 'String'.
--
-- This is just a convenience function intended for valid module strings. It is
-- an error if it is used with a string that is not a valid module name. If you
-- are parsing user input then use 'Distribution.Text.simpleParse' instead.
--
fromString :: String -> ModuleName
fromString string
  | all validModuleComponent components' = ModuleName components'
  | otherwise                            = error badName

  where
    components' = split string
    badName     = "ModuleName.fromString: invalid module name " ++ show string

    split cs = case break (=='.') cs of
      (chunk,[])     -> chunk : []
      (chunk,_:rest) -> chunk : split rest

-- | The module name @Main@.
--
main :: ModuleName
main = ModuleName ["Main"]

-- | The individual components of a hierarchical module name. For example
--
-- > components (fromString "A.B.C") = ["A", "B", "C"]
--
components :: ModuleName -> [String]
components (ModuleName ms) = ms

-- | Convert a module name to a file path, but without any file extension.
-- For example:
--
-- > toFilePath (fromString "A.B.C") = "A/B/C"
--
toFilePath :: ModuleName -> FilePath
toFilePath = concat . intersperse [pathSeparator] . components
