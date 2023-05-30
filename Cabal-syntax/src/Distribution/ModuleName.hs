{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------

-- |
-- Module      :  Distribution.ModuleName
-- Copyright   :  Duncan Coutts 2008
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Data type for Haskell module names.
module Distribution.ModuleName
  ( ModuleName
  , fromString
  , fromComponents
  , components
  , toFilePath
  , main

    -- * Internal
  , validModuleComponent
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Parsec
import Distribution.Pretty
import Distribution.Utils.ShortText (ShortText, fromShortText, toShortText)
import System.FilePath (pathSeparator)

import qualified Distribution.Compat.CharParsing as P
import qualified Distribution.Compat.DList as DList
import qualified Text.PrettyPrint as Disp

-- | A valid Haskell module name.
newtype ModuleName = ModuleName ShortText
  deriving (Eq, Generic, Ord, Read, Show, Typeable, Data)

unModuleName :: ModuleName -> String
unModuleName (ModuleName s) = fromShortText s

instance Binary ModuleName
instance Structured ModuleName

instance NFData ModuleName where
  rnf (ModuleName ms) = rnf ms

instance Pretty ModuleName where
  pretty = Disp.text . unModuleName

instance Parsec ModuleName where
  parsec = parsecModuleName

parsecModuleName :: forall m. CabalParsing m => m ModuleName
parsecModuleName = state0 DList.empty
  where
    upper :: m Char
    !upper = P.satisfy isUpper

    ch :: m Char
    !ch = P.satisfy (\c -> validModuleChar c || c == '.')

    alt :: m ModuleName -> m ModuleName -> m ModuleName
    !alt = (<|>)

    state0 :: DList.DList Char -> m ModuleName
    state0 acc = do
      c <- upper
      state1 (DList.snoc acc c)

    state1 :: DList.DList Char -> m ModuleName
    state1 acc = state1' acc `alt` return (fromString (DList.toList acc))

    state1' :: DList.DList Char -> m ModuleName
    state1' acc = do
      c <- ch
      case c of
        '.' -> state0 (DList.snoc acc c)
        _ -> state1 (DList.snoc acc c)

validModuleChar :: Char -> Bool
validModuleChar c = isAlphaNum c || c == '_' || c == '\''

validModuleComponent :: String -> Bool
validModuleComponent [] = False
validModuleComponent (c : cs) = isUpper c && all validModuleChar cs

-- | Construct a 'ModuleName' from a valid module name 'String'.
--
-- This is just a convenience function intended for valid module strings. It is
-- an error if it is used with a string that is not a valid module name. If you
-- are parsing user input then use 'Distribution.Text.simpleParse' instead.
instance IsString ModuleName where
  fromString = ModuleName . toShortText

-- | Construct a 'ModuleName' from valid module components, i.e. parts
-- separated by dots.
fromComponents :: [String] -> ModuleName
fromComponents comps = fromString (intercalate "." comps)
{-# DEPRECATED fromComponents "Exists for cabal-install only" #-}

-- | The module name @Main@.
main :: ModuleName
main = ModuleName (fromString "Main")

-- | The individual components of a hierarchical module name. For example
--
-- > components (fromString "A.B.C") = ["A", "B", "C"]
components :: ModuleName -> [String]
components mn = split (unModuleName mn)
  where
    split cs = case break (== '.') cs of
      (chunk, []) -> chunk : []
      (chunk, _ : rest) -> chunk : split rest

-- | Convert a module name to a file path, but without any file extension.
-- For example:
--
-- > toFilePath (fromString "A.B.C") = "A/B/C"
toFilePath :: ModuleName -> FilePath
toFilePath = map f . unModuleName
  where
    f '.' = pathSeparator
    f c = c
