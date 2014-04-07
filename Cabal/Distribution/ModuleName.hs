{-# LANGUAGE DeriveDataTypeable #-}
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

import Data.Data (Data)
import Data.Typeable (Typeable)
import qualified Distribution.Compat.ReadP as Parse
import qualified Text.PrettyPrint as Disp
import qualified Data.Char as Char
         ( isAlphaNum, isUpper )
import System.FilePath
         ( pathSeparator )
import Data.List
         ( intercalate, intersperse )

-- | A valid Haskell module name.
--
newtype ModuleName = ModuleName [String]
  deriving (Eq, Ord, Read, Show, Typeable, Data)

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
toFilePath = intercalate [pathSeparator] . components
