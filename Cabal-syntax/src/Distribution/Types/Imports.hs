{-# LANGUAGE TupleSections #-}

module Distribution.Types.Imports where

import qualified Data.Bifunctor as Bi

type WithImportNames a = ([String], a)

unImportNames :: WithImportNames a -> a
unImportNames = snd

importNames :: WithImportNames a -> [String]
importNames = fst

mapData :: (a -> b) -> WithImportNames a -> WithImportNames b
mapData = Bi.second

withImportNames :: [String] -> a -> WithImportNames a
withImportNames = (,)

withNoImports :: a -> WithImportNames a
withNoImports = ([],)
