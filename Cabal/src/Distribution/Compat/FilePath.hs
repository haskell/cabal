{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Distribution.Compat.FilePath
  ( isExtensionOf
  , stripExtension
  ) where

import Data.List (isSuffixOf, stripPrefix)
import System.FilePath

#if !MIN_VERSION_filepath(1,4,2)
isExtensionOf :: String -> FilePath -> Bool
isExtensionOf ext@('.':_) = isSuffixOf ext . takeExtensions
isExtensionOf ext         = isSuffixOf ('.':ext) . takeExtensions
#endif

#if !MIN_VERSION_filepath(1,4,1)
stripExtension :: String -> FilePath -> Maybe FilePath
stripExtension []        path = Just path
stripExtension ext@(x:_) path = stripSuffix dotExt path
 where
  dotExt = if isExtSeparator x then ext else '.':ext
  stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
  stripSuffix xs ys = fmap reverse $ stripPrefix (reverse xs) (reverse ys)
#endif
