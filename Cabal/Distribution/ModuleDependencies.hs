{-# LANGUAGE DeriveDataTypeable #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.ModuleName
-- Copyright   :  Sebastiaan Visser 2013
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Data type for Haskell module hierarchies.

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

module Distribution.ModuleDependencies (
        ModuleHierarchy (..),
        parse,
        allModules
  ) where

import Control.Applicative
import Data.Char (isSpace, isLower)
import Data.Data (Data)
import Data.Function (on)
import Data.List (isPrefixOf, stripPrefix, intercalate, groupBy, sortBy)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Ord (comparing)
import Data.Typeable (Typeable)
import Distribution.ModuleName (ModuleName, fromString)

-- | A module with a list of dependencies.
newtype ModuleHierarchy = ModuleHierarchy { unModuleHierarchy :: [(ModuleName, [ModuleName])] }
  deriving (Eq, Ord, Read, Show, Typeable, Data)

parse :: FilePath -> IO ModuleHierarchy
parse file = buildHierarchy . parseMakefile <$> readFile file

allModules :: ModuleHierarchy -> [ModuleName]
allModules = map fst . unModuleHierarchy

buildHierarchy :: [(ModuleName, ModuleName)] -> ModuleHierarchy
buildHierarchy = ModuleHierarchy
               . map (\xs -> (fst (head xs), map snd xs))
               . groupBy (equating fst)
               . sortBy (comparing fst)
        where equating = ((==) `on`)

parseMakefile :: String -> [(ModuleName, ModuleName)]
parseMakefile = map toModules
              . splitRule
              . noEmptyLine
              . noComments
              . lines

     where noComments  = filter (not . isPrefixOf "#")
           noEmptyLine = filter (not . all isSpace)
           splitRule   = mapMaybe onColon
           onColon   s = case break (== ':') s of
                           (_, "") -> Nothing
                           (a, _:b) -> Just (a, b)
           trim        = reverse . dropWhile isSpace . reverse . dropWhile isSpace
           toModules (a, b) = (toModule a, toModule b)
           toModule    = fromString
                       . intercalate "."
                       . map (dropExts ["hs", "hi", "o"])
                       . dropWhile (all isLower . take 1)
                       . split '/'
                       . trim
           dropExt ext s = fmap reverse $ stripPrefix (reverse ('.' : ext)) (reverse s)
           dropExts []     s = s
           dropExts (e:es) s = dropExts es s `fromMaybe` dropExt e s
           split x = go
             where go s = case break (== x) s of
                             (a, "")  -> [a]
                             (a, _:b) -> a : go b

