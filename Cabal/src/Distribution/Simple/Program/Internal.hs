-----------------------------------------------------------------------------

-- |
-- Module      :  Distribution.Simple.Program.Internal
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Internal utilities used by Distribution.Simple.Program.*.
module Distribution.Simple.Program.Internal
  ( stripExtractVersion
  ) where

import Distribution.Compat.Prelude
import Distribution.Utils.Generic (safeTail)
import Prelude ()

-- | Extract the version number from the output of 'strip --version'.
--
-- Invoking "strip --version" gives very inconsistent results. We
-- ignore everything in parentheses (see #2497), look for the first
-- word that starts with a number, and try parsing out the first two
-- components of it. Non-GNU, non-LLVM 'strip' doesn't appear to have
-- a version flag.
stripExtractVersion :: String -> String
stripExtractVersion str =
  let numeric "" = False
      numeric (x : _) = isDigit x

      closingParentheses =
        [ ")"
        , -- LLVM strip outputs "llvm-strip, compatible with GNU strip\nLLVM (http://llvm.org/):\n..."
          "):"
        ]

      -- Filter out everything in parentheses.
      filterPar' :: Int -> [String] -> [String]
      filterPar' _ [] = []
      filterPar' n (x : xs)
        | n >= 0 && "(" `isPrefixOf` x = filterPar' (n + 1) ((safeTail x) : xs)
        | n > 0 && any (`isSuffixOf` x) closingParentheses = filterPar' (n - 1) xs
        | n > 0 = filterPar' n xs
        | otherwise = x : filterPar' n xs

      filterPar = filterPar' 0
   in case dropWhile (not . numeric) (filterPar . words $ str) of
        (ver : _) ->
          -- take the first two version components
          let isDot = (== '.')
              (major, rest) = break isDot ver
              minor = takeWhile isDigit (dropWhile isDot rest)
           in major ++ "." ++ minor
        _ -> ""
