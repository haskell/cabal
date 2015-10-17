module Main where

import IndexUtils

import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.Simple.Utils (fromUTF8)

import System.Environment
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List
import Data.List.Split
import Data.Char

main = do
  [indexfile] <- getArgs
  cabalFiles <- IndexUtils.readPackageIndexFile indexfile

  print (length cabalFiles)

  let parse = parsePackageDescription . fromUTF8 . LBS.unpack
      pkgs  = [ packageDescription pkg
              | ParseOk _ pkg <- map parse cabalFiles ]
      latestPkgs = Map.fromListWith
                     (\a b -> if packageVersion a > packageVersion b
                                then a else b)
                     [ (packageName pkg, pkg) | pkg <- pkgs ]

  print (Map.size latestPkgs)

  let wordfreqs = Map.fromListWith (+)
                  [ (w,1)
                  | pkg <- Map.elems latestPkgs
                  , w   <- extractWords pkg ]
      mostfreq = take 100 $ sortBy (flip compare)
                   [ (f,w) | (w,f) <- Map.toList wordfreqs, f > 1 ]
      mostfreqSet = Set.fromList (map snd mostfreq)

  putStr $ unlines
    [ display pkg ++ ": " ++ intercalate ", " ws)
    | pkg <- Map.elems latestPkgs
    , let ws = filter (`Set.notMember` mostfreqSet) (extractWords pkg) ]

--  putStr $ unlines
--    [ show n ++ " " ++ show f ++ ": " ++ w
--    | ((f,w), n) <- zip (Set.toList mostfreq) [0..] ]

extractWords =
    noDups . map (map toLower) . split strategy . description
  where
    strategy = dropFinalBlank . dropInitBlank . dropDelims . condense
             $ oneOf " \n\t.,;:@|/\\&^%$!\"*()<>"

noDups = map head . group . sort
