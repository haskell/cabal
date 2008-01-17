module Hackage.Utils where

import Distribution.ParseUtils (showDependency)
import Distribution.Version (Dependency(..))

import Control.Exception
import Control.Monad (guard)
import qualified Data.Char as Char (toLower)
import Data.List (intersperse, isPrefixOf, tails)

import System.IO.Error (isDoesNotExistError)

readFileIfExists :: FilePath -> IO (Maybe String)
readFileIfExists path = 
    catchJust fileNotFoundExceptions 
                  (fmap Just (readFile path)) 
                  (\_ -> return Nothing)

fileNotFoundExceptions :: Exception -> Maybe IOError
fileNotFoundExceptions e = 
    ioErrors e >>= \ioe -> guard (isDoesNotExistError ioe) >> return ioe

showDependencies :: [Dependency] -> String
showDependencies = concat . intersperse ", " . map (show . showDependency)

equating :: Eq a => (b -> a) -> b -> b -> Bool
equating p x y = p x == p y

comparing :: Ord a => (b -> a) -> b -> b -> Ordering
comparing p x y = p x `compare` p y

isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)

intercalate :: [a] -> [[a]] -> [a]
intercalate sep = concat . intersperse sep

lowercase :: String -> String
lowercase = map Char.toLower
