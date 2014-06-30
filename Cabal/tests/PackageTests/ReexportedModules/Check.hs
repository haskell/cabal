module PackageTests.ReexportedModules.Check where

import Data.Version
import PackageTests.PackageTester
import System.FilePath
import Test.HUnit
import Data.Maybe
import Data.List
import Control.Monad
import Data.Char
import Text.ParserCombinators.ReadP

orFail :: String -> [(a, String)] -> a
orFail err r = case find (all isSpace . snd) r of
    Nothing -> error err
    Just (i, _) -> i

suite :: FilePath -> Test
suite ghcPath = TestCase $ do
    -- ToDo: Turn this into a utility function
    (_, _, xs) <- run Nothing ghcPath ["--info"]
    let compat = (>= Version [7,9] [])
               . orFail "could not parse version"
               . readP_to_S parseVersion
               . snd
               . fromJust
               . find ((=="Project version").fst)
               . orFail "could not parse ghc --info output"
               . reads
               $ xs
    when compat $ do
        let spec = PackageSpec ("PackageTests" </> "ReexportedModules") []
        result <- cabal_build spec ghcPath
        assertBuildSucceeded result
