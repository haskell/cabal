{-# LANGUAGE RecordWildCards #-}
module PackageTests.ReexportedModules.Check where

import Data.Version
import PackageTests.PackageTester
import System.FilePath
import Test.Tasty.HUnit
import Data.Maybe
import Data.List
import Control.Monad
import Data.Char
import Text.ParserCombinators.ReadP

orFail :: String -> [(a, String)] -> a
orFail err r = case find (all isSpace . snd) r of
    Nothing -> error err
    Just (i, _) -> i

find' :: (a -> Bool) -> [a] -> Maybe a
find' = find

suite :: IO TestsConfig -> Assertion
suite cfg = do
    TestsConfig{..} <- cfg
    -- ToDo: Turn this into a utility function
    (_, _, xs) <- run Nothing testsConfigGhcPath [] ["--info"]
    let compat = (>= Version [7,9] [])
               . orFail "could not parse version"
               . readP_to_S parseVersion
               . snd
               . fromJust
               . find' ((=="Project version").fst)
               . orFail "could not parse ghc --info output"
               . reads
               $ xs
    when compat $ do
        let spec = PackageSpec
                { directory = "PackageTests" </> "ReexportedModules"
                , configOpts = []
                , distPref = Nothing
                }
        result <- cabal_build cfg spec
        assertBuildSucceeded result
