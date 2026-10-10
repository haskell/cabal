-- | Check that the nix-style commands classify every option they could
-- inherit from the v1 option lists: each one is either taken or deliberately
-- left out, and the names in those lists are spelt as the options are.
module UnitTests.Distribution.Client.NixStyleOptions (tests) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Distribution.Client.NixStyleOptions
  ( configureOptionNames
  , excludedConfigureOptionNames
  , excludedInstallOptionNames
  , installOptionNames
  , isProgramOptionName
  )
import Distribution.Client.Setup (configureOptions, installOptions)
import Distribution.Simple.Command (OptionField (optionName), ShowOrParseArgs (..))

import Test.Tasty
import Test.Tasty.HUnit

tests :: [TestTree]
tests =
  [ testGroup "configureOptions" $
      classified
        (map optionName . configureOptions)
        configureOptionNames
        excludedConfigureOptionNames
        isProgramOptionName
  , testGroup "installOptions" $
      classified
        (map optionName . installOptions)
        installOptionNames
        excludedInstallOptionNames
        (const False)
  ]

-- | Every name in the source list is taken or excluded, and never both;
-- every listed name exists in the source, so a renamed or removed v1 option
-- cannot leave a stale name behind.
--
-- The source differs between showing and parsing: shown, it has the
-- @--with-PROG@ placeholders; parsed, it has one option per known program
-- and some options that are accepted but not shown, such as @--only@.
classified :: (ShowOrParseArgs -> [String]) -> [String] -> [String] -> (String -> Bool) -> [TestTree]
classified source taken excluded byShape =
  [ testCase "every option shown is classified" $ covered (source ShowArgs)
  , testCase "every option parsed is classified" $ covered (source ParseArgs)
  , testCase "no option is both taken and excluded" $
      assertEqual "" [] [n | n <- taken, n `elem` excluded]
  , testCase "no excluded option is matched by shape" $
      assertEqual "" [] [n | n <- excluded, byShape n]
  , testCase "every taken name is an option" $
      assertEqual "" [] [n | n <- taken, n `notElem` known]
  , testCase "every excluded name is an option" $
      assertEqual "" [] [n | n <- excluded, n `notElem` known]
  ]
  where
    known = source ShowArgs ++ source ParseArgs
    covered names =
      assertEqual "" [] [n | n <- names, n `notElem` taken, n `notElem` excluded, not (byShape n)]
