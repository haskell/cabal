{-# LANGUAGE DeriveDataTypeable #-}

module PackageTests.Options
    ( OptionEnableAllTests(..)
    ) where

import Data.Typeable (Typeable)

import Test.Tasty.Options (IsOption(..), flagCLParser, safeRead)

newtype OptionEnableAllTests = OptionEnableAllTests Bool
  deriving Typeable

instance IsOption OptionEnableAllTests where
  defaultValue   = OptionEnableAllTests False
  parseValue     = fmap OptionEnableAllTests . safeRead
  optionName     = return "enable-all-tests"
  optionHelp     = return "Enable all tests"
  optionCLParser = flagCLParser Nothing (OptionEnableAllTests True)
