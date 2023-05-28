{-# LANGUAGE DeriveDataTypeable #-}

module UnitTests.Options
  ( OptionShowSolverLog (..)
  , OptionMtimeChangeDelay (..)
  , RunNetworkTests (..)
  , extraOptions
  )
where

import Data.Proxy
import Data.Typeable

import Test.Tasty.Options

{-------------------------------------------------------------------------------
  Test options
-------------------------------------------------------------------------------}

extraOptions :: [OptionDescription]
extraOptions =
  [ Option (Proxy :: Proxy OptionShowSolverLog)
  , Option (Proxy :: Proxy OptionMtimeChangeDelay)
  , Option (Proxy :: Proxy RunNetworkTests)
  ]

newtype OptionShowSolverLog = OptionShowSolverLog Bool
  deriving (Typeable)

instance IsOption OptionShowSolverLog where
  defaultValue = OptionShowSolverLog False
  parseValue = fmap OptionShowSolverLog . safeReadBool
  optionName = return "show-solver-log"
  optionHelp = return "Show full log from the solver"
  optionCLParser = flagCLParser Nothing (OptionShowSolverLog True)

newtype OptionMtimeChangeDelay = OptionMtimeChangeDelay Int
  deriving (Typeable)

instance IsOption OptionMtimeChangeDelay where
  defaultValue = OptionMtimeChangeDelay 0
  parseValue = fmap OptionMtimeChangeDelay . safeRead
  optionName = return "mtime-change-delay"
  optionHelp =
    return $
      "How long to wait before attempting to detect"
        ++ "file modification, in microseconds"

newtype RunNetworkTests = RunNetworkTests Bool
  deriving (Typeable)

instance IsOption RunNetworkTests where
  defaultValue = RunNetworkTests True
  parseValue = fmap RunNetworkTests . safeReadBool
  optionName = return "run-network-tests"
  optionHelp = return "Run tests that need network access (default true)."
