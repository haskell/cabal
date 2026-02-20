module UnitTests.Options
  ( OptionShowSolverLog (..)
  , OptionMtimeChangeDelay (..)
  , RunNetworkTests (..)
  , extraOptions
  )
where

import Data.Proxy

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

instance IsOption OptionShowSolverLog where
  defaultValue = OptionShowSolverLog False
  parseValue = fmap OptionShowSolverLog . safeReadBool
  optionName = return "show-solver-log"
  optionHelp = return "Show full log from the solver"
  optionCLParser = flagCLParser Nothing (OptionShowSolverLog True)

newtype OptionMtimeChangeDelay = OptionMtimeChangeDelay Int

instance IsOption OptionMtimeChangeDelay where
  defaultValue = OptionMtimeChangeDelay 10000
  showDefaultValue (OptionMtimeChangeDelay v) = Just (show v)
  parseValue = fmap OptionMtimeChangeDelay . safeRead
  optionName = return "mtime-change-delay"
  optionHelp =
    return $
      "How long to wait before attempting to detect"
        ++ "file modification, in microseconds"

newtype RunNetworkTests = RunNetworkTests Bool

instance IsOption RunNetworkTests where
  defaultValue = RunNetworkTests True
  parseValue = fmap RunNetworkTests . safeReadBool
  optionName = return "run-network-tests"
  optionHelp = return "Run tests that need network access (default true)."
