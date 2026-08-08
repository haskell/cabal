module UnitTests.Options
  ( OptionMtimeChangeDelay (..)
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
  [ Option (Proxy :: Proxy OptionMtimeChangeDelay)
  ]

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
