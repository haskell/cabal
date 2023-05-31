module Distribution.Client.Types.Credentials
  ( Username (..)
  , Password (..)
  ) where

import Prelude (String)

newtype Username = Username {unUsername :: String}
newtype Password = Password {unPassword :: String}
