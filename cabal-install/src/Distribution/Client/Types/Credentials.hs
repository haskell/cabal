module Distribution.Client.Types.Credentials
  ( Auth
  , Token (..)
  , Username (..)
  , Password (..)
  ) where

import Prelude (String, Either)

-- | Either (username, password) or authentacation token
type Auth = Either (String, String) String

newtype Token = Token {unToken :: String}
newtype Username = Username {unUsername :: String}
newtype Password = Password {unPassword :: String}
