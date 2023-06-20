module Distribution.Client.Types.Credentials
  ( Auth
  , Token (..)
  , Username (..)
  , Password (..)
  ) where

import Prelude (String)
import Data.Either (Either)

type Auth = Either (String, String) String

newtype Token = Token {unToken :: String}
newtype Username = Username {unUsername :: String}
newtype Password = Password {unPassword :: String}
