module Distribution.Client.Types.Credentials (
    Username (..),
    Password (..),
) where

newtype Username = Username { unUsername :: String }
newtype Password = Password { unPassword :: String }
