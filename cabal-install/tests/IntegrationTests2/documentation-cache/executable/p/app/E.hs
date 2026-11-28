module E (message) where

import P (value)

-- | The message printed by the executable.
message :: String
message = show value
