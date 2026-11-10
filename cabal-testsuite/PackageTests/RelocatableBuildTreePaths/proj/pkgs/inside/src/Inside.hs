module Inside (inside) where

import External (external)

inside :: String
inside = "inside/" ++ external
