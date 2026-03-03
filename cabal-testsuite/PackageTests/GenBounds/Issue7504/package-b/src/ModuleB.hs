module ModuleB (getEnhancedMessage) where

import ModuleA (getMessage)

-- | Return an enhanced message that uses ModuleA's functionality
getEnhancedMessage :: String
getEnhancedMessage = getMessage ++ " Enhanced by package-b!"
