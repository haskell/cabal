{-# LANGUAGE CPP #-}
module SetupHelper (helperGhcVersion) where

-- | The __GLASGOW_HASKELL__ of the compiler that built this library. Used
-- from client's Setup.hs, so that is the build compiler.
helperGhcVersion :: Int
helperGhcVersion = __GLASGOW_HASKELL__
