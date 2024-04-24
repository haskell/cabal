{-# LANGUAGE TemplateHaskell #-}

module Main ( main ) where

-- template-haskell
import Language.Haskell.TH
  ( runIO )

-- pbts
import Call
  ( callCustomPp )

--------------------------------------------------------------------------------

-- Check that we can invoke the custom preprocessor, and that it finds its
-- data directory, both at compile-time and at run-time.

$( do
    runIO $ do
      callCustomPp "custom-pp1"
      callCustomPp "custom-pp2"
    return []
  )

main :: IO ()
main = do
  callCustomPp "custom-pp1"
  callCustomPp "custom-pp2"
