-- This test-suite verifies some custom-setup scripts compile ok
-- so we don't break them by accident, i.e. when breakage can be prevented.
module Main (main) where
import CabalDoctestSetup ()
import IdrisSetup ()

main :: IO ()
main = return ()
