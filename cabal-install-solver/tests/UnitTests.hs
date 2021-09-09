module Main (main) where


import Test.Tasty

import qualified UnitTests.Distribution.Solver.Modular.MessageUtils

main :: IO ()
main = defaultMain $ testGroup "Unit Tests"
    [ testGroup "UnitTests.Distribution.Solver.Modular.MessageUtils"
          UnitTests.Distribution.Solver.Modular.MessageUtils.tests
    ]
