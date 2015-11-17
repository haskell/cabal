module Main
       where

import Test.Tasty
import Test.Tasty.Options
import Data.Proxy
import Data.Typeable

import qualified UnitTests.Distribution.Solver.Modular.Internal.PSQ

tests :: TestTree
tests = testGroup "Unit Tests"
        [ testGroup "UnitTests.Distribution.Solver.Modular.Internal.PSQ"
                    UnitTests.Distribution.Solver.Modular.Internal.PSQ.tests
        ]

main :: IO ()
main = defaultMainWithIngredients
         (includingOptions extraOptions : defaultIngredients)
         tests

{-------------------------------------------------------------------------------
  Test options
-------------------------------------------------------------------------------}

extraOptions :: [OptionDescription]
extraOptions = [
    Option (Proxy :: Proxy OptionShowSolverLog)
  ]

newtype OptionShowSolverLog = OptionShowSolverLog Bool
  deriving Typeable

instance IsOption OptionShowSolverLog where
  defaultValue   = OptionShowSolverLog False
  parseValue     = fmap OptionShowSolverLog . safeRead
  optionName     = return "show-solver-log"
  optionHelp     = return "Show full log from the solver"
  optionCLParser = flagCLParser Nothing (OptionShowSolverLog True)
