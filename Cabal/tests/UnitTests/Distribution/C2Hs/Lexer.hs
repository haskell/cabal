module UnitTests.Distribution.C2Hs.Lexer ( tests ) where

import Distribution.C2Hs.Lexer

import Test.Tasty
import Test.Tasty.HUnit

tests :: [TestTree]
tests = [ testCase "simple import" $
            getImports "{# import Data.Char #}" @?= Right ["Data.Char"]
        , testCase "line comment" $
            getImports "-- {# import Data.Char #}" @?= Right []
        , testCase "nested block comment" $
            getImports "{- nested {- comment -} -} {# import Data.Char #}" @?= Right ["Data.Char"]
        ]
