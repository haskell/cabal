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
        , testCase "Not find spurious imports" $
            getImports "import Data.Word\n{# import Data.Char #}" @?= Right ["Data.Char"]
        , testCase "Work with qualified imports + spaces" $
            getImports "{# import qualified   Data.Char #}" @?= Right ["Data.Char"]
        , testCase "Error on bad block comments" $
            getImports "{- {- block comment -} {# import Data.Char #}" @?= Left "Error in nested comment at line 1, column 46"
        ]
