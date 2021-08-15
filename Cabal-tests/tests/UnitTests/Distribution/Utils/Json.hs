{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
module UnitTests.Distribution.Utils.Json
    ( tests
    ) where

import Distribution.Utils.Json

import Test.Tasty
import Test.Tasty.HUnit

tests :: [TestTree]
tests =
    [ testCase "escapes strings correctly" $
      renderJson (JsonString "foo\"bar") @?= "\"foo\\\"bar\""
    , testCase "renders empty list" $
      renderJson (JsonArray []) @?= "[]"
    , testCase "renders singleton list" $
      renderJson (JsonArray [JsonString "foo\"bar"]) @?= "[\"foo\\\"bar\"]"
    , testCase "renders list" $
      renderJson (JsonArray [JsonString "foo\"bar", JsonString "baz"]) @?= "[\"foo\\\"bar\",\"baz\"]"
    , testCase "renders empty object" $
      renderJson (JsonObject []) @?= "{}"
    , testCase "renders singleton object" $
      renderJson (JsonObject [("key", JsonString "foo\"bar")]) @?= "{\"key\":\"foo\\\"bar\"}"
    , testCase "renders object" $
      renderJson (JsonObject
                    [ ("key", JsonString "foo\"bar")
                    , ("key2", JsonString "baz")])
          @?= "{\"key\":\"foo\\\"bar\",\"key2\":\"baz\"}"
    , testCase "renders number" $
      renderJson (JsonNumber 0) @?= "0"
    , testCase "renders negative number" $
      renderJson (JsonNumber (-1)) @?= "-1"
    , testCase "renders big number" $
      renderJson (JsonNumber 5000000) @?= "5000000"
    , testCase "renders bool" $ do
      renderJson (JsonBool True) @?= "true"
      renderJson (JsonBool False) @?= "false"
    , testCase "renders null" $ do
      renderJson JsonNull @?= "null"
    ]
