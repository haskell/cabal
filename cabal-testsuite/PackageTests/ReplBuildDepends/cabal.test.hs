import Test.Cabal.Prelude

-- Test “repl --build-depends”
main = do
  testWithByteString "normal" []
  -- See https://github.com/haskell/cabal/issues/6859
  testWithByteString "allow-newer" ["--allow-newer"]
  -- See https://github.com/haskell/cabal/issues/6859
  testWithByteString "allow-older" ["--allow-older"]
  where
  testWithByteString label extraArgs = cabalTest' label $ do
    cabal' "clean" []
    res <- cabalWithStdin
        "repl"
        ("-v2" : "--build-depends" : "bytestring" : extraArgs)
        "import qualified Data.ByteString as BS"
    assertOutputContains "Ok, one module loaded." res
    -- Ensure we can load ‘bytestring’
    assertOutputDoesNotContain "Could not load" res
