import Test.Cabal.Prelude

-- Check that hsc2hs is re-run when the .hsc file changes, and that the newly
-- generated module is used. This was broken in #11810, where a stale
-- preprocessor output in the build directory was mistaken for a source file.
main = setupAndCabalTest $ do
  writeSourceFile "Foo.hsc" (hscModule 1)
  setup "configure" []
  setup "build" []
  runExe' "my-executable" [] >>= assertOutputContains "hsc2hs value: 1"
  delay
  writeSourceFile "Foo.hsc" (hscModule 2)
  setup "build" []
  runExe' "my-executable" [] >>= assertOutputContains "hsc2hs value: 2"
  where
    hscModule :: Int -> String
    hscModule n =
      unlines
        [ "module Foo where"
        , ""
        , "foo :: Int"
        , "foo = #{const " ++ show n ++ "}"
        ]
