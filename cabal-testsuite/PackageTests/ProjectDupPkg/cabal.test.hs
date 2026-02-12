import Test.Cabal.Prelude
import Data.List (isInfixOf)

-- output contains filepaths into /tmp, so we only match parts of the output
main = cabalTest . recordMode DoNotRecord $ do
      liftIO $ skipIfWindows "\\r\\n confused with \\n"

      let msg = unlines
            [ "cabal project has multiple sources for pkg-one-0.1:"
            , "  .*/pkg-one"
            , "  .*/pkg-two"
            , "the choice of source that will be used is undefined."
            ]

      normal <- cabal' "configure" ["-v1", "pkg-one"]
      assertOutputMatches msg normal

      quiet <- cabal' "configure" ["-v0", "pkg-one"]
      assertOutputDoesNotMatch msg quiet

      return ()
