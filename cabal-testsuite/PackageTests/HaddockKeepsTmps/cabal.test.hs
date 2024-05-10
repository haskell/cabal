{-# LANGUAGE LambdaCase #-}
import Test.Cabal.Prelude
import Distribution.Verbosity
import Distribution.Simple.Glob
import Distribution.Simple.Glob.Internal

-- Test that "cabal haddock" preserves temporary files
-- We use haddock-keep-temp-file: True in the cabal.project.
main = cabalTest $ recordMode DoNotRecord $ do
  skipIfWindows

  cabal "haddock" []

  cwd <- fmap testCurrentDir getTestEnv

  (globMatches <$> liftIO (runDirFileGlob silent Nothing cwd (GlobDirRecursive [WildCard, Literal "response", WildCard, Literal "txt"]))) >>= \case
    [] -> error "Expecting a response file to exist"
    (m:_) ->
      -- Assert the matched response file is not empty.
      assertFileDoesContain (cwd </> m) "Simple.hs"
