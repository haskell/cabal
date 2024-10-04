{-# LANGUAGE LambdaCase #-}
import Test.Cabal.Prelude
import Data.List (sort, isPrefixOf)
import Distribution.Verbosity
import Distribution.Simple.Glob
import Distribution.Simple.Glob.Internal
import Distribution.Simple.Utils
import System.Directory

-- Test that "cabal haddock" preserves temporary files
-- We use haddock-keep-temp-file: True in the cabal.project.
main =
  cabalTest $ recordMode DoNotRecord $ withProjectFile "cabal.project" $ do
      pwd <- testTmpDir <$> getTestEnv
      liftIO $ createDirectory (pwd </> "temp")
      withEnv [(if isWindows then "TMP" else "TMPDIR", Just $ pwd </> "temp")] $
        cabal "haddock" []
      files <- liftIO $ listDirectory (pwd </> "temp")
      case [ pwd </> "temp" </> f | f <- files, takeExtension f == ".txt" ] of
        [] -> error "Expecting a response file being mentioned in the outcome"
        files' ->
          -- Assert the matched response file is not empty, and indeed a haddock rsp
          assertAnyFileContains files' "--package-name"
