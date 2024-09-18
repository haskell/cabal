{-# LANGUAGE LambdaCase #-}
import Test.Cabal.Prelude
import Data.List (sort)
import Distribution.Verbosity
import Distribution.Simple.Glob
import Distribution.Simple.Glob.Internal
import Distribution.Simple.Utils

-- Test that "cabal haddock" preserves temporary files
-- We use haddock-keep-temp-file: True in the cabal.project.
main = cabalTest $ recordMode DoNotRecord $ withProjectFile "cabal.project" $ do
    cabal "haddock" []

    cwd <- fmap testCurrentDir getTestEnv

    -- Windows has multiple response files, and only the last one (alphabetically) is the important one.
    (safeLast . sort . globMatches <$> liftIO (runDirFileGlob silent Nothing cwd (GlobDirRecursive [WildCard, Literal "txt"]))) >>= \case
      Nothing -> error "Expecting a response file to exist"
      Just m -> do
        -- Assert the matched response file is not empty, and indeed a haddock rsp
        assertFileDoesContain (cwd </> m) "--package-name"
