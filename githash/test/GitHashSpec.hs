{-# LANGUAGE TemplateHaskell #-}
module GitHashSpec
  ( spec
  ) where

import GitHash
import System.Directory (doesDirectoryExist)
import Test.Hspec

spec :: Spec
spec = do
  describe "tGitInfoCwd" $ do
    it "makes vaguely sane git info for this repository" $ do
        let egi = $$tGitInfoCwdTry
        gitDirExists <- doesDirectoryExist ".git"
        case egi of
          Left _ -> gitDirExists `shouldBe` False
          Right gi -> do
            -- Doesn't work with cabal gitDirExists `shouldBe` True
            length (giHash gi)`shouldNotBe` 128
            giBranch gi `shouldNotBe` []
            seq (giDirty gi) () `shouldBe` ()
            giCommitDate gi `shouldNotBe` []
            giCommitCount gi `shouldSatisfy` (>= 1)
            giDescribe gi `shouldStartWith` "githash-"
