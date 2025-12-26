{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module WorktreeRepoSpec
    ( spec
    ) where

import Control.Monad
import qualified Data.ByteString as SB
import GitHash
import System.Directory
import System.FilePath
import System.Process
import Test.Hspec
import UnliftIO.Temporary

spec :: Spec
spec =
    around setupGitRepo $ do
        describe "getGitInfo" $ do
            it "it makes sensible git info for a git-worktree repository" $ \fp -> do
                errOrGi <- getGitInfo fp
                case errOrGi of
                    Left err -> expectationFailure $ show err
                    Right gi -> do
                        length (giHash gi) `shouldNotBe` 128
                        giBranch gi `shouldBe` "worktree-branch"
                        giDirty gi `shouldBe` True
                        giCommitDate gi `shouldNotBe` []
                        giCommitCount gi `shouldBe` 1
                        giCommitMessage gi `shouldBe` "Initial commit"
                        length (giDescribe gi) `shouldBe` 7
        describe "getGitRoot" $ do
            it "it gets the expected git root for a git-worktree repository" $ \fp ->
                getGitRoot fp `shouldReturn` Right fp

setupGitRepo :: (FilePath -> IO ()) -> IO ()
setupGitRepo runTest =
    withSystemTempDirectory "normal" $ \fp -> do
        let fp1 = fp </> "1"
            fp2 = fp </> "2"
        createDirectoryIfMissing True fp1
        createDirectoryIfMissing True fp2
        let runGit args =
                void $ readCreateProcess ((proc "git" args) {cwd = Just fp1}) ""
        runGit ["init"]
        SB.writeFile
            (fp1 </> "README.md")
            "This is a readme, you should read it."
        runGit ["add", "README.md"]
        runGit
            [ "-c"
            , "user.name='Test User'"
            , "-c"
            , "user.email='test@example.com'"
            , "commit"
            , "-m"
            , "Initial commit"
            ]
        runGit ["branch", "worktree-branch"]
        runGit ["worktree", "add", fp2, "worktree-branch"]
        SB.writeFile
            (fp2 </> "README.md")
            "This is a readme that has been modified."
        runTest fp2
