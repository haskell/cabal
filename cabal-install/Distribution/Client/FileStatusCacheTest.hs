{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Distribution.Text (simpleParse)
import Control.Concurrent (threadDelay)
import Control.Monad.Reader
import Control.Monad.Trans.Except
import Control.Monad.Error.Class
import System.FilePath
import System.Directory
import System.Posix.Temp
import Control.Exception

import Distribution.Client.FileStatusCache
import Distribution.Client.Glob

main :: IO ()
main = do
  test "changed if no cache" $ do
    touch "a"
    assertChanged True

  test "no change if empty cache" $ do
    updateCache []
    touch "a"
    assertChanged False

  test "add file" $ do
    updateCache [SingleFile "a"]
    touch "a"
    assertChanged True

  test "add file in directory" $ do
    updateCache [SingleFile "dir1/a"]
    touch "dir1/a"
    assertChanged True

  test "remove file" $ do
    touch "a"
    updateCache [SingleFile "a"]
    remove "a"
    assertChanged True

  test "remove directory" $ do
    touch "dir1/a"
    updateCache [SingleFile "dir1/a"]
    liftIO . removeDirectoryRecursive =<< getPath "dir1"
    assertChanged True

  let glob = [GlobHashPath $ GlobDir (Glob [Literal "dir1"])
                           $ GlobFile (Glob [Literal "good-", WildCard])]
  test "glob add match" $ do
    touch "dir1/good-a"
    --updateCache [globHashPath "dir1/good*"]
    updateCache glob
    touch "dir1/good-b"
    assertChanged True

  test "glob add nested match" $ do
    touch "dir1/dir2/good-a"
    --updateCache [globHashPath "dir1/good*"]
    updateCache glob
    touch "dir1/dir2/good-b"
    --assertChanged True -- FIXME

  test "glob add non-match" $ do
    touch "dir1/good-a"
    --updateCache [globHashPath "dir1/good*"]
    updateCache glob
    touch "dir1/bad"
    assertChanged False

  test "glob remove match" $ do
    touch "dir1/good-a"
    --updateCache [globHashPath "dir1/good*"]
    updateCache glob
    remove "dir1/good-a"
    assertChanged True

  test "glob remove non-match" $ do
    touch "dir1/good-a"
    touch "dir1/bad"
    --updateCache [globHashPath "dir1/good*"]
    updateCache glob
    remove "dir1/bad"
    assertChanged False

globHashPath :: String -> FileSpec
globHashPath glob
  | Just glob' <- simpleParse glob = GlobHashPath glob'
  | otherwise                      = error $ "Failed to parse "++glob

newtype TestM a = TestM { runTestM :: ExceptT String (ReaderT FilePath IO) a }
                deriving (Functor, Applicative, Monad, MonadIO, MonadError String)

getRoot :: TestM FilePath
getRoot = TestM $ lift ask

getPath :: FilePath -> TestM FilePath
getPath fname = (</>) <$> getRoot <*> pure fname

touch :: FilePath -> TestM ()
touch fname = write fname "hello"

remove :: FilePath -> TestM ()
remove fname = do
  file <- getPath fname
  liftIO $ removeFile file

write :: FilePath -> String -> TestM ()
write fname contents = do
  path <- (</> fname) <$> getRoot
  liftIO $ createDirectoryIfMissing True (takeDirectory path)
  liftIO $ writeFile path contents
  liftIO $ threadDelay 10000

getCacheName :: TestM FilePath
getCacheName = (++".cache") <$> getRoot

expectChanged, expectUnchanged :: TestM ()
expectChanged = assertChanged True
expectUnchanged = assertChanged False

assertChanged :: Bool -> TestM ()
assertChanged expectChange = do
  res <- checkChanged
  case res of
    Changed | not expectChange -> throwError "expected no change"
    Unchanged _ | expectChange -> throwError "expected change"
    _                          -> return ()

checkChanged :: TestM (Changed ())
checkChanged = do
  cname <- getCacheName
  root <- getRoot
  liftIO $ checkFileStatusChanged cname root

updateCache :: [FileSpec] -> TestM ()
updateCache specs = do
  cname <- getCacheName
  root <- getRoot
  liftIO $ updateFileStatusCache cname root specs ()

test :: String -> TestM a -> IO ()
test name action = do
  putStr $ (take 50 $ name++repeat ' ') ++ "  "
  let --clean root = print root
      clean root = do
        removeDirectoryRecursive root
        let cacheFile = root++".cache"
        exists <- doesFileExist cacheFile
        when exists $ removeFile cacheFile
  res <- bracket (mkdtemp "file-status-")
                 clean
                 (\root -> runReaderT (runExceptT $ runTestM action) root)
  case res of
    Left e -> putStrLn ("fail, "++e)
    Right _ -> putStrLn "ok"
