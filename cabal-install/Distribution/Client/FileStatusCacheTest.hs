import System.IO
import Distribution.Client.FileStatusCache
import Control.Monad.Reader
import System.FilePath
import System.Directory
import System.Posix.Temp

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

type TestM = ReaderT FilePath IO

touch :: FilePath -> TestM ()
touch fname = do
  root <- ask
  liftIO $ withFile (root</>fname) WriteMode $ \_->return ()

write :: FilePath -> String -> TestM ()
write fname contents = do
  root <- ask
  liftIO $ writeFile (root</>fname) contents

getCacheName :: TestM FilePath
getCacheName = (++".cache") <$> ask

assertChanged :: Bool -> TestM ()
assertChanged expectChange = do
  res <- checkChanged
  case res of
    Changed | not expectChange -> error "it was not supposed to change"
    Unchanged _ | expectChange -> error "it was supposed to change"
    _                          -> return ()

checkChanged :: TestM (Changed ())
checkChanged = do
  cname <- getCacheName
  root <- ask
  liftIO $ checkFileStatusChanged cname root

updateCache :: [FileSpec] -> TestM ()
updateCache specs = do
  cname <- getCacheName
  root <- ask
  liftIO $ updateFileStatusCache cname root specs ()

test :: String -> TestM a -> IO a
test name action = do
  putStrLn name
  root <- mkdtemp "filestatus"
  res <- runReaderT action root
  removeDirectoryRecursive root
  return res
