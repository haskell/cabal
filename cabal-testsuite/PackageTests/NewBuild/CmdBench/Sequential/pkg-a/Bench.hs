import Control.Concurrent (threadDelay)
import System.Directory (createDirectory, removeDirectory)
import System.Exit (die)
import System.IO.Error (catchIOError, isAlreadyExistsError)

-- All the benchmarks of this test, in both packages, claim the same marker
-- directory while they run. Creating it fails if it already exists, that is,
-- if another benchmark is running at the same time.
main :: IO ()
main = do
  createDirectory marker `catchIOError` \e ->
    if isAlreadyExistsError e
      then die "Another benchmark is running at the same time"
      else ioError e
  threadDelay 2000000
  removeDirectory marker
  where
    marker = "../benchmark-running"
