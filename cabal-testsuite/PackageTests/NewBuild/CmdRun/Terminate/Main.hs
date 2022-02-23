import Control.Concurrent (killThread, threadDelay, myThreadId)
import Control.Exception (finally)
import qualified System.Posix.Signals as Signal
import System.Exit (exitFailure)

main = do
  writeFile "exe.run" "up and running"
  mainThreadId <- myThreadId
  Signal.installHandler Signal.sigTERM (Signal.Catch $ killThread mainThreadId) Nothing
  sleep
    `finally` putStrLn "exiting"
  where
    sleep = do
      putStrLn "about to sleep"
      threadDelay 10000000 -- 10s
      putStrLn "done sleeping"
