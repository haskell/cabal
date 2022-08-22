import Control.Concurrent (killThread, threadDelay, myThreadId)
import Control.Exception (finally)
import qualified System.Posix.Signals as Signal
import System.Exit (exitFailure)

main = do
  mainThreadId <- myThreadId
  Signal.installHandler Signal.sigTERM (Signal.Catch $ killThread mainThreadId) Nothing
  (do
    putStrLn "about to sleep"
    writeFile "exe.run" "up and running"
    threadDelay 10000000 -- 10s
    putStrLn "done sleeping")
    `finally` putStrLn "exiting"
