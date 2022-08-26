import Control.Concurrent (killThread, threadDelay, myThreadId)
import Control.Exception (finally)
import qualified System.Posix.Signals as Signal
import System.Exit (exitFailure)
import qualified Data.Time.Clock as Time
import qualified Data.Time.Format as Time

main = do
  -- timestamped logging to aid with #8416
  let log msg = do
        ts <- Time.getCurrentTime
        let tsfmt = Time.formatTime Time.defaultTimeLocale "%H:%M:%S.%q" ts
        putStrLn $ tsfmt <> " [exe       ] " <> msg
  mainThreadId <- myThreadId
  Signal.installHandler Signal.sigTERM (Signal.Catch $ killThread mainThreadId) Nothing
  (do
    log "about to write file"
    writeFile "exe.run" "up and running"
    log "about to sleep"
    threadDelay 10000000 -- 10s
    log "done sleeping")
    `finally` log "exiting"
