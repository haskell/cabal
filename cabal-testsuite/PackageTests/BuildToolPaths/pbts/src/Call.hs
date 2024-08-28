

module Call ( callCustomPp ) where

-- base
import System.Exit
  ( ExitCode(..) )

-- process
import System.Process
  ( readProcessWithExitCode )

--------------------------------------------------------------------------------

callCustomPp :: String -> IO ()
callCustomPp customPpName = do
  (exitCode, stdout, stderr) <- readProcessWithExitCode customPpName [] ""
  case exitCode of
    ExitSuccess ->
      putStr stdout
    ExitFailure {} ->
      error $ unlines
        [ customPpName ++ " failed with error code " ++ show exitCode
        , "stdout: " ++ stdout
        , "stderr: " ++ stderr ]

