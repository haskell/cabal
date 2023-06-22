import System.Exit (ExitCode (ExitFailure), exitWith)

main = exitWith $ ExitFailure 42
