module Distribution.Simple ( defaultMainWithSetupHooks ) where

import System.Exit ( exitFailure )
import Distribution.Simple.SetupHooks ( SetupHooks )

defaultMainWithSetupHooks :: SetupHooks -> IO ()
defaultMainWithSetupHooks _ = do
  putStrLn "Chosen GOOD Cabal"
  exitFailure
  -- NB: we call 'exitFailure' here so that cabal-install, who is invoking
  -- this code thinking it is building the package, doesn't try to proceed
  -- when actually we haven't done anything (e.g. try to read a package
  -- description file).
