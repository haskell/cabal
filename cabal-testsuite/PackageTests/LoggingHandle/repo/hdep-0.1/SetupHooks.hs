module SetupHooks (setupHooks) where

import Distribution.Simple.SetupHooks
import System.IO (hFlush, stdout)

setupHooks :: SetupHooks
setupHooks =
  noSetupHooks
    { configureHooks =
        noConfigureHooks
          { preConfPackageHook = Just $ \pcpi -> do
              putStrLn "hdep: running pre-conf hook"
              hFlush stdout
              return (noPreConfPackageOutputs pcpi)
          }
    }
