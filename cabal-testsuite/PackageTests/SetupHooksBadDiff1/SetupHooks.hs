module SetupHooks where

import Distribution.Simple.SetupHooks

import Control.Monad ( void )

setupHooks :: SetupHooks
setupHooks =
  noSetupHooks
    { configureHooks =
        noConfigureHooks
          { preConfComponentHook = Just pccHook }
    }

pccHook :: PreConfComponentHook
pccHook _ = return $
  PreConfComponentOutputs $ ComponentDiff $ CExe emptyExecutable
    -- Bad: component is a library, but we returned an executable!
