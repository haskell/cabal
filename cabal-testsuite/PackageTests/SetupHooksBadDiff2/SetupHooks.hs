{-# LANGUAGE OverloadedStrings #-}

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
  -- Make invalid changes to a library
  PreConfComponentOutputs $ ComponentDiff $ CLib $
    emptyLibrary
      { libName = LSubLibName "hocus-pocus"
      , libExposed = False
      , libBuildInfo =
          emptyBuildInfo
            { buildable = False }
      }
