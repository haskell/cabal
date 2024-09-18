{-# LANGUAGE NoImplicitPrelude #-}

module SetupHooks ( setupHooks ) where

import Distribution.Simple.SetupHooks ( SetupHooks, noSetupHooks )

setupHooks :: SetupHooks
setupHooks = noSetupHooks
