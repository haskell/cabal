{-# LANGUAGE NoImplicitPrelude #-}

module FancyHooks ( setupHooks ) where

import Distribution.Simple.SetupHooks ( SetupHooks(..) )

setupHooks :: SetupHooks
setupHooks = SetupHooks
