{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -freduction-depth=0 #-}
#else
{-# OPTIONS_GHC -fcontext-stack=151 #-}
#endif
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Instances.TreeDiff.Language where

import Data.TreeDiff
import Language.Haskell.Extension (Extension, KnownExtension, Language)

-- This are big enums, so they are in separate file.
--
instance ToExpr Extension
instance ToExpr KnownExtension
instance ToExpr Language
