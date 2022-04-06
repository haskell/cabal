{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -freduction-depth=0 #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.TreeDiff.Instances.CabalLanguage () where

import Data.TreeDiff
import Language.Haskell.Extension (Extension, KnownExtension, Language)

-- These are big enums, so they are in separate file.
--
instance ToExpr Extension
instance ToExpr KnownExtension
instance ToExpr Language
