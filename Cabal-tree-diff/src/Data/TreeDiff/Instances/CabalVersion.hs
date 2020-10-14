{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -freduction-depth=0 #-}
#else
{-# OPTIONS_GHC -fcontext-stack=151 #-}
#endif
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.TreeDiff.Instances.CabalVersion where

import Data.TreeDiff
import Distribution.Version (Version, VersionRange, versionNumbers)

instance ToExpr Version where toExpr v = App "mkVersion" [toExpr $ versionNumbers v]
instance ToExpr VersionRange
