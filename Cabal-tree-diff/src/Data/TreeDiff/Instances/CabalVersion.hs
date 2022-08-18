{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -freduction-depth=0 #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.TreeDiff.Instances.CabalVersion where

import Data.TreeDiff
import Distribution.Version (Version, VersionRange, versionNumbers)

instance ToExpr Version where toExpr v = App "mkVersion" [toExpr $ versionNumbers v]
instance ToExpr VersionRange
