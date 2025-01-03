{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -freduction-depth=0 #-}

module Data.TreeDiff.Instances.CabalVersion where

import Data.TreeDiff
import Distribution.Version (Version, VersionRange, versionNumbers)

instance ToExpr Version where toExpr v = App "mkVersion" [toExpr $ versionNumbers v]
instance ToExpr VersionRange
