{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -freduction-depth=0 #-}
#else
{-# OPTIONS_GHC -fcontext-stack=151 #-}
#endif
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Instances.TreeDiff.Version where

import Data.TreeDiff
import Distribution.Version (Version, VersionRange)

instance ToExpr Version where toExpr = defaultExprViaShow
instance ToExpr VersionRange
