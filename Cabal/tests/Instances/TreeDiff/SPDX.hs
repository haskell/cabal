{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -freduction-depth=0 #-}
#else
{-# OPTIONS_GHC -fcontext-stack=151 #-}
#endif
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Instances.TreeDiff.SPDX where

import Data.TreeDiff
import Distribution.License (License)

import Instances.TreeDiff.Version ()

import qualified Distribution.SPDX as SPDX

-- 'License' almost belongs here.

instance ToExpr License

-- Generics instance is too heavy
instance ToExpr SPDX.LicenseId where toExpr = defaultExprViaShow
instance ToExpr SPDX.LicenseExceptionId where toExpr = defaultExprViaShow

instance ToExpr SPDX.License
instance ToExpr SPDX.LicenseExpression
instance ToExpr SPDX.LicenseRef
instance ToExpr SPDX.SimpleLicenseExpression
