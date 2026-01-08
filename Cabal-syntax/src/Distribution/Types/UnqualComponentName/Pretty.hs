{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Distribution.Types.UnqualComponentName.Pretty
  ( UnqualComponentName
  , unUnqualComponentName
  , unUnqualComponentNameST
  , mkUnqualComponentName
  , packageNameToUnqualComponentName
  , unqualComponentNameToPackageName
  , combineNames
  ) where

import Distribution.Compat.Prelude
import Distribution.Utils.ShortText

import Distribution.Pretty
import Distribution.Types.PackageName.Internal

instance Pretty UnqualComponentName where
  pretty = showToken . unUnqualComponentName
