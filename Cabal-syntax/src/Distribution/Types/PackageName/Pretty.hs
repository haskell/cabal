{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Distribution.Types.PackageName.Pretty
  ( PackageName
  , unPackageName
  , mkPackageName
  , unPackageNameST
  , mkPackageNameST
  ) where

import Distribution.Compat.Prelude
import Distribution.Utils.ShortText
import Prelude ()

import Distribution.Types.PackageName.Internal

import Distribution.Pretty
import qualified Text.PrettyPrint as Disp

instance Pretty PackageName where
  pretty = Disp.text . unPackageName
