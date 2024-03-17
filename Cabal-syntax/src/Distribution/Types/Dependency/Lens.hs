module Distribution.Types.Dependency.Lens
  ( private_depends
  ) where

import Distribution.Compat.Lens
import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Types.Dependency (Dependency, PrivateDependency)

import qualified Distribution.Types.Dependency as T

private_depends :: Lens' PrivateDependency [Dependency]
private_depends f d = fmap (\x -> d{T.private_depends = x}) (f (T.private_depends d))
