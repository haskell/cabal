module Distribution.Types.PackageDescription.Lens (
    PackageDescription,
    module Distribution.Types.PackageDescription.Lens,
    ) where

import Prelude()
import Distribution.Compat.Prelude
import Distribution.Compat.Lens

import Distribution.Types.PackageDescription (PackageDescription)
import qualified Distribution.Types.PackageDescription as T

customFieldsPD :: Lens' PackageDescription [(String,String)]
customFieldsPD f pd =
    fmap (\x -> pd { T.customFieldsPD = x }) (f (T.customFieldsPD pd))
