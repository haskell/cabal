module Distribution.Types.AnnotatedGenericPackageDescription.Lens where

import Distribution.Compat.Lens
import Distribution.Compat.Prelude
import qualified Distribution.Types.AnnotatedGenericPackageDescription as T
import qualified Distribution.Types.GenericPackageDescription as T
import Prelude ()

unannotatedGpd :: Lens' T.AnnotatedGenericPackageDescription T.GenericPackageDescription
unannotatedGpd f s = fmap (\x -> s{T.unannotatedGpd = x}) (f (T.unannotatedGpd s))
