module Distribution.Types.BuildInfo.Lens (
    BuildInfo,
    HasBuildInfo (..),
    ) where

import Prelude()
import Distribution.Compat.Prelude
import Distribution.Compat.Lens

import Distribution.Types.BuildInfo (BuildInfo)
import qualified Distribution.Types.BuildInfo as T

-- | Classy lenses for 'BuildInfo'.
class HasBuildInfo a where
    buildInfo :: Lens' a BuildInfo

    customFieldsBI :: Lens' a [(String,String)]
    customFieldsBI = buildInfo . customFieldsBI

    hsSourceDirs :: Lens' a [FilePath]
    hsSourceDirs = buildInfo . hsSourceDirs

instance HasBuildInfo BuildInfo where
    buildInfo = id

    customFieldsBI f bi = fmap (\x -> bi { T.customFieldsBI = x }) (f (T.customFieldsBI bi))
    hsSourceDirs   f bi = fmap (\x -> bi { T.hsSourceDirs   = x }) (f (T.hsSourceDirs bi))
