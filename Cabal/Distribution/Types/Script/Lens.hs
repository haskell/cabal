module Distribution.Types.Script.Lens (
    Script,
    module Distribution.Types.Script.Lens,
    ) where

import Distribution.Compat.Lens
import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Types.Executable          (Executable)
import Distribution.Types.Script              (Script)

import qualified Distribution.Types.Script as T

executable :: Lens' Script Executable
executable f s = fmap (\x -> s { T.executable = x }) (f (T.executable s))
{-# INLINE executable #-}

hcPath :: Lens' Script (Maybe FilePath)
hcPath f s = fmap (\x -> s { T.hcPath = x }) (f (T.hcPath s))
{-# INLINE hcPath #-}
