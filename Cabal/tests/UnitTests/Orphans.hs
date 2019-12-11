{-# LANGUAGE CPP                #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module UnitTests.Orphans where

#if !MIN_VERSION_base(4,7,0)
import GHC.Fingerprint (Fingerprint (..))

deriving instance Show Fingerprint
#endif
