{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module UnitTests.Distribution.Client.GenericInstances () where

import Network.URI
import GHC.Generics

deriving instance Generic URIAuth
