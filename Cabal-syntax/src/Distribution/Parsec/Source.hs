{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

module Distribution.Parsec.Source
  ( PSource (..)
  , CabalFileSource (..)
  , InstalledPackageInfoSource (..)
  , renderCabalFileSource
  , renderInstalledPackageInfoSource
  ) where

import qualified Data.ByteString as BS
import Distribution.Compat.Prelude
import Prelude ()

-- | The source of a parse error
data PSource src
  = PKnownSource src
  | PUnknownSource
  deriving (Ord, Show, Generic, Functor)

newtype CabalFileSource
  = PCabalFile (FilePath, BS.ByteString)
  deriving (Ord, Show, Generic)

data InstalledPackageInfoSource
  = PInstalledPackageInfo
  deriving (Eq, Ord, Show, Generic)

renderCabalFileSource :: CabalFileSource -> String
renderCabalFileSource (PCabalFile (path, _)) = path

renderInstalledPackageInfoSource :: InstalledPackageInfoSource -> String
renderInstalledPackageInfoSource PInstalledPackageInfo = ""

instance Eq CabalFileSource where
  PCabalFile (path, _) == PCabalFile (path', _) = path == path'

instance Eq src => Eq (PSource src) where
  PKnownSource src == PKnownSource src' = src == src'
  PUnknownSource == PUnknownSource = True
  _ == _ = False

instance Binary src => Binary (PSource src)
instance NFData src => NFData (PSource src) where rnf = genericRnf
