{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}

module Distribution.Parsec.Source where

import Distribution.Compat.Prelude
import qualified Data.ByteString as BS
import Prelude ()


data PSource src = PKnownSource src
                 | PUnknownSource
                 deriving (Ord, Show, Generic, Functor)

data CabalParserSource = PCabalFile (FilePath, BS.ByteString)
                        | PInstalledPackageInfo
                        deriving (Ord, Show, Generic)

renderCabalParserSource :: CabalParserSource -> String
renderCabalParserSource PInstalledPackageInfo = ""
renderCabalParserSource (PCabalFile (path, _)) = path

instance Eq CabalParserSource where
  PInstalledPackageInfo == PInstalledPackageInfo = True
  PCabalFile (path, _) == PCabalFile (path', _) = path == path'
  _ == _ = False

instance Eq src => Eq (PSource src) where
  PKnownSource src == PKnownSource src' = src == src'
  PUnknownSource == PUnknownSource = True
  _ == _ = False

instance Binary src =>Binary (PSource src)
instance NFData src => NFData (PSource src) where rnf = genericRnf