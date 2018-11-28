{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
module Distribution.Types.MungedPackageId
  ( MungedPackageId(..)
  , computeCompatPackageId
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Parsec.Class
import Distribution.Pretty
import Distribution.Types.MungedPackageName
import Distribution.Types.PackageId
import Distribution.Types.UnqualComponentName
import Distribution.Version                   (Version, nullVersion)

import qualified Distribution.Compat.CharParsing as P
import qualified Text.PrettyPrint                as Disp

-- | A simple pair of a 'MungedPackageName' and 'Version'. 'MungedPackageName' is to
-- 'MungedPackageId' as 'PackageName' is to 'PackageId'. See 'MungedPackageName' for more
-- info.
data MungedPackageId
    = MungedPackageId {
        -- | The combined package and component name. see documentation for
        -- 'MungedPackageName'.
        mungedName    :: MungedPackageName,
        -- | The version of this package / component, eg 1.2
        mungedVersion :: Version
     }
     deriving (Generic, Read, Show, Eq, Ord, Typeable, Data)

instance Binary MungedPackageId

instance Pretty MungedPackageId where
    pretty (MungedPackageId n v)
        | v == nullVersion = pretty n -- if no version, don't show version.
        | otherwise        = pretty n <<>> Disp.char '-' <<>> pretty v

instance Parsec MungedPackageId where
    parsec = do
        n <- parsec
        v <- (P.char '-' >> parsec) <|> return nullVersion
        return (MungedPackageId n v)

instance NFData MungedPackageId where
    rnf (MungedPackageId name version) = rnf name `seq` rnf version

-- | See docs for 'Distribution.Types.MungedPackageName.computeCompatPackageId'. this
-- is a thin wrapper around that.
computeCompatPackageId :: PackageId -> Maybe UnqualComponentName -> MungedPackageId
computeCompatPackageId (PackageIdentifier pn vr) mb_uqn = MungedPackageId pn' vr
  where pn' = computeCompatPackageName pn mb_uqn
