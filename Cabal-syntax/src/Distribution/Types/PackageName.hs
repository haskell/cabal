{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}

module Distribution.Types.PackageName
  ( PackageName
  , unPackageName
  , mkPackageName
  , unPackageNameST
  , mkPackageNameST
  ) where

import Distribution.Compat.Prelude
import Distribution.Utils.ShortText
import Prelude ()

import Distribution.Parsec
import Distribution.Pretty
import Distribution.Types.Annotation
import qualified Text.Parsec as P
import qualified Text.PrettyPrint as Disp

-- | A package name.
--
-- Use 'mkPackageName' and 'unPackageName' to convert from/to a
-- 'String'.
--
-- This type is opaque since @Cabal-2.0@
--
-- @since 2.0.0.2
newtype PackageName = PackageName ShortText
  deriving (Generic, Read, Show, Eq, Ord, Data)

-- | Convert 'PackageName' to 'String'
unPackageName :: PackageName -> String
unPackageName (PackageName s) = fromShortText s

-- | @since 3.4.0.0
unPackageNameST :: PackageName -> ShortText
unPackageNameST (PackageName s) = s

-- | Construct a 'PackageName' from a 'String'
--
-- 'mkPackageName' is the inverse to 'unPackageName'
--
-- Note: No validations are performed to ensure that the resulting
-- 'PackageName' is valid
--
-- @since 2.0.0.2
mkPackageName :: String -> PackageName
mkPackageName = PackageName . toShortText

-- | Construct a 'PackageName' from a 'ShortText'
--
-- Note: No validations are performed to ensure that the resulting
-- 'PackageName' is valid
--
-- @since 3.4.0.0
mkPackageNameST :: ShortText -> PackageName
mkPackageNameST = PackageName

-- | 'mkPackageName'
--
-- @since 2.0.0.2
instance IsString PackageName where
  fromString = mkPackageName

instance Binary PackageName
instance Structured PackageName

instance Markable PackageName
instance Pretty PackageName where
  pretty = Disp.text . unPackageName
instance ExactPretty PackageName where
  exactPretty t0 x =
    let doc = pretty x
        t = unmarkTriviaTree x t0
    in  [DocAnn doc t]

instance Parsec PackageName where parsec = snd <$> exactParsec
instance ExactParsec PackageName where
  exactParsec = do
    pos <- getPosition
    x <- mkPackageName <$> parsecUnqualComponentName
    let t = fromNamedTrivia x [ExactPosition (Position (P.sourceLine pos) (P.sourceColumn pos))]
    pure (t, x)

instance NFData PackageName where
  rnf (PackageName pkg) = rnf pkg
