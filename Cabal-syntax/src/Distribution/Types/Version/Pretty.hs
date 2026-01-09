{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

module Distribution.Types.Version.Pretty
  ( -- * Package versions
    Version
  , mkVersion
  , mkVersion'
  , versionNumbers
  , nullVersion
  , alterVersion
  , version0

    -- * Internal
  , validVersion
  -- , versionDigitParser
  ) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Pretty

import Distribution.Types.Version.Internal
import Distribution.Types.AnnotationNamespace
import Distribution.Types.AnnotationTrivium

import qualified Data.Map as M

import qualified Data.Version as Base
import qualified Distribution.Compat.CharParsing as P
import qualified Text.PrettyPrint as Disp
import qualified Text.Read as Read

instance Pretty Version where
  prettier t ver =
    let !() = trace ("=== Printed from instance Pretty Version\n" <> show t) ()
    in  Disp.hcat
      ( Disp.punctuate
          (Disp.char '.')
          (map Disp.int $ versionNumbers ver)
      )
