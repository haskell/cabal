{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Distribution.Types.BenchmarkType (
    BenchmarkType(..),
    knownBenchmarkTypes,
) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Pretty
import Distribution.Text
import Distribution.Version

import Text.PrettyPrint as Disp

-- | The \"benchmark-type\" field in the benchmark stanza.
--
data BenchmarkType = BenchmarkTypeExe Version
                     -- ^ \"type: exitcode-stdio-x.y\"
                   | BenchmarkTypeUnknown String Version
                     -- ^ Some unknown benchmark type e.g. \"type: foo\"
    deriving (Generic, Show, Read, Eq, Typeable, Data)

instance Binary BenchmarkType

knownBenchmarkTypes :: [BenchmarkType]
knownBenchmarkTypes = [ BenchmarkTypeExe (mkVersion [1,0]) ]

instance Pretty BenchmarkType where
  pretty (BenchmarkTypeExe ver)          = text "exitcode-stdio-" <<>> pretty ver
  pretty (BenchmarkTypeUnknown name ver) = text name <<>> char '-' <<>> pretty ver

instance Text BenchmarkType where
  parse = stdParse $ \ver name -> case name of
    "exitcode-stdio" -> BenchmarkTypeExe ver
    _                -> BenchmarkTypeUnknown name ver


