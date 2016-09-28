{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Distribution.Types.BenchmarkType (
    BenchmarkType(..),
    knownBenchmarkTypes,
) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Version
import Distribution.Text

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

instance Text BenchmarkType where
  disp (BenchmarkTypeExe ver)          = text "exitcode-stdio-" <<>> disp ver
  disp (BenchmarkTypeUnknown name ver) = text name <<>> char '-' <<>> disp ver

  parse = stdParse $ \ver name -> case name of
    "exitcode-stdio" -> BenchmarkTypeExe ver
    _                -> BenchmarkTypeUnknown name ver


