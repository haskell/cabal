{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Distribution.Types.TestType
  ( TestType (..)
  , knownTestTypes
  , testTypeExe
  , testTypeLib
  ) where

import Distribution.Compat.Prelude
import Distribution.Version
import Prelude ()

import Distribution.Parsec
import Distribution.Pretty
import Text.PrettyPrint (char, text)

-- | The \"test-type\" field in the test suite stanza.
data TestType
  = -- | \"type: exitcode-stdio-x.y\"
    TestTypeExe Version
  | -- | \"type: detailed-x.y\"
    TestTypeLib Version
  | -- | Some unknown test type e.g. \"type: foo\"
    TestTypeUnknown String Version
  deriving (Generic, Show, Read, Eq, Ord, Typeable, Data)

instance Binary TestType
instance Structured TestType

instance NFData TestType where rnf = genericRnf

knownTestTypes :: [TestType]
knownTestTypes =
  [ testTypeExe
  , testTypeLib
  ]

testTypeExe :: TestType
testTypeExe = TestTypeExe (mkVersion [1, 0])

testTypeLib :: TestType
testTypeLib = TestTypeLib (mkVersion [0, 9])

instance Pretty TestType where
  pretty (TestTypeExe ver) = text "exitcode-stdio-" <<>> pretty ver
  pretty (TestTypeLib ver) = text "detailed-" <<>> pretty ver
  pretty (TestTypeUnknown name ver) = text name <<>> char '-' <<>> pretty ver

instance Parsec TestType where
  parsec = parsecStandard $ \ver name -> case name of
    "exitcode-stdio" -> TestTypeExe ver
    "detailed" -> TestTypeLib ver
    _ -> TestTypeUnknown name ver
