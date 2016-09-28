{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Distribution.Types.TestType (
    TestType(..),
    knownTestTypes,
) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Version
import Distribution.Text

import Text.PrettyPrint as Disp

-- | The \"test-type\" field in the test suite stanza.
--
data TestType = TestTypeExe Version     -- ^ \"type: exitcode-stdio-x.y\"
              | TestTypeLib Version     -- ^ \"type: detailed-x.y\"
              | TestTypeUnknown String Version -- ^ Some unknown test type e.g. \"type: foo\"
    deriving (Generic, Show, Read, Eq, Typeable, Data)

instance Binary TestType

knownTestTypes :: [TestType]
knownTestTypes = [ TestTypeExe (mkVersion [1,0])
                 , TestTypeLib (mkVersion [0,9]) ]

instance Text TestType where
  disp (TestTypeExe ver)          = text "exitcode-stdio-" <<>> disp ver
  disp (TestTypeLib ver)          = text "detailed-"       <<>> disp ver
  disp (TestTypeUnknown name ver) = text name <<>> char '-' <<>> disp ver

  parse = stdParse $ \ver name -> case name of
    "exitcode-stdio" -> TestTypeExe ver
    "detailed"       -> TestTypeLib ver
    _                -> TestTypeUnknown name ver
