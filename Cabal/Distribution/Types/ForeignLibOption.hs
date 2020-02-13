{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Distribution.Types.ForeignLibOption(
    ForeignLibOption(..)
) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Pretty
import Distribution.Parsec
import Distribution.FieldGrammar.Described

import qualified Distribution.Compat.CharParsing as P
import qualified Text.PrettyPrint as Disp

data ForeignLibOption =
     -- | Merge in all dependent libraries (i.e., use
     -- @ghc -shared -static@ rather than just record
     -- the dependencies, ala @ghc -shared -dynamic@).
     -- This option is compulsory on Windows and unsupported
     -- on other platforms.
     ForeignLibStandalone
    deriving (Generic, Show, Read, Eq, Typeable, Data)

instance Pretty ForeignLibOption where
  pretty ForeignLibStandalone = Disp.text "standalone"

instance Parsec ForeignLibOption where
  parsec = do
    name <- P.munch1 (\c -> isAlphaNum c || c == '-')
    case name of
      "standalone" -> return ForeignLibStandalone
      _            -> fail "unrecognized foreign-library option"

instance Described ForeignLibOption where
    describe _ = "standalone"

instance Binary ForeignLibOption
instance Structured ForeignLibOption
instance NFData ForeignLibOption where rnf = genericRnf
