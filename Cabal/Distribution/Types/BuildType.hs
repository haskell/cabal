{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Distribution.Types.BuildType (
    BuildType(..),
    knownBuildTypes,
) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.CabalSpecVersion (CabalSpecVersion (..))
import Distribution.Pretty
import Distribution.Parsec
import Distribution.FieldGrammar.Described

import qualified Distribution.Compat.CharParsing as P
import qualified Text.PrettyPrint as Disp

-- | The type of build system used by this package.
data BuildType
  = Simple      -- ^ calls @Distribution.Simple.defaultMain@
  | Configure   -- ^ calls @Distribution.Simple.defaultMainWithHooks defaultUserHooks@,
                -- which invokes @configure@ to generate additional build
                -- information used by later phases.
  | Make        -- ^ calls @Distribution.Make.defaultMain@
  | Custom      -- ^ uses user-supplied @Setup.hs@ or @Setup.lhs@ (default)
                deriving (Generic, Show, Read, Eq, Typeable, Data)

instance Binary BuildType
instance Structured BuildType
instance NFData BuildType where rnf = genericRnf

knownBuildTypes :: [BuildType]
knownBuildTypes = [Simple, Configure, Make, Custom]

instance Pretty BuildType where
  pretty = Disp.text . show

instance Parsec BuildType where
  parsec = do
    name <- P.munch1 isAlphaNum
    case name of
      "Simple"    -> return Simple
      "Configure" -> return Configure
      "Custom"    -> return Custom
      "Make"      -> return Make
      "Default"   -> do
          v <- askCabalSpecVersion
          if v <= CabalSpecV1_18 -- oldest version needing this, based on hackage-tests
          then do
              parsecWarning PWTBuildTypeDefault "build-type: Default is parsed as Custom for legacy reasons. See https://github.com/haskell/cabal/issues/5020"
              return Custom
          else fail ("unknown build-type: '" ++ name ++ "'")
      _           -> fail ("unknown build-type: '" ++ name ++ "'")
      
instance Described BuildType where
    describe _ = REUnion ["Simple","Configure","Custom","Make","Default"]
