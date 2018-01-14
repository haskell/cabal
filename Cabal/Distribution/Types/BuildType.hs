{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Distribution.Types.BuildType (
    BuildType(..),
    knownBuildTypes,
) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.CabalSpecVersion (CabalSpecVersion (..))
import Distribution.Pretty
import Distribution.Parsec.Class
import Distribution.Text

import qualified Distribution.Compat.CharParsing as P
import qualified Distribution.Compat.ReadP as Parse
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
          if v <= CabalSpecOld
          then do
              parsecWarning PWTBuildTypeDefault "build-type: Default is parsed as Custom for legacy reasons. See https://github.com/haskell/cabal/issues/5020"
              return Custom
          else fail ("unknown build-type: '" ++ name ++ "'")
      _           -> fail ("unknown build-type: '" ++ name ++ "'")

instance Text BuildType where
  parse = do
    name <- Parse.munch1 isAlphaNum
    case name of
      "Simple"    -> return Simple
      "Configure" -> return Configure
      "Custom"    -> return Custom
      "Make"      -> return Make
      "Default"   -> return Custom
      _           -> fail ("unknown build-type: '" ++ name ++ "'")
