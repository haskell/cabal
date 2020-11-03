{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
module Distribution.Solver.Types.OptionalStanza (
    -- * OptionalStanza
    OptionalStanza(..),
    showStanza,
    showStanzas,
    enableStanzas,
    -- * Set of stanzas
    OptionalStanzaSet,
    optStanzaSetFromList,
    optStanzaSetToList,
    optStanzaSetMember,
    optStanzaSetInsert,
    optStanzaSetSingleton,
    optStanzaSetIntersection,
    optStanzaSetNull,
    optStanzaSetIsSubset,
    -- * Map indexed by stanzas
    OptionalStanzaMap,
    optStanzaTabulate,
    optStanzaIndex,
    optStanzaLookup,
    optStanzaKeysFilteredByValue,
) where

import Distribution.Solver.Compat.Prelude
import Prelude ()

import Data.Bits                                 (testBit, (.|.), (.&.))
import Distribution.Types.ComponentRequestedSpec (ComponentRequestedSpec (..))
import Distribution.Utils.Structured (Structured (..), nominalStructure)

-------------------------------------------------------------------------------
-- OptionalStanza
-------------------------------------------------------------------------------

data OptionalStanza
    = TestStanzas
    | BenchStanzas
  deriving (Eq, Ord, Enum, Bounded, Show, Generic, Typeable)

-- | String representation of an OptionalStanza.
showStanza :: OptionalStanza -> String
showStanza TestStanzas  = "test"
showStanza BenchStanzas = "bench"

showStanzas :: OptionalStanzaSet -> String
showStanzas = unwords . map (("*" ++) . showStanza) . optStanzaSetToList

-- | Convert a list of 'OptionalStanza' into the corresponding
-- Cabal's 'ComponentRequestedSpec' which records what components are enabled.
--
enableStanzas :: OptionalStanzaSet -> ComponentRequestedSpec
enableStanzas optionalStanzas = ComponentRequestedSpec
    { testsRequested      = optStanzaSetMember TestStanzas  optionalStanzas
    , benchmarksRequested = optStanzaSetMember BenchStanzas optionalStanzas
    }

instance Binary OptionalStanza
instance Structured OptionalStanza

-------------------------------------------------------------------------------
-- OptionalStanzaSet
-------------------------------------------------------------------------------

newtype OptionalStanzaSet = OptionalStanzaSet Word
  deriving (Eq, Ord, Show)

instance Binary OptionalStanzaSet where
    put (OptionalStanzaSet w) = put w
    get = fmap (OptionalStanzaSet . (.&. 0x03)) get

instance Structured OptionalStanzaSet where
    structure = nominalStructure

optStanzaSetFromList :: [OptionalStanza] -> OptionalStanzaSet
optStanzaSetFromList = foldl' (flip optStanzaSetInsert) mempty

optStanzaSetToList :: OptionalStanzaSet -> [OptionalStanza]
optStanzaSetToList (OptionalStanzaSet 0) = []
optStanzaSetToList (OptionalStanzaSet 1) = [TestStanzas]
optStanzaSetToList (OptionalStanzaSet 2) = [BenchStanzas]
optStanzaSetToList (OptionalStanzaSet 3) = [TestStanzas, BenchStanzas]
optStanzaSetToList (OptionalStanzaSet _) = []

optStanzaSetInsert :: OptionalStanza -> OptionalStanzaSet -> OptionalStanzaSet
optStanzaSetInsert x s = optStanzaSetSingleton x <> s

optStanzaSetMember :: OptionalStanza -> OptionalStanzaSet -> Bool
optStanzaSetMember TestStanzas  (OptionalStanzaSet w) = testBit w 0
optStanzaSetMember BenchStanzas (OptionalStanzaSet w) = testBit w 1

optStanzaSetSingleton :: OptionalStanza -> OptionalStanzaSet
optStanzaSetSingleton TestStanzas  = OptionalStanzaSet 1
optStanzaSetSingleton BenchStanzas = OptionalStanzaSet 2

optStanzaSetIntersection :: OptionalStanzaSet -> OptionalStanzaSet -> OptionalStanzaSet
optStanzaSetIntersection (OptionalStanzaSet a) (OptionalStanzaSet b) = OptionalStanzaSet (a .&. b)

optStanzaSetNull :: OptionalStanzaSet -> Bool
optStanzaSetNull (OptionalStanzaSet w) = w == 0

optStanzaSetIsSubset :: OptionalStanzaSet -> OptionalStanzaSet -> Bool
optStanzaSetIsSubset (OptionalStanzaSet a) (OptionalStanzaSet b) = (a .|. b) == b

instance Semigroup OptionalStanzaSet where
    OptionalStanzaSet a <> OptionalStanzaSet b = OptionalStanzaSet (a .|. b)

instance Monoid OptionalStanzaSet where
    mempty = OptionalStanzaSet 0
    mappend = (<>)

-------------------------------------------------------------------------------
-- OptionalStanzaMap
-------------------------------------------------------------------------------

-- | Note: this is total map.
data OptionalStanzaMap a = OptionalStanzaMap a a
  deriving (Eq, Ord, Show, Generic)

instance Binary a => Binary (OptionalStanzaMap a)
instance Structured a => Structured (OptionalStanzaMap a)

optStanzaTabulate :: (OptionalStanza -> a) -> OptionalStanzaMap a
optStanzaTabulate f = OptionalStanzaMap (f TestStanzas) (f BenchStanzas)

optStanzaIndex :: OptionalStanzaMap a -> OptionalStanza -> a
optStanzaIndex (OptionalStanzaMap x _) TestStanzas  = x
optStanzaIndex (OptionalStanzaMap _ x) BenchStanzas = x

optStanzaLookup :: OptionalStanza -> OptionalStanzaMap a -> a
optStanzaLookup = flip optStanzaIndex

optStanzaKeysFilteredByValue :: (a -> Bool) -> OptionalStanzaMap a -> OptionalStanzaSet
optStanzaKeysFilteredByValue p (OptionalStanzaMap x y)
    | p x       = if p y then OptionalStanzaSet 3 else OptionalStanzaSet 1
    | otherwise = if p y then OptionalStanzaSet 2 else OptionalStanzaSet 0
