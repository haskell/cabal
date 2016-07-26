{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Distribution.Types.GenericPackageDescription (
    GenericPackageDescription(..),
    Flag(..),
    FlagName(..),
    FlagAssignment,
    ConfVar(..),
    Condition(..),
    CondTree(..),
    cOr,
    cAnd,
    cNot,
) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Types.PackageDescription

import Distribution.Types.Library
import Distribution.Types.Executable
import Distribution.Types.TestSuite
import Distribution.Types.Benchmark

import Distribution.Package
import Distribution.Version
import Distribution.Compiler
import Distribution.System

-- ---------------------------------------------------------------------------
-- The GenericPackageDescription type

data GenericPackageDescription =
    GenericPackageDescription {
        packageDescription :: PackageDescription,
        genPackageFlags    :: [Flag],
        condLibrary        :: Maybe (CondTree ConfVar [Dependency] Library),
        condSubLibraries   :: [(String, CondTree ConfVar [Dependency] Library)],
        condExecutables    :: [(String, CondTree ConfVar [Dependency] Executable)],
        condTestSuites     :: [(String, CondTree ConfVar [Dependency] TestSuite)],
        condBenchmarks     :: [(String, CondTree ConfVar [Dependency] Benchmark)]
      }
    deriving (Show, Eq, Typeable, Data, Generic)

instance Package GenericPackageDescription where
  packageId = packageId . packageDescription

instance Binary GenericPackageDescription

-- | A flag can represent a feature to be included, or a way of linking
--   a target against its dependencies, or in fact whatever you can think of.
data Flag = MkFlag
    { flagName        :: FlagName
    , flagDescription :: String
    , flagDefault     :: Bool
    , flagManual      :: Bool
    }
    deriving (Show, Eq, Typeable, Data, Generic)

instance Binary Flag

-- | A 'FlagName' is the name of a user-defined configuration flag
newtype FlagName = FlagName String
    deriving (Eq, Generic, Ord, Show, Read, Typeable, Data)

instance Binary FlagName

-- | A 'FlagAssignment' is a total or partial mapping of 'FlagName's to
-- 'Bool' flag values. It represents the flags chosen by the user or
-- discovered during configuration. For example @--flags=foo --flags=-bar@
-- becomes @[("foo", True), ("bar", False)]@
--
type FlagAssignment = [(FlagName, Bool)]

-- | A @ConfVar@ represents the variable type used.
data ConfVar = OS OS
             | Arch Arch
             | Flag FlagName
             | Impl CompilerFlavor VersionRange
    deriving (Eq, Show, Typeable, Data, Generic)

instance Binary ConfVar

-- | A boolean expression parameterized over the variable type used.
data Condition c = Var c
                 | Lit Bool
                 | CNot (Condition c)
                 | COr (Condition c) (Condition c)
                 | CAnd (Condition c) (Condition c)
    deriving (Show, Eq, Typeable, Data, Generic)

-- | Boolean negation of a 'Condition' value.
cNot :: Condition a -> Condition a
cNot (Lit b)  = Lit (not b)
cNot (CNot c) = c
cNot c        = CNot c

-- | Boolean AND of two 'Condtion' values.
cAnd :: Condition a -> Condition a -> Condition a
cAnd (Lit False) _           = Lit False
cAnd _           (Lit False) = Lit False
cAnd (Lit True)  x           = x
cAnd x           (Lit True)  = x
cAnd x           y           = CAnd x y

-- | Boolean OR of two 'Condition' values.
cOr :: Eq v => Condition v -> Condition v -> Condition v
cOr  (Lit True)  _           = Lit True
cOr  _           (Lit True)  = Lit True
cOr  (Lit False) x           = x
cOr  x           (Lit False) = x
cOr  c           (CNot d)
  | c == d                   = Lit True
cOr  (CNot c)    d
  | c == d                   = Lit True
cOr  x           y           = COr x y

instance Functor Condition where
  f `fmap` Var c    = Var (f c)
  _ `fmap` Lit c    = Lit c
  f `fmap` CNot c   = CNot (fmap f c)
  f `fmap` COr c d  = COr  (fmap f c) (fmap f d)
  f `fmap` CAnd c d = CAnd (fmap f c) (fmap f d)

instance Foldable Condition where
  f `foldMap` Var c    = f c
  _ `foldMap` Lit _    = mempty
  f `foldMap` CNot c   = foldMap f c
  f `foldMap` COr c d  = foldMap f c `mappend` foldMap f d
  f `foldMap` CAnd c d = foldMap f c `mappend` foldMap f d

instance Traversable Condition where
  f `traverse` Var c    = Var `fmap` f c
  _ `traverse` Lit c    = pure $ Lit c
  f `traverse` CNot c   = CNot `fmap` traverse f c
  f `traverse` COr c d  = COr  `fmap` traverse f c <*> traverse f d
  f `traverse` CAnd c d = CAnd `fmap` traverse f c <*> traverse f d

instance Applicative Condition where
  pure  = Var
  (<*>) = ap

instance Monad Condition where
  return = pure
  -- Terminating cases
  (>>=) (Lit x) _ = Lit x
  (>>=) (Var x) f = f x
  -- Recursing cases
  (>>=) (CNot  x  ) f = CNot (x >>= f)
  (>>=) (COr   x y) f = COr  (x >>= f) (y >>= f)
  (>>=) (CAnd  x y) f = CAnd (x >>= f) (y >>= f)

instance Monoid (Condition a) where
  mempty = Lit False
  mappend = (<>)

instance Semigroup (Condition a) where
  (<>) = COr

instance Alternative Condition where
  empty = mempty
  (<|>) = mappend

instance MonadPlus Condition where
  mzero = mempty
  mplus = mappend

instance Binary c => Binary (Condition c)

data CondTree v c a = CondNode
    { condTreeData        :: a
    , condTreeConstraints :: c
    , condTreeComponents  :: [( Condition v
                              , CondTree v c a
                              , Maybe (CondTree v c a))]
    }
    deriving (Show, Eq, Typeable, Data, Generic)

instance (Binary v, Binary c, Binary a) => Binary (CondTree v c a)
