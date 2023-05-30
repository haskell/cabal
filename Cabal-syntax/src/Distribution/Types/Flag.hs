{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Distribution.Types.Flag
  ( -- * Package flag
    PackageFlag (..)
  , emptyFlag

    -- * Flag name
  , FlagName
  , mkFlagName
  , unFlagName

    -- * Flag assignment
  , FlagAssignment
  , mkFlagAssignment
  , unFlagAssignment
  , lookupFlagAssignment
  , insertFlagAssignment
  , diffFlagAssignment
  , findDuplicateFlagAssignments
  , nullFlagAssignment
  , showFlagValue
  , dispFlagAssignment
  , showFlagAssignment
  , parsecFlagAssignment
  , parsecFlagAssignmentNonEmpty

    -- ** Legacy formats
  , legacyShowFlagAssignment
  , legacyShowFlagAssignment'
  , legacyParsecFlagAssignment
  ) where

import Distribution.Compat.Prelude
import Distribution.Utils.Generic (lowercase)
import Distribution.Utils.ShortText
import Prelude ()

import Distribution.Parsec
import Distribution.Pretty

import qualified Data.Map as Map
import qualified Distribution.Compat.CharParsing as P
import qualified Text.PrettyPrint as Disp

-- -----------------------------------------------------------------------------
-- The Flag' type

-- | A flag can represent a feature to be included, or a way of linking
--   a target against its dependencies, or in fact whatever you can think of.
data PackageFlag = MkPackageFlag
  { flagName :: FlagName
  , flagDescription :: String
  , flagDefault :: Bool
  , flagManual :: Bool
  }
  deriving (Show, Eq, Typeable, Data, Generic)

instance Binary PackageFlag
instance Structured PackageFlag
instance NFData PackageFlag where rnf = genericRnf

-- | A 'PackageFlag' initialized with default parameters.
emptyFlag :: FlagName -> PackageFlag
emptyFlag name =
  MkPackageFlag
    { flagName = name
    , flagDescription = ""
    , flagDefault = True
    , flagManual = False
    }

-- | A 'FlagName' is the name of a user-defined configuration flag
--
-- Use 'mkFlagName' and 'unFlagName' to convert from/to a 'String'.
--
-- This type is opaque since @Cabal-2.0@
--
-- @since 2.0.0.2
newtype FlagName = FlagName ShortText
  deriving (Eq, Generic, Ord, Show, Read, Typeable, Data, NFData)

-- | Construct a 'FlagName' from a 'String'
--
-- 'mkFlagName' is the inverse to 'unFlagName'
--
-- Note: No validations are performed to ensure that the resulting
-- 'FlagName' is valid
--
-- @since 2.0.0.2
mkFlagName :: String -> FlagName
mkFlagName = FlagName . toShortText

-- | 'mkFlagName'
--
-- @since 2.0.0.2
instance IsString FlagName where
  fromString = mkFlagName

-- | Convert 'FlagName' to 'String'
--
-- @since 2.0.0.2
unFlagName :: FlagName -> String
unFlagName (FlagName s) = fromShortText s

instance Binary FlagName
instance Structured FlagName

instance Pretty FlagName where
  pretty = Disp.text . unFlagName

instance Parsec FlagName where
  -- Note:  we don't check that FlagName doesn't have leading dash,
  -- cabal check will do that.
  parsec = mkFlagName . lowercase <$> parsec'
    where
      parsec' = (:) <$> lead <*> rest
      lead = P.satisfy (\c -> isAlphaNum c || c == '_')
      rest = P.munch (\c -> isAlphaNum c || c == '_' || c == '-')

-- | A 'FlagAssignment' is a total or partial mapping of 'FlagName's to
-- 'Bool' flag values. It represents the flags chosen by the user or
-- discovered during configuration. For example @--flags=foo --flags=-bar@
-- becomes @[("foo", True), ("bar", False)]@
--
-- TODO: Why we record the multiplicity of the flag?
newtype FlagAssignment = FlagAssignment {getFlagAssignment :: Map.Map FlagName (Int, Bool)}
  deriving (Binary, Generic, NFData, Typeable)

instance Structured FlagAssignment

instance Eq FlagAssignment where
  (==) (FlagAssignment m1) (FlagAssignment m2) =
    fmap snd m1 == fmap snd m2

instance Ord FlagAssignment where
  compare (FlagAssignment m1) (FlagAssignment m2) =
    fmap snd m1 `compare` fmap snd m2

-- | Combines pairs of values contained in the 'FlagAssignment' Map.
--
-- The last flag specified takes precedence, and we record the number
-- of times we have seen the flag.
combineFlagValues :: (Int, Bool) -> (Int, Bool) -> (Int, Bool)
combineFlagValues (c1, _) (c2, b2) = (c1 + c2, b2)

-- The 'Semigroup' instance currently is right-biased.
--
-- If duplicate flags are specified, we want the last flag specified to
-- take precedence and we want to know how many times the flag has been
-- specified so that we have the option of warning the user about
-- supplying duplicate flags.
instance Semigroup FlagAssignment where
  (<>) (FlagAssignment m1) (FlagAssignment m2) =
    FlagAssignment (Map.unionWith combineFlagValues m1 m2)

instance Monoid FlagAssignment where
  mempty = FlagAssignment Map.empty
  mappend = (<>)

-- | Construct a 'FlagAssignment' from a list of flag/value pairs.
--
-- If duplicate flags occur in the input list, the later entries
-- in the list will take precedence.
--
-- @since 2.2.0
mkFlagAssignment :: [(FlagName, Bool)] -> FlagAssignment
mkFlagAssignment =
  FlagAssignment
    . Map.fromListWith (flip combineFlagValues)
    . fmap (fmap (\b -> (1, b)))

-- | Deconstruct a 'FlagAssignment' into a list of flag/value pairs.
--
-- @ 'null' ('findDuplicateFlagAssignments' fa) ==> ('mkFlagAssignment' . 'unFlagAssignment') fa == fa @
--
-- @since 2.2.0
unFlagAssignment :: FlagAssignment -> [(FlagName, Bool)]
unFlagAssignment = fmap (fmap snd) . Map.toList . getFlagAssignment

-- | Test whether 'FlagAssignment' is empty.
--
-- @since 2.2.0
nullFlagAssignment :: FlagAssignment -> Bool
nullFlagAssignment = Map.null . getFlagAssignment

-- | Lookup the value for a flag
--
-- Returns 'Nothing' if the flag isn't contained in the 'FlagAssignment'.
--
-- @since 2.2.0
lookupFlagAssignment :: FlagName -> FlagAssignment -> Maybe Bool
lookupFlagAssignment fn = fmap snd . Map.lookup fn . getFlagAssignment

-- | Insert or update the boolean value of a flag.
--
-- If the flag is already present in the 'FlagAssignment', the
-- value will be updated and the fact that multiple values have
-- been provided for that flag will be recorded so that a
-- warning can be generated later on.
--
-- @since 2.2.0
insertFlagAssignment :: FlagName -> Bool -> FlagAssignment -> FlagAssignment
-- TODO: this currently just shadows prior values for an existing
-- flag; rather than enforcing uniqueness at construction, it's
-- verified later on via `D.C.Dependency.configuredPackageProblems`
insertFlagAssignment flag val =
  FlagAssignment
    . Map.insertWith (flip combineFlagValues) flag (1, val)
    . getFlagAssignment

-- | Remove all flag-assignments from the first 'FlagAssignment' that
-- are contained in the second 'FlagAssignment'
--
-- NB/TODO: This currently only removes flag assignments which also
-- match the value assignment! We should review the code which uses
-- this operation to figure out if this it's not enough to only
-- compare the flagnames without the values.
--
-- @since 2.2.0
diffFlagAssignment :: FlagAssignment -> FlagAssignment -> FlagAssignment
diffFlagAssignment fa1 fa2 =
  FlagAssignment
    (Map.difference (getFlagAssignment fa1) (getFlagAssignment fa2))

-- | Find the 'FlagName's that have been listed more than once.
--
-- @since 2.2.0
findDuplicateFlagAssignments :: FlagAssignment -> [FlagName]
findDuplicateFlagAssignments =
  Map.keys . Map.filter ((> 1) . fst) . getFlagAssignment

-- | @since 2.2.0
instance Read FlagAssignment where
  readsPrec p s = [(FlagAssignment x, rest) | (x, rest) <- readsPrec p s]

-- | @since 2.2.0
instance Show FlagAssignment where
  showsPrec p (FlagAssignment xs) = showsPrec p xs

-- | String representation of a flag-value pair.
showFlagValue :: (FlagName, Bool) -> String
showFlagValue (f, True) = '+' : unFlagName f
showFlagValue (f, False) = '-' : unFlagName f

-- | @since 3.4.0.0
instance Pretty FlagAssignment where
  pretty = dispFlagAssignment

-- |
--
-- >>> simpleParsec "" :: Maybe FlagAssignment
-- Just (fromList [])
--
-- >>> simpleParsec "+foo -bar" :: Maybe FlagAssignment
-- Just (fromList [(FlagName "bar",(1,False)),(FlagName "foo",(1,True))])
--
-- >>> simpleParsec "-none -any" :: Maybe FlagAssignment
-- Just (fromList [(FlagName "any",(1,False)),(FlagName "none",(1,False))])
--
-- >>> simpleParsec "+foo -foo +foo +foo" :: Maybe FlagAssignment
-- Just (fromList [(FlagName "foo",(4,True))])
--
-- >>> simpleParsec "+foo -bar baz" :: Maybe FlagAssignment
-- Nothing
--
-- Issue #7279 was fixed in Cabal-3.8
--
-- >>> explicitEitherParsec (parsecCommaList parsec) "+foo , -bar" :: Either String [FlagAssignment]
-- Right [fromList [(FlagName "foo",(1,True))],fromList [(FlagName "bar",(1,False))]]
--
-- >>> explicitEitherParsec (parsecCommaList parsecFlagAssignmentNonEmpty) "+foo , -bar" :: Either String [FlagAssignment]
-- Right [fromList [(FlagName "foo",(1,True))],fromList [(FlagName "bar",(1,False))]]
--
-- >>> simpleParsec "+foo+foo" :: Maybe FlagAssignment
-- Nothing
--
-- @since 3.4.0.0
instance Parsec FlagAssignment where
  parsec = parsecFlagAssignment

-- | Pretty-prints a flag assignment.
dispFlagAssignment :: FlagAssignment -> Disp.Doc
dispFlagAssignment = Disp.hsep . map (Disp.text . showFlagValue) . unFlagAssignment

-- | Parses a flag assignment.
parsecFlagAssignment :: CabalParsing m => m FlagAssignment
parsecFlagAssignment = mkFlagAssignment <$> sepByEnding (onFlag <|> offFlag) P.skipSpaces1
  where
    onFlag = do
      _ <- P.char '+'
      f <- parsec
      return (f, True)
    offFlag = do
      _ <- P.char '-'
      f <- parsec
      return (f, False)

    sepByEnding :: CabalParsing m => m a -> m b -> m [a]
    sepByEnding p sep = afterSeparator
      where
        element = (:) <$> p <*> afterElement
        afterElement = sep *> afterSeparator <|> pure []
        afterSeparator = element <|> pure []

-- | Parse a non-empty flag assignment
--
-- The flags have to explicitly start with minus or plus.
--
-- @since 3.4.0.0
parsecFlagAssignmentNonEmpty :: CabalParsing m => m FlagAssignment
parsecFlagAssignmentNonEmpty = mkFlagAssignment <$> sepByEnding1 (onFlag <|> offFlag) P.skipSpaces1
  where
    onFlag = do
      _ <- P.char '+'
      f <- parsec
      return (f, True)
    offFlag = do
      _ <- P.char '-'
      f <- parsec
      return (f, False)

    sepByEnding1 :: CabalParsing m => m a -> m b -> m [a]
    sepByEnding1 p sep = element
      where
        element = (:) <$> p <*> afterElement
        afterElement = sep *> afterSeparator <|> pure []
        afterSeparator = element <|> pure []

-- | Show flag assignment.
--
-- @since 3.4.0.0
showFlagAssignment :: FlagAssignment -> String
showFlagAssignment = prettyShow . dispFlagAssignment

-------------------------------------------------------------------------------
-- Legacy: without requiring +
-------------------------------------------------------------------------------

-- | We need this as far as we support custom setups older than 2.2.0.0
--
-- @since 3.4.0.0
legacyShowFlagAssignment :: FlagAssignment -> String
legacyShowFlagAssignment =
  prettyShow . Disp.hsep . map Disp.text . legacyShowFlagAssignment'

-- | @since 3.4.0.0
legacyShowFlagAssignment' :: FlagAssignment -> [String]
legacyShowFlagAssignment' = map legacyShowFlagValue . unFlagAssignment

-- | @since 3.4.0.0
legacyShowFlagValue :: (FlagName, Bool) -> String
legacyShowFlagValue (f, True) = unFlagName f
legacyShowFlagValue (f, False) = '-' : unFlagName f

-- |
-- We need this as far as we support custom setups older than 2.2.0.0
--
-- @since 3.4.0.0
legacyParsecFlagAssignment :: CabalParsing m => m FlagAssignment
legacyParsecFlagAssignment =
  mkFlagAssignment
    <$> P.sepBy (onFlag <|> offFlag) P.skipSpaces1
  where
    onFlag = do
      _ <- P.optional (P.char '+')
      f <- parsec
      return (f, True)
    offFlag = do
      _ <- P.char '-'
      f <- parsec
      return (f, False)
