{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Distribution.Types.GenericPackageDescription (
    GenericPackageDescription(..),
    emptyGenericPackageDescription,
    Flag(..),
    emptyFlag,
    FlagName,
    mkFlagName,
    unFlagName,
    FlagAssignment,
    mkFlagAssignment,
    unFlagAssignment,
    lookupFlagAssignment,
    insertFlagAssignment,
    diffFlagAssignment,
    findDuplicateFlagAssignments,
    nullFlagAssignment,
    showFlagValue,
    dispFlagAssignment,
    parseFlagAssignment,
    parsecFlagAssignment,
    ConfVar(..),
) where

import Prelude ()
import Distribution.Compat.Prelude
import Distribution.Utils.ShortText
import Distribution.Utils.Generic (lowercase)
import qualified Text.PrettyPrint as Disp
import qualified Data.Map as Map
import qualified Distribution.Compat.ReadP as Parse
import qualified Distribution.Compat.CharParsing as P
import Distribution.Compat.ReadP ((+++))

-- lens
import Distribution.Compat.Lens                     as L
import qualified Distribution.Types.BuildInfo.Lens  as L

import Distribution.Types.PackageDescription

import Distribution.Types.Dependency
import Distribution.Types.Library
import Distribution.Types.ForeignLib
import Distribution.Types.Executable
import Distribution.Types.TestSuite
import Distribution.Types.Benchmark
import Distribution.Types.UnqualComponentName
import Distribution.Types.CondTree

import Distribution.Package
import Distribution.Version
import Distribution.Compiler
import Distribution.System
import Distribution.Parsec.Class
import Distribution.Pretty
import Distribution.Text

-- ---------------------------------------------------------------------------
-- The 'GenericPackageDescription' type

data GenericPackageDescription =
  GenericPackageDescription
  { packageDescription :: PackageDescription
  , genPackageFlags    :: [Flag]
  , condLibrary        :: Maybe (CondTree ConfVar [Dependency] Library)
  , condSubLibraries   :: [( UnqualComponentName
                           , CondTree ConfVar [Dependency] Library )]
  , condForeignLibs    :: [( UnqualComponentName
                           , CondTree ConfVar [Dependency] ForeignLib )]
  , condExecutables    :: [( UnqualComponentName
                           , CondTree ConfVar [Dependency] Executable )]
  , condTestSuites     :: [( UnqualComponentName
                           , CondTree ConfVar [Dependency] TestSuite )]
  , condBenchmarks     :: [( UnqualComponentName
                           , CondTree ConfVar [Dependency] Benchmark )]
  }
    deriving (Show, Eq, Typeable, Data, Generic)

instance Package GenericPackageDescription where
  packageId = packageId . packageDescription

instance Binary GenericPackageDescription

instance NFData GenericPackageDescription where rnf = genericRnf

emptyGenericPackageDescription :: GenericPackageDescription
emptyGenericPackageDescription = GenericPackageDescription emptyPackageDescription [] Nothing [] [] [] [] []

-- -----------------------------------------------------------------------------
-- Traversal Instances

instance L.HasBuildInfos GenericPackageDescription where
  traverseBuildInfos f (GenericPackageDescription p a1 x1 x2 x3 x4 x5 x6) =
    GenericPackageDescription
        <$> L.traverseBuildInfos f p
        <*> pure a1
        <*> (traverse . traverse . L.buildInfo) f x1
        <*> (traverse . L._2 . traverse . L.buildInfo) f x2
        <*> (traverse . L._2 . traverse . L.buildInfo) f x3
        <*> (traverse . L._2 . traverse . L.buildInfo) f x4
        <*> (traverse . L._2 . traverse . L.buildInfo) f x5
        <*> (traverse . L._2 . traverse . L.buildInfo) f x6

-- -----------------------------------------------------------------------------
-- The Flag' type

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

instance NFData Flag where rnf = genericRnf

-- | A 'Flag' initialized with default parameters.
emptyFlag :: FlagName -> Flag
emptyFlag name = MkFlag
    { flagName        = name
    , flagDescription = ""
    , flagDefault     = True
    , flagManual      = False
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

instance Pretty FlagName where
    pretty = Disp.text . unFlagName

instance Parsec FlagName where
    parsec = mkFlagName . lowercase <$> parsec'
      where
        parsec' = (:) <$> lead <*> rest
        lead = P.satisfy (\c ->  isAlphaNum c || c == '_')
        rest = P.munch (\c -> isAlphaNum c ||  c == '_' || c == '-')

instance Text FlagName where
    -- Note:  we don't check that FlagName doesn't have leading dash,
    -- cabal check will do that.
    parse = mkFlagName . lowercase <$> parse'
      where
        parse' = (:) <$> lead <*> rest
        lead = Parse.satisfy (\c ->  isAlphaNum c || c == '_')
        rest = Parse.munch (\c -> isAlphaNum c ||  c == '_' || c == '-')

-- | A 'FlagAssignment' is a total or partial mapping of 'FlagName's to
-- 'Bool' flag values. It represents the flags chosen by the user or
-- discovered during configuration. For example @--flags=foo --flags=-bar@
-- becomes @[("foo", True), ("bar", False)]@
--
newtype FlagAssignment
  = FlagAssignment { getFlagAssignment :: Map.Map FlagName (Int, Bool) }
  deriving (Binary, NFData)

instance Eq FlagAssignment where
  (==) (FlagAssignment m1) (FlagAssignment m2)
    = fmap snd m1 == fmap snd m2

instance Ord FlagAssignment where
  compare (FlagAssignment m1) (FlagAssignment m2)
    = fmap snd m1 `compare` fmap snd m2

-- | Combines pairs of values contained in the 'FlagAssignment' Map.
--
-- The last flag specified takes precedence, and we record the number
-- of times we have seen the flag.
--
combineFlagValues :: (Int, Bool) -> (Int, Bool) -> (Int, Bool)
combineFlagValues (c1, _) (c2, b2) = (c1 + c2, b2)

-- The 'Semigroup' instance currently is right-biased.
--
-- If duplicate flags are specified, we want the last flag specified to
-- take precedence and we want to know how many times the flag has been
-- specified so that we have the option of warning the user about
-- supplying duplicate flags.
instance Semigroup FlagAssignment where
  (<>) (FlagAssignment m1) (FlagAssignment m2)
    = FlagAssignment (Map.unionWith combineFlagValues m1 m2)

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
  FlagAssignment .
  Map.fromListWith (flip combineFlagValues) . fmap (fmap (\b -> (1, b)))

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
-- If the flag is already present in the 'FlagAssigment', the
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
  FlagAssignment .
  Map.insertWith (flip combineFlagValues) flag (1, val) .  getFlagAssignment

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
diffFlagAssignment fa1 fa2 = FlagAssignment
  (Map.difference (getFlagAssignment fa1) (getFlagAssignment fa2))

-- | Find the 'FlagName's that have been listed more than once.
--
-- @since 2.2.0
findDuplicateFlagAssignments :: FlagAssignment -> [FlagName]
findDuplicateFlagAssignments =
  Map.keys . Map.filter ((> 1) . fst) . getFlagAssignment

-- | @since 2.2.0
instance Read FlagAssignment where
    readsPrec p s = [ (FlagAssignment x, rest) | (x,rest) <- readsPrec p s ]

-- | @since 2.2.0
instance Show FlagAssignment where
    showsPrec p (FlagAssignment xs) = showsPrec p xs

-- | String representation of a flag-value pair.
showFlagValue :: (FlagName, Bool) -> String
showFlagValue (f, True)   = '+' : unFlagName f
showFlagValue (f, False)  = '-' : unFlagName f

-- | Pretty-prints a flag assignment.
dispFlagAssignment :: FlagAssignment -> Disp.Doc
dispFlagAssignment = Disp.hsep . map (Disp.text . showFlagValue) . unFlagAssignment

-- | Parses a flag assignment.
parsecFlagAssignment :: ParsecParser FlagAssignment
parsecFlagAssignment = mkFlagAssignment <$>
                       P.sepBy (onFlag <|> offFlag) P.skipSpaces1
  where
    onFlag = do
        _ <- P.optional (P.char '+')
        f <- parsec
        return (f, True)
    offFlag = do
        _ <- P.char '-'
        f <- parsec
        return (f, False)

-- | Parses a flag assignment.
parseFlagAssignment :: Parse.ReadP r FlagAssignment
parseFlagAssignment = mkFlagAssignment <$>
                      Parse.sepBy parseFlagValue Parse.skipSpaces1
  where
    parseFlagValue =
          (do Parse.optional (Parse.char '+')
              f <- parse
              return (f, True))
      +++ (do _ <- Parse.char '-'
              f <- parse
              return (f, False))
-- {-# DEPRECATED parseFlagAssignment "Use parsecFlagAssignment. This symbol will be removed in Cabal-3.0 (est. Oct 2018)." #-}

-- -----------------------------------------------------------------------------
-- The 'CondVar' type

-- | A @ConfVar@ represents the variable type used.
data ConfVar = OS OS
             | Arch Arch
             | Flag FlagName
             | Impl CompilerFlavor VersionRange
    deriving (Eq, Show, Typeable, Data, Generic)

instance Binary ConfVar

instance NFData ConfVar where rnf = genericRnf
