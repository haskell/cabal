{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}

module Distribution.Types.GenericPackageDescription (
    GenericPackageDescription(..),
    emptyGenericPackageDescription,
    allCondLibraries,
    lowerSpecVersion,
    lowerLicense,
    lowerBuildType,
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
import qualified Distribution.Types.Benchmark.Lens  as L
import qualified Distribution.Types.CommonPackageDescription.Lens as L
import qualified Distribution.Types.BuildInfo.Lens  as L
import qualified Distribution.Types.Executable.Lens as L
import qualified Distribution.Types.ForeignLib.Lens as L
import qualified Distribution.Types.Library.Lens    as L
import qualified Distribution.Types.TestSuite.Lens  as L

import Distribution.Types.BuildType
import Distribution.Types.CommonPackageDescription
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
import Distribution.License
import qualified Distribution.SPDX as SPDX
import Distribution.System
import Distribution.Parsec.Class
import Distribution.Pretty
import Distribution.Text

-- ---------------------------------------------------------------------------
-- The 'GenericPackageDescription' type

-- | This data type is the concrete representation of the file @pkg.cabal@.
--
-- It contains two kinds of information about the package: information which is
-- needed for all packages, such as the package name and version, and
-- information which is needed for the simple build system only, such as the
-- compiler options and library name.
--
-- When we initially read a @.cabal@ file we get a 'GenericPackageDescription',
-- which preserves all the conditional sections as is. For this reason, it is a
-- more concrete representation than 'PackageDescription'.
data GenericPackageDescription =
  GenericPackageDescription
  { -- | Fields shared with 'PackageDescription'
    --
    -- The "generic" in 'genericCommonPD' disambiguates it from the field of
    -- 'PackageDescription'.
    --
    -- @since 2.6
    genericCommonPD    :: CommonPackageDescription

    -- the following are required by all packages:

  , -- | The version of the Cabal spec that this package description uses.
    -- For historical reasons this is specified with a version range but
    -- only ranges of the form @>= v@ make sense. We are in the process of
    -- transitioning to specifying just a single version, not a range.
    --
    -- @since 2.6
    specVersionRaw     :: Either Version VersionRange
  , licenseRaw         :: Either SPDX.License License
  , -- | The original @build-type@ value as parsed from the
    -- @.cabal@ file without defaulting. See also 'buildType'.
    --
    -- @since 2.6
    buildTypeRaw       :: Maybe BuildType

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
  packageId = packageId . genericCommonPD

instance L.HasCommonPackageDescription GenericPackageDescription where
  commonPackageDescription f l = (\x -> l { genericCommonPD = x }) <$> f (genericCommonPD l)

instance Binary GenericPackageDescription

instance NFData GenericPackageDescription where rnf = genericRnf

emptyGenericPackageDescription :: GenericPackageDescription
emptyGenericPackageDescription = GenericPackageDescription
  { genericCommonPD  = emptyCommonPackageDescription

  , specVersionRaw   = Right anyVersion
  , licenseRaw       = Right UnspecifiedLicense -- TODO:
  , buildTypeRaw     = Nothing

  , genPackageFlags  = []
  , condLibrary      = Nothing
  , condSubLibraries = []
  , condForeignLibs  = []
  , condExecutables  = []
  , condTestSuites   = []
  , condBenchmarks   = []
  }

allCondLibraries :: GenericPackageDescription
                 -> [(Maybe UnqualComponentName, CondTree ConfVar [Dependency] Library)]
allCondLibraries p = ((Nothing,) <$> maybeToList (condLibrary p))
                  ++ (first Just <$> condSubLibraries p)

-- | Convert legacy-supporting raw version to version
--
-- Historically we used a version range but we are switching to using a single
-- version. Currently we accept either. This function converts into a single
-- version by ignoring upper bounds in the version range.
--
-- @since 2.4.0.0
lowerSpecVersion :: Either Version VersionRange -> Version
lowerSpecVersion (Left version) = version
lowerSpecVersion (Right versionRange) = case asVersionIntervals versionRange of
    []                            -> mkVersion [0]
    ((LowerBound version _, _):_) -> version

-- | Convert flexible license to SPDX
--
-- @since 2.2.0.0
lowerLicense :: Either SPDX.License License -> SPDX.License
lowerLicense = either id licenseToSPDX

-- | The effective @build-type@ after applying defaulting rules.
--
-- The original @build-type@ value parsed is stored in the
-- 'buildTypeRaw' field.  However, the @build-type@ field is optional
-- and can therefore be empty in which case we need to compute the
-- /effective/ @build-type@. This function implements the following
-- defaulting rules:
--
--  * For @cabal-version:2.0@ and below, default to the @Custom@
--    build-type unconditionally.
--
--  * Otherwise, if a @custom-setup@ stanza is defined, default to
--    the @Custom@ build-type; else default to @Simple@ build-type.
--
-- @since 2.2
lowerBuildType :: GenericPackageDescription -> BuildType
lowerBuildType pkg
  | lowerSpecVersion (specVersionRaw pkg) >= mkVersion [2,1]
    = fromMaybe newDefault (buildTypeRaw pkg)
  | otherwise -- cabal-version < 2.1
    = fromMaybe Custom (buildTypeRaw pkg)
  where
    newDefault = case setupBuildInfo $ genericCommonPD pkg of
      Nothing -> Simple
      Just _  -> Custom

-- -----------------------------------------------------------------------------
-- Traversal Instances

instance L.HasBuildInfos GenericPackageDescription where
  traverseBuildInfos f (GenericPackageDescription p a1 a2 a3 a4 x1 x2 x3 x4 x5 x6) =
    GenericPackageDescription p a1 a2 a3 a4
        <$> (traverse . traverse . L.buildInfo) f x1
        <*> (traverse . L._2 . traverse . L.buildInfo) f x2
        <*> (traverse . L._2 . traverse . L.buildInfo) f x3
        <*> (traverse . L._2 . traverse . L.buildInfo) f x4
        <*> (traverse . L._2 . traverse . L.buildInfo) f x5
        <*> (traverse . L._2 . traverse . L.buildInfo) f x6

instance L.HasLibraries GenericPackageDescription where
  traverseLibraries f (GenericPackageDescription p a1 a2 a3 a4 x1 x2 x3 x4 x5 x6) =
    GenericPackageDescription p a1 a2 a3 a4
        <$> (traverse . traverse) f x1
        <*> (traverse . L._2 . traverse) f x2
        <*> pure x3
        <*> pure x4
        <*> pure x5
        <*> pure x6

instance L.HasExecutables GenericPackageDescription where
  traverseExecutables = lens . traverse . L._2 . traverse
    where lens f s = fmap (\x -> s { condExecutables = x }) (f (condExecutables s))

instance L.HasForeignLibs GenericPackageDescription where
  traverseForeignLibs = lens . traverse . L._2 . traverse
    where lens f s = fmap (\x -> s { condForeignLibs = x }) (f (condForeignLibs s))

instance L.HasTestSuites GenericPackageDescription where
  traverseTestSuites = lens . traverse . L._2 . traverse
    where lens f s = fmap (\x -> s { condTestSuites = x }) (f (condTestSuites s))

instance L.HasBenchmarks GenericPackageDescription where
  traverseBenchmarks = lens . traverse . L._2 . traverse
    where lens f s = fmap (\x -> s { condBenchmarks = x }) (f (condBenchmarks s))

instance L.IsPackageDescription GenericPackageDescription where
  lensSpecVersion f s = fmap (\x -> s { specVersionRaw = Left x })
                             (f $ lowerSpecVersion $ specVersionRaw s)
  {-# INLINE lensSpecVersion #-}
  
  lensLicense f s = fmap (\x -> s { licenseRaw = Left x })
                         (f $ lowerLicense $ licenseRaw s)
  {-# INLINE lensLicense #-}
  
  lensBuildType f s = fmap (\x -> s { buildTypeRaw = Just x })
                           (f $ lowerBuildType s)
  {-# INLINE lensBuildType #-}

  traversePublicLib = lens . traverse . traverse
    where lens f s = fmap (\x -> s { condLibrary = x }) (f (condLibrary s))
  {-# INLINE traversePublicLib #-}

  traverseSubLibs = lens . traverse . L._2 . traverse
    where lens f s = fmap (\x -> s { condSubLibraries = x }) (f (condSubLibraries s))
  {-# INLINE traverseSubLibs #-}


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
  deriving (Binary, Generic, NFData)

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
