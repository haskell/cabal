{-# LANGUAGE DeriveGeneric #-}

module Distribution.Client.Types.AllowNewer
  ( AllowNewer (..)
  , AllowOlder (..)
  , RelaxDeps (..)
  , mkRelaxDepSome
  , RelaxDepMod (..)
  , RelaxDepScope (..)
  , RelaxDepSubject (..)
  , RelaxedDep (..)
  , isRelaxDeps
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Distribution.Parsec (parsecLeadingCommaNonEmpty)
import Distribution.Types.PackageId (PackageId, PackageIdentifier (..))
import Distribution.Types.PackageName (PackageName, mkPackageName)
import Distribution.Types.Version (nullVersion)

import qualified Distribution.Compat.CharParsing as P
import qualified Text.PrettyPrint as Disp

-- $setup
-- >>> import Distribution.Parsec

-- TODO: When https://github.com/haskell/cabal/issues/4203 gets tackled,
-- it may make sense to move these definitions to the Solver.Types
-- module

-- | 'RelaxDeps' in the context of upper bounds (i.e. for @--allow-newer@ flag)
newtype AllowNewer = AllowNewer {unAllowNewer :: RelaxDeps}
  deriving (Eq, Read, Show, Generic)

-- | 'RelaxDeps' in the context of lower bounds (i.e. for @--allow-older@ flag)
newtype AllowOlder = AllowOlder {unAllowOlder :: RelaxDeps}
  deriving (Eq, Read, Show, Generic)

-- | Generic data type for policy when relaxing bounds in dependencies.
-- Don't use this directly: use 'AllowOlder' or 'AllowNewer' depending
-- on whether or not you are relaxing an lower or upper bound
-- (respectively).
data RelaxDeps
  = -- | Ignore upper (resp. lower) bounds in some (or no) dependencies on the given packages.
    --
    -- @RelaxDepsSome []@ is the default, i.e. honor the bounds in all
    -- dependencies, never choose versions newer (resp. older) than allowed.
    RelaxDepsSome [RelaxedDep]
  | -- | Ignore upper (resp. lower) bounds in dependencies on all packages.
    --
    -- __Note__: This is should be semantically equivalent to
    --
    -- > RelaxDepsSome [RelaxedDep RelaxDepScopeAll RelaxDepModNone RelaxDepSubjectAll]
    --
    -- (TODO: consider normalising 'RelaxDeps' and/or 'RelaxedDep')
    RelaxDepsAll
  deriving (Eq, Read, Show, Generic)

-- | Dependencies can be relaxed either for all packages in the install plan, or
-- only for some packages.
data RelaxedDep = RelaxedDep !RelaxDepScope !RelaxDepMod !RelaxDepSubject
  deriving (Eq, Read, Show, Generic)

-- | Specify the scope of a relaxation, i.e. limit which depending
-- packages are allowed to have their version constraints relaxed.
data RelaxDepScope
  = -- | Apply relaxation in any package
    RelaxDepScopeAll
  | -- | Apply relaxation to in all versions of a package
    RelaxDepScopePackage !PackageName
  | -- | Apply relaxation to a specific version of a package only
    RelaxDepScopePackageId !PackageId
  deriving (Eq, Read, Show, Generic)

-- | Modifier for dependency relaxation
data RelaxDepMod
  = -- | Default semantics
    RelaxDepModNone
  | -- | Apply relaxation only to @^>=@ constraints
    RelaxDepModCaret
  deriving (Eq, Read, Show, Generic)

-- | Express whether to relax bounds /on/ @all@ packages, or a single package
data RelaxDepSubject
  = RelaxDepSubjectAll
  | RelaxDepSubjectPkg !PackageName
  deriving (Eq, Ord, Read, Show, Generic)

instance Pretty RelaxedDep where
  pretty (RelaxedDep scope rdmod subj) = case scope of
    RelaxDepScopeAll -> Disp.text "*:" Disp.<> modDep
    RelaxDepScopePackage p0 -> pretty p0 Disp.<> Disp.colon Disp.<> modDep
    RelaxDepScopePackageId p0 -> pretty p0 Disp.<> Disp.colon Disp.<> modDep
    where
      modDep = case rdmod of
        RelaxDepModNone -> pretty subj
        RelaxDepModCaret -> Disp.char '^' Disp.<> pretty subj

instance Parsec RelaxedDep where
  parsec = P.char '*' *> relaxedDepStarP <|> (parsec >>= relaxedDepPkgidP)

-- continuation after *
relaxedDepStarP :: CabalParsing m => m RelaxedDep
relaxedDepStarP =
  RelaxedDep RelaxDepScopeAll <$ P.char ':' <*> modP <*> parsec
    <|> pure (RelaxedDep RelaxDepScopeAll RelaxDepModNone RelaxDepSubjectAll)

-- continuation after package identifier
relaxedDepPkgidP :: CabalParsing m => PackageIdentifier -> m RelaxedDep
relaxedDepPkgidP pid@(PackageIdentifier pn v)
  | pn == mkPackageName "all"
  , v == nullVersion =
      RelaxedDep RelaxDepScopeAll <$ P.char ':' <*> modP <*> parsec
        <|> pure (RelaxedDep RelaxDepScopeAll RelaxDepModNone RelaxDepSubjectAll)
  | v == nullVersion =
      RelaxedDep (RelaxDepScopePackage pn) <$ P.char ':' <*> modP <*> parsec
        <|> pure (RelaxedDep RelaxDepScopeAll RelaxDepModNone (RelaxDepSubjectPkg pn))
  | otherwise =
      RelaxedDep (RelaxDepScopePackageId pid) <$ P.char ':' <*> modP <*> parsec

modP :: P.CharParsing m => m RelaxDepMod
modP = RelaxDepModCaret <$ P.char '^' <|> pure RelaxDepModNone

instance Pretty RelaxDepSubject where
  pretty RelaxDepSubjectAll = Disp.text "*"
  pretty (RelaxDepSubjectPkg pn) = pretty pn

instance Parsec RelaxDepSubject where
  parsec = RelaxDepSubjectAll <$ P.char '*' <|> pkgn
    where
      pkgn = do
        pn <- parsec
        pure $
          if pn == mkPackageName "all"
            then RelaxDepSubjectAll
            else RelaxDepSubjectPkg pn

instance Pretty RelaxDeps where
  pretty rd | not (isRelaxDeps rd) = Disp.text "none"
  pretty (RelaxDepsSome pkgs) =
    Disp.fsep
      . Disp.punctuate Disp.comma
      . map pretty
      $ pkgs
  pretty RelaxDepsAll = Disp.text "all"

-- |
--
-- >>> simpleParsec "all" :: Maybe RelaxDeps
-- Just RelaxDepsAll
--
-- >>> simpleParsec "none" :: Maybe RelaxDeps
-- Just (RelaxDepsSome [])
--
-- >>> simpleParsec "*, *" :: Maybe RelaxDeps
-- Just RelaxDepsAll
--
-- >>> simpleParsec "*:*" :: Maybe RelaxDeps
-- Just RelaxDepsAll
--
-- >>> simpleParsec "foo:bar, quu:puu" :: Maybe RelaxDeps
-- Just (RelaxDepsSome [RelaxedDep (RelaxDepScopePackage (PackageName "foo")) RelaxDepModNone (RelaxDepSubjectPkg (PackageName "bar")),RelaxedDep (RelaxDepScopePackage (PackageName "quu")) RelaxDepModNone (RelaxDepSubjectPkg (PackageName "puu"))])
--
-- This is not a glitch, even it looks like:
--
-- >>> simpleParsec ", all" :: Maybe RelaxDeps
-- Just RelaxDepsAll
--
-- >>> simpleParsec "" :: Maybe RelaxDeps
-- Nothing
instance Parsec RelaxDeps where
  parsec = do
    xs <- parsecLeadingCommaNonEmpty parsec
    pure $ case toList xs of
      [RelaxedDep RelaxDepScopeAll RelaxDepModNone RelaxDepSubjectAll] ->
        RelaxDepsAll
      [RelaxedDep RelaxDepScopeAll RelaxDepModNone (RelaxDepSubjectPkg pn)]
        | pn == mkPackageName "none" ->
            mempty
      xs' -> mkRelaxDepSome xs'

instance Binary RelaxDeps
instance Binary RelaxDepMod
instance Binary RelaxDepScope
instance Binary RelaxDepSubject
instance Binary RelaxedDep
instance Binary AllowNewer
instance Binary AllowOlder

instance Structured RelaxDeps
instance Structured RelaxDepMod
instance Structured RelaxDepScope
instance Structured RelaxDepSubject
instance Structured RelaxedDep
instance Structured AllowNewer
instance Structured AllowOlder

-- | Return 'True' if 'RelaxDeps' specifies a non-empty set of relaxations
--
-- Equivalent to @isRelaxDeps = (/= 'mempty')@
isRelaxDeps :: RelaxDeps -> Bool
isRelaxDeps (RelaxDepsSome []) = False
isRelaxDeps (RelaxDepsSome (_ : _)) = True
isRelaxDeps RelaxDepsAll = True

-- | A smarter 'RelaxedDepsSome', @*:*@ is the same as @all@.
mkRelaxDepSome :: [RelaxedDep] -> RelaxDeps
mkRelaxDepSome xs
  | any (== RelaxedDep RelaxDepScopeAll RelaxDepModNone RelaxDepSubjectAll) xs =
      RelaxDepsAll
  | otherwise =
      RelaxDepsSome xs

-- | 'RelaxDepsAll' is the /absorbing element/
instance Semigroup RelaxDeps where
  -- identity element
  RelaxDepsSome [] <> r = r
  l@(RelaxDepsSome _) <> RelaxDepsSome [] = l
  -- absorbing element
  l@RelaxDepsAll <> _ = l
  (RelaxDepsSome _) <> r@RelaxDepsAll = r
  -- combining non-{identity,absorbing} elements
  (RelaxDepsSome a) <> (RelaxDepsSome b) = RelaxDepsSome (a ++ b)

-- | @'RelaxDepsSome' []@ is the /identity element/
instance Monoid RelaxDeps where
  mempty = RelaxDepsSome []
  mappend = (<>)

instance Semigroup AllowNewer where
  AllowNewer x <> AllowNewer y = AllowNewer (x <> y)

instance Semigroup AllowOlder where
  AllowOlder x <> AllowOlder y = AllowOlder (x <> y)

instance Monoid AllowNewer where
  mempty = AllowNewer mempty
  mappend = (<>)

instance Monoid AllowOlder where
  mempty = AllowOlder mempty
  mappend = (<>)
