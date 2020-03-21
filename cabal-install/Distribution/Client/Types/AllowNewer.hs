{-# LANGUAGE DeriveGeneric #-}
module Distribution.Client.Types.AllowNewer (
    AllowNewer (..),
    AllowOlder (..),
    RelaxDeps (..),
    RelaxDepMod (..),
    RelaxDepScope (..),
    RelaxDepSubject (..),
    RelaxedDep (..),
    isRelaxDeps,
) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Distribution.Types.PackageId   (PackageId, pkgVersion)
import Distribution.Types.PackageName (PackageName, mkPackageName)
import Distribution.Types.Version     (nullVersion)

import qualified Text.PrettyPrint as Disp

import           Distribution.Deprecated.ParseUtils (parseOptCommaList)
import qualified Distribution.Deprecated.ReadP      as Parse
import           Distribution.Deprecated.Text       (Text (..))

-- TODO: When https://github.com/haskell/cabal/issues/4203 gets tackled,
-- it may make sense to move these definitions to the Solver.Types
-- module

-- | 'RelaxDeps' in the context of upper bounds (i.e. for @--allow-newer@ flag)
newtype AllowNewer = AllowNewer { unAllowNewer :: RelaxDeps }
                   deriving (Eq, Read, Show, Generic)

-- | 'RelaxDeps' in the context of lower bounds (i.e. for @--allow-older@ flag)
newtype AllowOlder = AllowOlder { unAllowOlder :: RelaxDeps }
                   deriving (Eq, Read, Show, Generic)

-- | Generic data type for policy when relaxing bounds in dependencies.
-- Don't use this directly: use 'AllowOlder' or 'AllowNewer' depending
-- on whether or not you are relaxing an lower or upper bound
-- (respectively).
data RelaxDeps =

  -- | Ignore upper (resp. lower) bounds in some (or no) dependencies on the given packages.
  --
  -- @RelaxDepsSome []@ is the default, i.e. honor the bounds in all
  -- dependencies, never choose versions newer (resp. older) than allowed.
    RelaxDepsSome [RelaxedDep]

  -- | Ignore upper (resp. lower) bounds in dependencies on all packages.
  --
  -- __Note__: This is should be semantically equivalent to
  --
  -- > RelaxDepsSome [RelaxedDep RelaxDepScopeAll RelaxDepModNone RelaxDepSubjectAll]
  --
  -- (TODO: consider normalising 'RelaxDeps' and/or 'RelaxedDep')
  | RelaxDepsAll
  deriving (Eq, Read, Show, Generic)

-- | Dependencies can be relaxed either for all packages in the install plan, or
-- only for some packages.
data RelaxedDep = RelaxedDep !RelaxDepScope !RelaxDepMod !RelaxDepSubject
                deriving (Eq, Read, Show, Generic)

-- | Specify the scope of a relaxation, i.e. limit which depending
-- packages are allowed to have their version constraints relaxed.
data RelaxDepScope = RelaxDepScopeAll
                     -- ^ Apply relaxation in any package
                   | RelaxDepScopePackage !PackageName
                     -- ^ Apply relaxation to in all versions of a package
                   | RelaxDepScopePackageId !PackageId
                     -- ^ Apply relaxation to a specific version of a package only
                   deriving (Eq, Read, Show, Generic)

-- | Modifier for dependency relaxation
data RelaxDepMod = RelaxDepModNone  -- ^ Default semantics
                 | RelaxDepModCaret -- ^ Apply relaxation only to @^>=@ constraints
                 deriving (Eq, Read, Show, Generic)

-- | Express whether to relax bounds /on/ @all@ packages, or a single package
data RelaxDepSubject = RelaxDepSubjectAll
                     | RelaxDepSubjectPkg !PackageName
                     deriving (Eq, Ord, Read, Show, Generic)

instance Text RelaxedDep where
  disp (RelaxedDep scope rdmod subj) = case scope of
      RelaxDepScopeAll          -> Disp.text "all:"           Disp.<> modDep
      RelaxDepScopePackage   p0 -> disp p0 Disp.<> Disp.colon Disp.<> modDep
      RelaxDepScopePackageId p0 -> disp p0 Disp.<> Disp.colon Disp.<> modDep
    where
      modDep = case rdmod of
               RelaxDepModNone  -> disp subj
               RelaxDepModCaret -> Disp.char '^' Disp.<> disp subj

  parse = RelaxedDep <$> scopeP <*> modP <*> parse
    where
      -- "greedy" choices
      scopeP =           (pure RelaxDepScopeAll  <* Parse.char '*' <* Parse.char ':')
               Parse.<++ (pure RelaxDepScopeAll  <* Parse.string "all:")
               Parse.<++ (RelaxDepScopePackageId <$> pidP  <* Parse.char ':')
               Parse.<++ (RelaxDepScopePackage   <$> parse <* Parse.char ':')
               Parse.<++ (pure RelaxDepScopeAll)

      modP =           (pure RelaxDepModCaret <* Parse.char '^')
             Parse.<++ (pure RelaxDepModNone)

      -- | Stricter 'PackageId' parser which doesn't overlap with 'PackageName' parser
      pidP = do
          p0 <- parse
          when (pkgVersion p0 == nullVersion) Parse.pfail
          pure p0

instance Text RelaxDepSubject where
  disp RelaxDepSubjectAll      = Disp.text "all"
  disp (RelaxDepSubjectPkg pn) = disp pn

  parse = (pure RelaxDepSubjectAll <* Parse.char '*') Parse.<++ pkgn
    where
      pkgn = do
          pn <- parse
          pure (if (pn == mkPackageName "all")
                then RelaxDepSubjectAll
                else RelaxDepSubjectPkg pn)

instance Text RelaxDeps where
  disp rd | not (isRelaxDeps rd) = Disp.text "none"
  disp (RelaxDepsSome pkgs)      = Disp.fsep .
                                   Disp.punctuate Disp.comma .
                                   map disp $ pkgs
  disp RelaxDepsAll              = Disp.text "all"

  parse =           (const mempty        <$> ((Parse.string "none" Parse.+++
                                               Parse.string "None") <* Parse.eof))
          Parse.<++ (const RelaxDepsAll  <$> ((Parse.string "all"  Parse.+++
                                               Parse.string "All"  Parse.+++
                                               Parse.string "*")  <* Parse.eof))
          Parse.<++ (      RelaxDepsSome <$> parseOptCommaList parse)

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
isRelaxDeps (RelaxDepsSome [])    = False
isRelaxDeps (RelaxDepsSome (_:_)) = True
isRelaxDeps RelaxDepsAll          = True

-- | 'RelaxDepsAll' is the /absorbing element/
instance Semigroup RelaxDeps where
  -- identity element
  RelaxDepsSome []    <> r                   = r
  l@(RelaxDepsSome _) <> RelaxDepsSome []    = l
  -- absorbing element
  l@RelaxDepsAll      <> _                   = l
  (RelaxDepsSome   _) <> r@RelaxDepsAll      = r
  -- combining non-{identity,absorbing} elements
  (RelaxDepsSome   a) <> (RelaxDepsSome b)   = RelaxDepsSome (a ++ b)

-- | @'RelaxDepsSome' []@ is the /identity element/
instance Monoid RelaxDeps where
  mempty  = RelaxDepsSome []
  mappend = (<>)

instance Semigroup AllowNewer where
  AllowNewer x <> AllowNewer y = AllowNewer (x <> y)

instance Semigroup AllowOlder where
  AllowOlder x <> AllowOlder y = AllowOlder (x <> y)

instance Monoid AllowNewer where
  mempty  = AllowNewer mempty
  mappend = (<>)

instance Monoid AllowOlder where
  mempty  = AllowOlder mempty
  mappend = (<>)
