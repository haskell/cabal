{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.PackageCollection
-- Copyright   :  (c) Duncan Coutts 2015, 2016
-- License     :  BSD-like
--
-- Maintainer  :  duncan@community.haskell.org
--
-- Support for package collections in cabal
--
-----------------------------------------------------------------------------
module Distribution.Client.PackageCollection {-(
    -- * Package collections
    -- $introduction

    -- * Implementation overview
    -- $overview

    -- * Syntax
    PkgCollectionId,
    PkgCollectionName,

    PkgCollectionFile,
    PkgCollectionMember(..),
    PkgCollectionConstraint(..),

    PkgCollectionExpr,
    PkgCollection,
    makePkgCollection,
    flattenPkgCollectionExpr,

    -- * Tests
    prop_expr_sem,
  )-} where

import Distribution.Package
import Distribution.PackageDescription (FlagName(..), FlagAssignment)
import Distribution.Version
import Distribution.Text

import Data.Map (Map)
import qualified Data.Map as Map

import qualified Distribution.Compat.ReadP as Parse
import Distribution.Compat.ReadP ((+++), (<++))
import qualified Text.PrettyPrint as Disp
import Text.PrettyPrint ((<>), (<+>))
import qualified Data.Char as Char
    ( isDigit, isAlphaNum, isSpace )
import Data.List ( foldl', intercalate )
import Data.Maybe (catMaybes)
import Control.Applicative

#ifdef TESTS
import Test.QuickCheck
#endif


-- $introduction
--
-- To a first approximation package collections are sets of constraints that
-- are used during solving. They can be used for several purposes including:
--
-- * defining stable sets of packages that work together where there is at most
--   one version of each package (think debian stable);
--
-- * defining sets of packages that are known to work individually, though not
--   necessarily all together (e.g. packages that build with ghcjs-8.0);
--
-- * selecting some set of packages that share some desirable or undesirable
--   property (e.g. use the GPL3 license or are deprecated);
--
-- * tightening lax dependency constraints in package descriptions.
--
-- Package collections themselves just specify which package etc they are
-- talking about. How they are interpreted by clients such as cabal depends on
-- their configuration (e.g. project config). Collections can be combined and
-- have their interpretation altered by various operators:
--
-- * conjunction: where constraints from two collections apply to the same
--   package, both constraints must be satisfied;
--
-- * disjunction: where constraints from two collections apply to the same
--   package, one or the other must be satisfied;
--
-- * negation: each constraint is negated, so version ranges and flag values
--   are negated;
--
-- * weakening: each hard constraint is replaced by its equivalent soft
--   constraint i.e. preference
--
-- These four operators all work point-wise. There is also an operator:
--
-- * exclusive: packages not mentioned by any constraint are not allowed, i.e.
--   a constraint disallowing all versions.
--
-- These operators allow combinations such as \"all these packages, except for
-- those ones\" by taking the conjunction of first collection with the negation
-- of the second. It allows policies such as trying to use a collection if
-- possible (weakening), or using only packages from a vetted list (exclusive).


-- $overview
--
-- This module defines:
--
-- * the external syntax, parser and printer;
--
-- * the abstract syntax;
--
-- * a high level semantics for collections and their operations, to use as a
--   spec and in tests;
--
-- * a concrete representation for use in collection expressions;
--
-- * conversion from the abstract syntax of individual collections into the
--   representation for collection expressions;
--
-- * the collection operations;
--
-- * sanity checking and error reporting;
--
-- * conversion of collection expression results into constraints for the
--   solver;
--
-- * tests (conditionally with CPP)


------------------------------------
-- Syntax for building collections
--

-- | The abstract syntax of a package collection file.
--
-- Package collection files are text files, with one member specification per
-- line. e.g.
--
-- > foo-1.0
-- > bar-1.2
--
type PkgCollectionFile = [PkgCollectionMember]

-- | Package collections are named and carry a version number since they
-- are typically revised over time. This id is what is used by users (e.g. in
-- project files) to refer to a collection.
--
data PkgCollectionId = PkgCollectionId !PkgCollectionName !Version
  deriving (Eq, Ord, Show)

-- | A package collection name.
--
newtype PkgCollectionName = PkgCollectionName String
  deriving (Eq, Ord, Show)

-- | A single member of a package collection file.
--
-- Each member corresponds to a constraint. Most collections consist of
-- constraints of the form @foo == 1.0@ meaning that /if/ the package @foo@
-- is used, its version must be 1.0. This is because most collections are
-- intended to nail down the versions of a related set of packages that are
-- known to work together. This kind of collection may also need to nail down
-- some or all of the flags associated with the packages, and flag constraints
-- like @foo +feature1 -feature2@ can also be used.
--
-- Other special purpose collections use constraints of other forms. A
-- collection can specify a whole package @foo@, including all versions. This
-- does not constrain the version but can be useful if the collection is used
-- to exclude a set of packages, or to pick /exclusively/ from a set of
-- packages.
--
-- Version range constraints on a package, e.g. @foo >= 1.1 && < 2@, can make
-- sense if nailing down a version is too much and some flexibility is needed,
-- or for example to exclude certain problematic versions of a package.
--
-- All of these constrains apply globally or unconditionally, in that they
-- are always in force and must always be respected. There is another style of
-- collection that is essentially to edit or correct the constraints in
-- existing package descriptions. Typically this would be to tighten a lower
-- or upper bound. Clearly these constraints cannot apply globally, they are
-- per-package adjustments. These forms of constraint apply only if a
-- particular package version (or any version) is chosen. Syntactically they
-- are written as an implication @foo-1.0 => bar >= 1.1 && < 2@. It is typical
-- that @foo@ directly depends on @bar@ but this is not essential and there
-- are plausible use cases where this may be useful.
--
data PkgCollectionMember =
     -- | A \"global\" constraint that applies unconditionally.
     PkgCollGlobalConstraint PkgCollectionConstraint

     -- | An \"implication\" constraint that applies only if a specific package
     -- version (or all versions) is chosen.
   | PkgCollPerPkgConstraint PackageName (Maybe Version) --TODO: generalise to VersionRange?
                             PkgCollectionConstraint
  deriving (Eq, Show)

-- | A constraint on a package, as part of a global or implication constraint
-- in a package collection.
--
data PkgCollectionConstraint =
     -- | @foo@ includes any version of the package
     PkgAnyVersion     PackageName
     -- | @foo-1.0@ includes exactly the given version of the package
   | PkgSingleVersion  PackageName Version 
     -- | @foo < 2@ includes all versions of the package in the given range
   | PkgVersionRange   PackageName VersionRange
     -- | @foo +this -that@ means the package flags must use the values given
   | PkgFlagAssignment PackageName FlagAssignment
  deriving (Eq, Show)


instance Text PkgCollectionName where
  disp (PkgCollectionName n) = Disp.text n
  parse = do
    ns <- Parse.sepBy1 component (Parse.char '-')
    return (PkgCollectionName (intercalate "-" ns))
    where
      component = do
        cs <- Parse.munch1 Char.isAlphaNum
        if all Char.isDigit cs then Parse.pfail else return cs
        -- each component must contain an alphabetic character, to avoid
        -- ambiguity in identifiers like foo-1 (the 1 is the version number).

instance Text PkgCollectionId where
  disp (PkgCollectionId n v) = disp n <> Disp.char '-' <> disp v

  parse = do
    n <- parse
    v <- Parse.char '-' >> parse
    return (PkgCollectionId n v)

instance Text PkgCollectionConstraint where
  disp (PkgAnyVersion     pkgname)          = disp pkgname
  disp (PkgSingleVersion  pkgname version)  = disp pkgname
                                           <> Disp.char '-' <> disp version
  disp (PkgVersionRange   pkgname verrange) = disp pkgname
                                          <+> disp verrange
  disp (PkgFlagAssignment pkgname flags)    = disp pkgname
                                          <+> dispFlagAssignment flags
    where
      dispFlagAssignment = Disp.hsep . map dispFlagValue
      dispFlagValue (f, True)   = Disp.char '+' <> dispFlagName f
      dispFlagValue (f, False)  = Disp.char '-' <> dispFlagName f
      dispFlagName (FlagName f) = Disp.text f

  parse = do
      pkgname <- parse
      parseDetails pkgname
    where
      spaces = Parse.satisfy Char.isSpace >> Parse.skipSpaces

      parseDetails pkgname =
           ((do _ <- Parse.char '-'
                v <- parse
                return (PkgSingleVersion pkgname v))
        +++ (do vr <- parse
                return (PkgVersionRange pkgname vr))
        +++ (do fs <- parseFlagAssignment
                return (PkgFlagAssignment pkgname fs)))
        <++ (return (PkgAnyVersion pkgname))

      parseFlagAssignment = Parse.many1 (spaces >> parseFlagValue)
      parseFlagValue =
            (do _ <- Parse.char '+'
                f <- parseFlagName
                return (f, True))
        +++ (do _ <- Parse.char '-'
                f <- parseFlagName
                return (f, False))
      parseFlagName = liftA FlagName ident

      ident :: Parse.ReadP r String
      ident = Parse.munch1 identChar >>= \s -> check s >> return s
        where
          identChar c   = Char.isAlphaNum c || c == '_' || c == '-'
          check ('-':_) = Parse.pfail
          check _       = return ()

instance Text PkgCollectionMember where
  disp (PkgCollGlobalConstraint constraint) = disp constraint
  disp (PkgCollPerPkgConstraint pkgname Nothing constraint) =
        disp pkgname
    <+> Disp.text "=>"
    <+> disp constraint
  disp (PkgCollPerPkgConstraint pkgname (Just version) constraint) =
        disp pkgid
    <+> Disp.text "=>"
    <+> disp constraint
    where
      pkgid = PackageIdentifier pkgname version

  parse = (do pkgname <- parse
              Parse.skipSpaces
              _ <- Parse.string "=>"
              Parse.skipSpaces
              constraint <- parse
              return (PkgCollPerPkgConstraint pkgname Nothing constraint))
      +++ (do PackageIdentifier pkgname pkgver <- parse
              Parse.skipSpaces
              _ <- Parse.string "=>"
              Parse.skipSpaces
              constraint <- parse
              return (PkgCollPerPkgConstraint pkgname (Just pkgver) constraint))
      +++ (do constraint <- parse
              return (PkgCollGlobalConstraint constraint))


------------------------------------
-- Syntax for using collections
--

-- | A package collection is referred to by it's name and optionally also by
-- its specific version. If a specific version is not given then it is obtained
-- from the context, e.g. the latest available version.
--
data PkgCollectionSpecifier
   = PkgCollectionSpecifier !PkgCollectionName !(Maybe Version)
  deriving (Eq, Show)

-- | A package collection expression.
--
-- When using a package collection the user defines what combination of
-- collections to use and any additional interpretation such as to use a
-- collection as soft preferences or use it exclusively.
--
-- This type is used for expressions over different base types, including
-- 'PkgCollectionSpecifier' which then corresponds to the abstract syntax for
-- a package collection expression, or at 'PkgCollection' which gives the
-- actual value of a collection.
--
data PkgCollectionExpr a =
      SinglePkgCollection    a
    | UnionPkgCollection     (PkgCollectionExpr a) (PkgCollectionExpr a)
    | IntersectPkgCollection (PkgCollectionExpr a) (PkgCollectionExpr a)
    | InvertPkgCollection    (PkgCollectionExpr a)
    | PreferPkgCollection    (PkgCollectionExpr a)
    | ExclusivePkgCollection (PkgCollectionExpr a)
  deriving (Eq, Show)

instance Functor PkgCollectionExpr where
  fmap f = foldPkgCollectionExpr
             (SinglePkgCollection . f)
             UnionPkgCollection
             IntersectPkgCollection
             InvertPkgCollection
             PreferPkgCollection
             ExclusivePkgCollection

-- | The natural fold for the 'PkgCollectionExpr'.
--
foldPkgCollectionExpr :: (a -> b)       -- ^ single pkg collection 
                      -> (b -> b -> b)  -- ^ union
                      -> (b -> b -> b)  -- ^ intersection
                      -> (b -> b)       -- ^ invert
                      -> (b -> b)       -- ^ prefer
                      -> (b -> b)       -- ^ exclusive
                      -> PkgCollectionExpr a
                      -> b
foldPkgCollectionExpr single union intersect invert prefer exclude = fold
  where
    fold (SinglePkgCollection    p)   = single p
    fold (UnionPkgCollection     a b) = union     (fold a) (fold b)
    fold (IntersectPkgCollection a b) = intersect (fold a) (fold b)
    fold (InvertPkgCollection    a)   = invert    (fold a)
    fold (PreferPkgCollection    a)   = prefer    (fold a)
    fold (ExclusivePkgCollection a)   = exclude   (fold a)


instance Text PkgCollectionSpecifier where
  disp (PkgCollectionSpecifier n Nothing)  = disp n
  disp (PkgCollectionSpecifier n (Just v)) = disp n <> Disp.char '-' <> disp v

  parse = do
    n  <- parse
    mv <- Parse.option Nothing (Parse.char '-' >> fmap Just parse)
    return (PkgCollectionSpecifier n mv)

instance Text a => Text (PkgCollectionExpr a) where
  disp = fst
       . foldPkgCollectionExpr
           -- paired with precedence
           (\c   -> (disp c, 0 :: Int))
           (\(r1, p1) (r2, p2) -> (punct 2 p1 r1 <+> Disp.text "||" <+> punct 2 p2 r2 , 2))
           (\(r1, p1) (r2, p2) -> (punct 1 p1 r1 <+> Disp.text "&&" <+> punct 1 p2 r2 , 1))
           (\(r,  p) -> (Disp.text "!"            <> punct 0 p r , 0))
           (\(r,  p) -> (Disp.text "prefer"      <+> punct 0 p r , 1))
           (\(r,  p) -> (Disp.text "exclusive"   <+> punct 0 p r , 1))
    where
      punct p p' | p < p'    = Disp.parens
                 | otherwise = id

  parse = expr
    where
      expr   = do Parse.skipSpaces
                  t <- term
                  Parse.skipSpaces
                  (do _  <- Parse.string "||"
                      Parse.skipSpaces
                      e <- expr
                      return (UnionPkgCollection t e)
                   +++
                   return t)
      term   = do f <- factor
                  Parse.skipSpaces
                  (do _  <- Parse.string "&&"
                      Parse.skipSpaces
                      t <- term
                      return (IntersectPkgCollection f t)
                   +++
                   return f)
      factor = do _ <- Parse.string "prefer"
                  Parse.skipSpaces
                  t <- atom
                  return (PreferPkgCollection t)
           +++ do _ <- Parse.string "exclusive"
                  Parse.skipSpaces
                  t <- atom
                  return (ExclusivePkgCollection t)
           +++ atom

      atom   = parens expr
           +++ do _ <- Parse.char '!' 
                  Parse.skipSpaces
                  t <- atom
                  return (InvertPkgCollection t)
           +++ do c <- parse
                  Parse.skipSpaces
                  return (SinglePkgCollection c)

      parens p = Parse.between (Parse.char '(' >> Parse.skipSpaces)
                               (Parse.char ')' >> Parse.skipSpaces)
                               (do a <- p
                                   Parse.skipSpaces
                                   return a)

---------------------------------
-- The semantics of collections
--

-- | An plausible domain for the meaning of package collections.
--
-- The interpretation is thus: you give me a package id and I'll tell you if
-- you're allowed it at all (hard constraint), if you should avoid it (soft
-- constraint); and if you further ask about a flag I'll tell you if it must
-- be assigned a particular value.
--
type PkgCollectionSem = PackageId -> (Allowed, -- hard constraint
                                      Allowed, -- soft constraint (prefs)
                                      FlagName -> FlagConstraint)

data Allowed = F  -- ^ False, not allowed
             | I  -- ^ Id,   don't care (identity to && and ||)
             | T  -- ^ True, allowed
  deriving (Eq, Ord, Enum, Show)

data FlagConstraint    = FlagValueAny    -- bottom: v == True || v == False
                       | FlagValue Bool  -- v'    : v == v'
                       | FlagValueNone   -- top   : v == True && v == False
  deriving (Eq, Show)

emptyPkgCollectionSem :: PkgCollectionSem
emptyPkgCollectionSem = \_ -> (I, I, \_ -> FlagValueAny)

unionAllowed :: Allowed -> Allowed -> Allowed
unionAllowed x I = x
unionAllowed I y = y
unionAllowed x y = max x y

intersectAllowed :: Allowed -> Allowed -> Allowed
intersectAllowed x I = x
intersectAllowed I y = y
intersectAllowed x y = min x y

invertAllowed :: Allowed -> Allowed
invertAllowed F = T
invertAllowed I = I
invertAllowed T = F

exclusiveAllowed :: Allowed -> Allowed
exclusiveAllowed F = F
exclusiveAllowed I = F
exclusiveAllowed T = T

-- | The semantics of a 'PkgCollectionExpr' in terms of the semantics of
-- base collections.
--
pkgCollectionExprSem :: PkgCollectionExpr PkgCollectionSem
                     -> PkgCollectionSem
pkgCollectionExprSem expr pkgid =
    foldPkgCollectionExpr
      -- individual collection
      (\sem -> sem pkgid)
      -- union collections
      (\(c1,p1,fs1)
        (c2,p2,fs2) -> (unionAllowed     c1 c2,
                        unionAllowed     p1 p2,
                        unionFlagConstraint <$> fs1 <*> fs2))
      -- intersect collections
      (\(c1,p1,fs1)
        (c2,p2,fs2) -> (intersectAllowed c1 c2,
                        intersectAllowed p1 p2,
                        intersectFlagConstraint <$> fs1 <*> fs2))
      -- invert collection
      (\(c, p, fs)  -> (invertAllowed c,
                        invertAllowed p,
                        invertFlagConstraint <$> fs))
      -- make collection merely a preference
      (\(c, p, fs)  -> (I,
                        intersectAllowed c p,
                        fs))
      -- make collection exclusive
      (\(c, p, fs)  -> (exclusiveAllowed c,
                        exclusiveAllowed p,
                        fs))
      expr


-------------------
-- representation

data PkgCollection
   = PkgCollection
       !Allowed                             -- ^ Hard constraints, not mentioned explicitly
       !(Map PackageName VersionConstraint) -- ^ Hard constraints, mentioned explicitly
       !(Map PackageName VersionConstraint) -- ^ Soft constraints, mentioned explicitly
       !(Map PackageName FlagConstraints)   -- ^ Flag constraints, mentioned explicitly
    -- !(Map PackageId (...))
  deriving (Show)

data VersionConstraint = NoVersionConstraint
                       | VersionConstraint VersionRange
                       | AllVersionsExcluded
  deriving (Show)

type FlagConstraints   = Map FlagName FlagConstraint

lookupInPkgCollection :: PkgCollection
                      -> PackageName
                      -> (VersionConstraint,
                          VersionConstraint,
                          FlagAssignment)
lookupInPkgCollection (PkgCollection other vcs vps fcs) pkgname =
    (versionConstraint, versionPreference, flagAssignment)
  where
    versionConstraint = case Map.lookup pkgname vcs of
                          Just vr        -> vr
                          Nothing
                            | other == F -> AllVersionsExcluded
                            | otherwise  -> NoVersionConstraint

    versionPreference = case Map.lookup pkgname vps of
                          Just vr        -> vr
                          Nothing        -> NoVersionConstraint

    flagAssignment    = case Map.lookup pkgname fcs of
                          Nothing   -> []
                          Just pfcs -> [ (fn, fv)
                                       | (fn, fc) <- Map.toList pfcs
                                       , fv       <- flagAssignmentValue fc ]
 
    -- any is possible, no constraint
    flagAssignmentValue  FlagValueAny  = []
    flagAssignmentValue (FlagValue v)  = [v]
    -- none is possible, must simultaneously be True and False
    flagAssignmentValue  FlagValueNone = [False, True]


unionPkgCollection :: PkgCollection -> PkgCollection -> PkgCollection
unionPkgCollection (PkgCollection other1 vcs1 vps1 fcs1)
                   (PkgCollection other2 vcs2 vps2 fcs2) =
    PkgCollection
      (unionAllowed other1 other2)
      (Map.unionWith unionVersionConstraint vcs1 vcs2)
      (Map.unionWith unionVersionConstraint vps1 vps2)
      (Map.unionWith (Map.unionWith unionFlagConstraint) fcs1 fcs2)

intersectPkgCollection :: PkgCollection -> PkgCollection -> PkgCollection
intersectPkgCollection (PkgCollection other1 vcs1 vps1 fcs1)
                       (PkgCollection other2 vcs2 vps2 fcs2) =
    PkgCollection
      (intersectAllowed other1 other2)
      (Map.unionWith intersectVersionConstraint vcs1 vcs2)
      (Map.unionWith intersectVersionConstraint vps1 vps2)
      (Map.unionWith (Map.unionWith intersectFlagConstraint) fcs1 fcs2)

invertPkgCollection :: PkgCollection -> PkgCollection
invertPkgCollection (PkgCollection other vcs vps fcs) =
    PkgCollection
      (invertAllowed other)
      (Map.map invertVersionConstraint vcs)
      (Map.map invertVersionConstraint vps)
      (Map.map (Map.map invertFlagConstraint) fcs)

preferPkgCollection :: PkgCollection -> PkgCollection
preferPkgCollection (PkgCollection other vcs vps fcs) =
    PkgCollection
      other
      Map.empty
      (Map.unionWith intersectVersionConstraint vcs vps)
      fcs

exclusivePkgCollection :: PkgCollection -> PkgCollection
exclusivePkgCollection (PkgCollection other vcs vps fcs) =
    PkgCollection (exclusiveAllowed other) vcs vps fcs


unionVersionConstraint :: VersionConstraint -> VersionConstraint -> VersionConstraint
unionVersionConstraint NoVersionConstraint _   = NoVersionConstraint
unionVersionConstraint _   NoVersionConstraint = NoVersionConstraint
unionVersionConstraint AllVersionsExcluded vc2 = vc2
unionVersionConstraint vc1 AllVersionsExcluded = vc1
unionVersionConstraint (VersionConstraint vr1) (VersionConstraint vr2) =
    VersionConstraint (unionVersionRanges vr1 vr2)

intersectVersionConstraint :: VersionConstraint -> VersionConstraint -> VersionConstraint
intersectVersionConstraint NoVersionConstraint vc2 = vc2
intersectVersionConstraint vc1 NoVersionConstraint = vc1
intersectVersionConstraint AllVersionsExcluded _   = AllVersionsExcluded
intersectVersionConstraint _   AllVersionsExcluded = AllVersionsExcluded
intersectVersionConstraint (VersionConstraint vr1) (VersionConstraint vr2) =
    VersionConstraint (intersectVersionRanges vr1 vr2)

invertVersionConstraint :: VersionConstraint -> VersionConstraint
invertVersionConstraint NoVersionConstraint    = AllVersionsExcluded
invertVersionConstraint (VersionConstraint vr) = VersionConstraint (invertVersionRange vr)
invertVersionConstraint AllVersionsExcluded    = NoVersionConstraint


unionFlagConstraint :: FlagConstraint -> FlagConstraint -> FlagConstraint
unionFlagConstraint FlagValueAny   _             = FlagValueAny
unionFlagConstraint _              FlagValueAny  = FlagValueAny
unionFlagConstraint (FlagValue a)  (FlagValue b)
                                     | a == b    = FlagValue a
                                     | otherwise = FlagValueAny
unionFlagConstraint (FlagValue a)  FlagValueNone = FlagValue a
unionFlagConstraint FlagValueNone (FlagValue b)  = FlagValue b
unionFlagConstraint FlagValueNone FlagValueNone  = FlagValueNone

intersectFlagConstraint :: FlagConstraint -> FlagConstraint -> FlagConstraint
intersectFlagConstraint FlagValueNone _              = FlagValueNone
intersectFlagConstraint _              FlagValueNone = FlagValueNone
intersectFlagConstraint (FlagValue a) (FlagValue b)
                                        | a == b     = FlagValue a
                                        | otherwise  = FlagValueNone
intersectFlagConstraint (FlagValue a)  FlagValueAny  = FlagValue a
intersectFlagConstraint  FlagValueAny (FlagValue b)  = FlagValue b
intersectFlagConstraint  FlagValueAny  FlagValueAny  = FlagValueAny

invertFlagConstraint :: FlagConstraint -> FlagConstraint
invertFlagConstraint (FlagValue a) = FlagValue (not a)
invertFlagConstraint fc            = fc

pkgCollectionSem :: PkgCollection -> PkgCollectionSem
pkgCollectionSem (PkgCollection other vcs vps fcs) pkgid =
    (hardConstraint, softConstraint, flagConstraints)
  where
    hardConstraint  = case Map.lookup (packageName pkgid) vcs of
                        Just NoVersionConstraint                     -> T
                        Just AllVersionsExcluded                     -> F
                        Just (VersionConstraint vr)
                          | packageVersion pkgid `withinRange` vr    -> T
                          | otherwise                                -> F
                        Nothing                                      -> other
    softConstraint  = case Map.lookup (packageName pkgid) vps of
                        Just NoVersionConstraint                     -> T
                        Just AllVersionsExcluded                     -> F -- no difference
                        Just (VersionConstraint vr)
                          | packageVersion pkgid `withinRange` vr    -> T
                          | otherwise                                -> F
                        Nothing                                      -> I
    flagConstraints = case Map.lookup (packageName pkgid) fcs of
                        Just fs -> \fn -> case Map.lookup fn fs of
                                   Just fc -> fc
                                   Nothing -> FlagValueAny
                        Nothing -> const FlagValueAny

pkgCollectionExprSem' :: PkgCollectionExpr PkgCollection
                      -> PkgCollectionSem
pkgCollectionExprSem' =
  pkgCollectionExprSem . fmap pkgCollectionSem

flattenPkgCollectionExpr :: PkgCollectionExpr PkgCollection
                         -> PkgCollection
flattenPkgCollectionExpr =
    foldPkgCollectionExpr
      id
      unionPkgCollection
      intersectPkgCollection
      invertPkgCollection
      preferPkgCollection
      exclusivePkgCollection


substPkgCollectionExpr :: Ord a => Map a b
                       -> PkgCollectionExpr a
                       -> Either a (PkgCollectionExpr b)
substPkgCollectionExpr m =
    foldPkgCollectionExpr
      (\k -> case Map.lookup k m of
               Nothing -> Left k
               Just v  -> Right (SinglePkgCollection v))
      (liftA2 UnionPkgCollection)
      (liftA2 IntersectPkgCollection)
      (liftA  InvertPkgCollection)
      (liftA  PreferPkgCollection)
      (liftA  ExclusivePkgCollection)


makePkgCollection :: [PkgCollectionMember] -> PkgCollection
makePkgCollection members =
    PkgCollection I
      (Map.fromListWith unionVersionConstraint
                        (catMaybes (map toVerCs  members)))
      Map.empty
      (Map.fromListWith (Map.unionWith unionFlagConstraint)
                        (catMaybes (map toFlagCs members)))
  where
    toVerCs (PkgCollGlobalConstraint (PkgAnyVersion pkgname))      = Just (pkgname, NoVersionConstraint)
    toVerCs (PkgCollGlobalConstraint (PkgSingleVersion pkgname v)) = Just (pkgname, VersionConstraint (thisVersion v))
    toVerCs (PkgCollGlobalConstraint (PkgVersionRange pkgname vr)) = Just (pkgname, VersionConstraint vr)
    toVerCs (PkgCollGlobalConstraint (PkgFlagAssignment _ _))      = Nothing
  --toVerCs (PkgCollPerPkgConstraint _)

    toFlagCs (PkgCollGlobalConstraint (PkgFlagAssignment pkgname flags)) = Just (pkgname, flagCs)
      where
        flagCs = Map.fromListWith intersectFlagConstraint
                                  [ (fn, FlagValue fv) | (fn, fv) <- flags ]
    toFlagCs (PkgCollGlobalConstraint _) = Nothing
  --toFlagCs (PkgCollPerPkgConstraint _) =


#if TESTS
mkPkgColl :: [String] -> PkgCollection
mkPkgColl strs = makePkgCollection [ p | str <- strs
                                   , let Just p = simpleParse str ]

empty, gpl, broken, broken', platform :: PkgCollection
empty = mkPkgColl []

gpl = mkPkgColl ["cpphs"]
-- cpphs -any
-- cpphs none

broken = mkPkgColl ["cpphs +foo"]
broken' = mkPkgColl ["cpphs -foo"]

platform = mkPkgColl ["bytestring-0.10.6.0", "cpphs-1.0"]
#endif

-----------------------------------------------------------------------------


#if TESTS
instance Arbitrary a => Arbitrary (PkgCollectionExpr a) where
    arbitrary =
      sized $ \sz ->
        frequency
          [ (3, SinglePkgCollection <$> arbitrary)
          , (1, UnionPkgCollection  <$> resize (sz `div` 2) arbitrary
                                    <*> resize (sz `div` 2) arbitrary)
          , (1, IntersectPkgCollection <$> resize (sz `div` 2) arbitrary
                                       <*> resize (sz `div` 2) arbitrary)
          , (1, InvertPkgCollection    <$> arbitrary)
          , (1, PreferPkgCollection    <$> arbitrary)
          , (1, ExclusivePkgCollection <$> arbitrary)
          ]
#endif

prop_expr_sem :: PkgCollectionExpr [PkgCollectionMember] -> Bool
prop_expr_sem expr =

    let expr' :: PkgCollectionExpr PkgCollectionSem
        expr' = fmap membersSem expr

        coll' :: PkgCollectionSem
        coll' = pkgCollectionExprSem expr'

        expr'' :: PkgCollectionExpr PkgCollection
        expr'' = fmap makePkgCollection expr

        coll :: PkgCollectionSem
        coll = pkgCollectionSem (flattenPkgCollectionExpr expr'')

     in equalPkgCollectionSem coll coll' [] --TODO: use useful set of pkgs

equalPkgCollectionSem :: PkgCollectionSem -> PkgCollectionSem
                      -> [(PackageId, [FlagName])]
                      -> Bool
equalPkgCollectionSem sem sem' pkgs =
    and
      [ c == c' && p == p' &&
        and [ fl flag == fl' flag | flag <- flags ]
      | (pkgid, flags) <- pkgs
      , let (c, p, fl ) = sem  pkgid
            (c',p',fl') = sem' pkgid
      ]

membersSem :: [PkgCollectionMember] -> PkgCollectionSem
membersSem members =
  \pkgid -> ( foldl' unionAllowed I
                [ pkgCollectionMemberSem member pkgid
                | PkgCollGlobalConstraint member <- members ]
                --TODO: PkgCollPerPkgConstraint
            , I
            , \flagname ->
              foldl1 unionFlagConstraint
                [ pkgCollectionMemberFlagSem member pkgid flagname
                | PkgCollGlobalConstraint member <- members ] )
                --TODO: PkgCollPerPkgConstraint
  where

    pkgCollectionMemberSem :: PkgCollectionConstraint -> PackageId -> Allowed
    pkgCollectionMemberSem (PkgAnyVersion    pkgname)    pkgid
      | packageName pkgid == pkgname = T
      | otherwise                    = I
    pkgCollectionMemberSem (PkgSingleVersion pkgname v)  pkgid
      | packageName pkgid == pkgname
      , packageVersion pkgid == v    = T
      | packageName pkgid == pkgname = F
      | otherwise                    = I
    pkgCollectionMemberSem (PkgVersionRange  pkgname vr) pkgid
      | packageName pkgid == pkgname
      , packageVersion pkgid `withinRange` vr = T
      | packageName pkgid == pkgname          = F
      | otherwise                             = I
    pkgCollectionMemberSem (PkgFlagAssignment _ _) _ = I

    pkgCollectionMemberFlagSem :: PkgCollectionConstraint -> PackageId -> FlagName -> FlagConstraint
    pkgCollectionMemberFlagSem (PkgFlagAssignment pkgname flags) pkgid flagname
      | packageName pkgid == pkgname
      , Just v <- lookup flagname flags
      = FlagValue v

    pkgCollectionMemberFlagSem _ _ _ = FlagValueAny
