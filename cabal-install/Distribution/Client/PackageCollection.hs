module Distribution.Client.PackageCollection (
  
  ) where

import Distribution.Package
import Distribution.PackageDescription (FlagName(..), FlagAssignment)
import Distribution.Version
import Distribution.Text

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import qualified Distribution.Compat.ReadP as Parse
import Distribution.Compat.ReadP ((+++), (<++))
import qualified Text.PrettyPrint as Disp
import Text.PrettyPrint ((<>), (<+>), text)
import qualified Data.Char as Char
    ( isDigit, isAlphaNum, isSpace )
import Data.List ( foldl', intercalate )
import Data.Maybe (catMaybes)
import Control.Applicative

import Test.QuickCheck


newtype PkgCollectionName
      = PkgCollectionName String
  deriving (Eq, Ord, Show)

data PkgCollectionId
   = PkgCollectionId !PkgCollectionName !Version
  deriving (Eq, Ord, Show)

data PkgCollectionSpecifier
   = PkgCollectionSpecifier !PkgCollectionName !(Maybe Version)
  deriving (Eq, Show)

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

---------------
-- spec

type PkgCollectionSem = PackageId -> (Allowed, -- hard constraint
                                      Allowed, -- soft constraint (prefs)
                                      FlagName -> FlagConstraint)

data Allowed = F  -- False, not allowed
             | I  -- Id,   don't care (identity to && and ||)
             | T  -- True, allowed
  deriving (Eq, Ord, Enum, Show)

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
  deriving (Show)

data VersionConstraint = NoVersionConstraint
                       | VersionConstraint VersionRange
                       | AllVersionsExcluded
  deriving (Show)

type FlagConstraints   = Map FlagName FlagConstraint
data FlagConstraint    = FlagValueAny    -- bottom: v == True || v == False
                       | FlagValue Bool  -- v'    : v == v'
                       | FlagValueNone   -- top   : v == True && v == False
  deriving (Show)

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
                          Nothing -> []
                          Just fc -> [ (fn, fv)
                                     | (fn, fc) <- Map.toList fc
                                     , fv       <- flagAssignmentValue fc ]

    -- any is possible, no constraint
    flagAssignmentValue  FlagValueAny  = []
    flagAssignmentValue (FlagValue v)  = [v]
    -- none is possible, must simultaneously be True and False
    flagAssignmentValue  FlagValueNone = [False, True]
    
  
unionPkgCollection (PkgCollection other1 vcs1 vps1 fcs1)
                   (PkgCollection other2 vcs2 vps2 fcs2) =
    PkgCollection
      (unionAllowed other1 other2)
      (Map.unionWith unionVersionConstraint vcs1 vcs2)
      (Map.unionWith unionVersionConstraint vps1 vps2)
      (Map.unionWith (Map.unionWith unionFlagConstraint) fcs1 fcs2)

intersectPkgCollection (PkgCollection other1 vcs1 vps1 fcs1)
                       (PkgCollection other2 vcs2 vps2 fcs2) =
    PkgCollection
      (intersectAllowed other1 other2)
      (Map.unionWith intersectVersionConstraint vcs1 vcs2)
      (Map.unionWith intersectVersionConstraint vps1 vps2)
      (Map.unionWith (Map.unionWith intersectFlagConstraint) fcs1 fcs2)

invertPkgCollection (PkgCollection other vcs vps fcs) =
    PkgCollection
      (invertAllowed other)
      (Map.map invertVersionConstraint vcs)
      (Map.map invertVersionConstraint vps)
      (Map.map (Map.map invertFlagConstraint) fcs)

preferPkgCollection (PkgCollection other vcs vps fcs) =
    PkgCollection
      other
      Map.empty
      (Map.unionWith intersectVersionConstraint vcs vps)
      fcs

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
intersectVersionConstraint AllVersionsExcluded vc2 = AllVersionsExcluded
intersectVersionConstraint vc1 AllVersionsExcluded = AllVersionsExcluded
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

------------------------------------
-- Syntax for building collections
--

data PkgCollectionMember = PkgAllVersions    PackageName
                         | PkgSingleVersion  PackageName Version 
                         | PkgVersionRange   PackageName VersionRange
                         | PkgFlagAssignment PackageName FlagAssignment
  deriving (Eq, Show)

instance Text PkgCollectionMember where
  disp (PkgAllVersions    pkgname)          = disp pkgname
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
           ((do Parse.char '-'
                v <- parse
                return (PkgSingleVersion pkgname v))
        +++ (do vr <- parse
                return (PkgVersionRange pkgname vr))
        +++ (do fs <- parseFlagAssignment
                return (PkgFlagAssignment pkgname fs)))
        <++ (return (PkgAllVersions pkgname))

      parseFlagAssignment = Parse.many1 (spaces >> parseFlagValue)
      parseFlagValue =
            (do Parse.char '+'
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


makePkgCollection :: [PkgCollectionMember] -> PkgCollection
makePkgCollection members =
    PkgCollection I
      (Map.fromListWith unionVersionConstraint
                        (catMaybes (map toVerCs  members)))
      Map.empty
      (Map.fromListWith (Map.unionWith unionFlagConstraint)
                        (catMaybes (map toFlagCs members)))
  where
    toVerCs (PkgAllVersions pkgname)     = Just (pkgname, NoVersionConstraint)
    toVerCs (PkgSingleVersion pkgname v) = Just (pkgname, VersionConstraint (thisVersion v))
    toVerCs (PkgVersionRange pkgname vr) = Just (pkgname, VersionConstraint vr)
    toVerCs (PkgFlagAssignment _ _)      = Nothing

    toFlagCs (PkgFlagAssignment pkgname flags) = Just (pkgname, flagCs)
      where
        flagCs = Map.fromListWith intersectFlagConstraint
                                  [ (fn, FlagValue fv) | (fn, fv) <- flags ]
    toFlagCs _                                 = Nothing


mkPkgColl :: [String] -> PkgCollection
mkPkgColl strs = makePkgCollection [ p | str <- strs
                                   , let Just p = simpleParse str ]

empty = mkPkgColl []

gpl = mkPkgColl ["cpphs"]
-- cpphs -any
-- cpphs none

broken = mkPkgColl ["cpphs +foo"]
broken' = mkPkgColl ["cpphs -foo"]

platform = mkPkgColl ["bytestring-0.10.6.0", "cpphs-1.0"]

-----------------------------------------------------------------------------


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

     in coll == coll'

membersSem :: [PkgCollectionMember] -> PkgCollectionSem
membersSem members =
  \pkgid -> ( foldl' unionAllowed I
                [ pkgCollectionMemberSem member pkgid
                | member <- members ]
            , I
            , \flagname ->
              foldl1 unionFlagConstraint
                [ pkgCollectionMemberFlagSem member pkgid flagname
                | member <- members ] )
  where

    pkgCollectionMemberSem :: PkgCollectionMember -> PackageId -> Allowed
    pkgCollectionMemberSem (PkgAllVersions   pkgname)    pkgid
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

    pkgCollectionMemberFlagSem :: PkgCollectionMember -> PackageId -> FlagName -> FlagConstraint
    pkgCollectionMemberFlagSem (PkgFlagAssignment pkgname flags) pkgid flagname
      | packageName pkgid == pkgname
      = case lookup flagname flags of
          Nothing -> FlagValueAny
          Just v  -> FlagValue v
      | otherwise
      = FlagValueAny

