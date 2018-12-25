{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE FlexibleContexts   #-}
module Distribution.Types.VersionRange (
    -- * Version ranges
    VersionRange(..),

    -- ** Constructing
    anyVersion, noVersion,
    thisVersion, notThisVersion,
    laterVersion, earlierVersion,
    orLaterVersion, orEarlierVersion,
    unionVersionRanges, intersectVersionRanges,
    withinVersion,
    majorBoundVersion,

    -- ** Inspection
    --
    -- See "Distribution.Version" for more utilities.
    withinRange,
    foldVersionRange,
    normaliseVersionRange,
    stripParensVersionRange,
    hasUpperBound,
    hasLowerBound,

    -- ** Cata & ana
    VersionRangeF (..),
    cataVersionRange,
    anaVersionRange,
    hyloVersionRange,
    projectVersionRange,
    embedVersionRange,

    -- ** Utilities
    wildcardUpperBound,
    majorUpperBound,
    isWildcardRange,
    ) where

import Distribution.Compat.Prelude
import Distribution.Types.Version
import Prelude ()

import Distribution.CabalSpecVersion
import Distribution.Parsec
import Distribution.Pretty
import Text.PrettyPrint          ((<+>))

import qualified Distribution.Compat.CharParsing as P
import qualified Distribution.Compat.DList       as DList
import qualified Text.PrettyPrint                as Disp

data VersionRange
  = AnyVersion
  | ThisVersion            Version -- = version
  | LaterVersion           Version -- > version  (NB. not >=)
  | OrLaterVersion         Version -- >= version
  | EarlierVersion         Version -- < version
  | OrEarlierVersion       Version -- <= version
  | WildcardVersion        Version -- == ver.*   (same as >= ver && < ver+1)
  | MajorBoundVersion      Version -- @^>= ver@ (same as >= ver && < MAJ(ver)+1)
  | UnionVersionRanges     VersionRange VersionRange
  | IntersectVersionRanges VersionRange VersionRange
  | VersionRangeParens     VersionRange -- just '(exp)' parentheses syntax
  deriving (Data, Eq, Generic, Read, Show, Typeable)

instance Binary VersionRange

instance NFData VersionRange where rnf = genericRnf

{-# DeprecateD AnyVersion
    "Use 'anyVersion', 'foldVersionRange' or 'asVersionIntervals'" #-}
{-# DEPRECATED ThisVersion
    "Use 'thisVersion', 'foldVersionRange' or 'asVersionIntervals'" #-}
{-# DEPRECATED LaterVersion
    "Use 'laterVersion', 'foldVersionRange' or 'asVersionIntervals'" #-}
{-# DEPRECATED EarlierVersion
    "Use 'earlierVersion', 'foldVersionRange' or 'asVersionIntervals'" #-}
{-# DEPRECATED WildcardVersion
    "Use 'anyVersion', 'foldVersionRange' or 'asVersionIntervals'" #-}
{-# DEPRECATED UnionVersionRanges
    "Use 'unionVersionRanges', 'foldVersionRange' or 'asVersionIntervals'" #-}
{-# DEPRECATED IntersectVersionRanges
    "Use 'intersectVersionRanges', 'foldVersionRange' or 'asVersionIntervals'"#-}

-- | The version range @-any@. That is, a version range containing all
-- versions.
--
-- > withinRange v anyVersion = True
--
anyVersion :: VersionRange
anyVersion = AnyVersion

-- | The empty version range, that is a version range containing no versions.
--
-- This can be constructed using any unsatisfiable version range expression,
-- for example @> 1 && < 1@.
--
-- > withinRange v noVersion = False
--
noVersion :: VersionRange
noVersion = IntersectVersionRanges (LaterVersion v) (EarlierVersion v)
  where v = mkVersion [1]

-- | The version range @== v@
--
-- > withinRange v' (thisVersion v) = v' == v
--
thisVersion :: Version -> VersionRange
thisVersion = ThisVersion

-- | The version range @< v || > v@
--
-- > withinRange v' (notThisVersion v) = v' /= v
--
notThisVersion :: Version -> VersionRange
notThisVersion v = UnionVersionRanges (EarlierVersion v) (LaterVersion v)

-- | The version range @> v@
--
-- > withinRange v' (laterVersion v) = v' > v
--
laterVersion :: Version -> VersionRange
laterVersion = LaterVersion

-- | The version range @>= v@
--
-- > withinRange v' (orLaterVersion v) = v' >= v
--
orLaterVersion :: Version -> VersionRange
orLaterVersion = OrLaterVersion

-- | The version range @< v@
--
-- > withinRange v' (earlierVersion v) = v' < v
--
earlierVersion :: Version -> VersionRange
earlierVersion = EarlierVersion

-- | The version range @<= v@
--
-- > withinRange v' (orEarlierVersion v) = v' <= v
--
orEarlierVersion :: Version -> VersionRange
orEarlierVersion = OrEarlierVersion

-- | The version range @vr1 || vr2@
--
-- >   withinRange v' (unionVersionRanges vr1 vr2)
-- > = withinRange v' vr1 || withinRange v' vr2
--
unionVersionRanges :: VersionRange -> VersionRange -> VersionRange
unionVersionRanges = UnionVersionRanges

-- | The version range @vr1 && vr2@
--
-- >   withinRange v' (intersectVersionRanges vr1 vr2)
-- > = withinRange v' vr1 && withinRange v' vr2
--
intersectVersionRanges :: VersionRange -> VersionRange -> VersionRange
intersectVersionRanges = IntersectVersionRanges

-- | The version range @== v.*@.
--
-- For example, for version @1.2@, the version range @== 1.2.*@ is the same as
-- @>= 1.2 && < 1.3@
--
-- > withinRange v' (laterVersion v) = v' >= v && v' < upper v
-- >   where
-- >     upper (Version lower t) = Version (init lower ++ [last lower + 1]) t
--
withinVersion :: Version -> VersionRange
withinVersion = WildcardVersion

-- | The version range @^>= v@.
--
-- For example, for version @1.2.3.4@, the version range @^>= 1.2.3.4@ is the same as
-- @>= 1.2.3.4 && < 1.3@.
--
-- Note that @^>= 1@ is equivalent to @>= 1 && < 1.1@.
--
-- @since 2.0.0.2
majorBoundVersion :: Version -> VersionRange
majorBoundVersion = MajorBoundVersion

-- | F-Algebra of 'VersionRange'. See 'cataVersionRange'.
--
-- @since 2.2
data VersionRangeF a
  = AnyVersionF
  | ThisVersionF            Version -- = version
  | LaterVersionF           Version -- > version  (NB. not >=)
  | OrLaterVersionF         Version -- >= version
  | EarlierVersionF         Version -- < version
  | OrEarlierVersionF       Version -- <= version
  | WildcardVersionF        Version -- == ver.*   (same as >= ver && < ver+1)
  | MajorBoundVersionF      Version -- @^>= ver@ (same as >= ver && < MAJ(ver)+1)
  | UnionVersionRangesF     a a
  | IntersectVersionRangesF a a
  | VersionRangeParensF     a
  deriving (Data, Eq, Generic, Read, Show, Typeable, Functor, Foldable, Traversable)

-- | @since 2.2
projectVersionRange :: VersionRange -> VersionRangeF VersionRange
projectVersionRange AnyVersion                   = AnyVersionF
projectVersionRange (ThisVersion v)              = ThisVersionF v
projectVersionRange (LaterVersion v)             = LaterVersionF v
projectVersionRange (OrLaterVersion v)           = OrLaterVersionF v
projectVersionRange (EarlierVersion v)           = EarlierVersionF v
projectVersionRange (OrEarlierVersion v)         = OrEarlierVersionF v
projectVersionRange (WildcardVersion v)          = WildcardVersionF v
projectVersionRange (MajorBoundVersion v)        = MajorBoundVersionF v
projectVersionRange (UnionVersionRanges a b)     = UnionVersionRangesF a b
projectVersionRange (IntersectVersionRanges a b) = IntersectVersionRangesF a b
projectVersionRange (VersionRangeParens a)       = VersionRangeParensF a

-- | Fold 'VersionRange'.
--
-- @since 2.2
cataVersionRange :: (VersionRangeF a -> a) -> VersionRange -> a
cataVersionRange f = c where c = f . fmap c . projectVersionRange

-- | @since 2.2
embedVersionRange :: VersionRangeF VersionRange -> VersionRange
embedVersionRange AnyVersionF                   = AnyVersion
embedVersionRange (ThisVersionF v)              = ThisVersion v
embedVersionRange (LaterVersionF v)             = LaterVersion v
embedVersionRange (OrLaterVersionF v)           = OrLaterVersion v
embedVersionRange (EarlierVersionF v)           = EarlierVersion v
embedVersionRange (OrEarlierVersionF v)         = OrEarlierVersion v
embedVersionRange (WildcardVersionF v)          = WildcardVersion v
embedVersionRange (MajorBoundVersionF v)        = MajorBoundVersion v
embedVersionRange (UnionVersionRangesF a b)     = UnionVersionRanges a b
embedVersionRange (IntersectVersionRangesF a b) = IntersectVersionRanges a b
embedVersionRange (VersionRangeParensF a)       = VersionRangeParens a

-- | Unfold 'VersionRange'.
--
-- @since 2.2
anaVersionRange :: (a -> VersionRangeF a) -> a -> VersionRange
anaVersionRange g = a where a = embedVersionRange . fmap a . g


-- | Fold over the basic syntactic structure of a 'VersionRange'.
--
-- This provides a syntactic view of the expression defining the version range.
-- The syntactic sugar @\">= v\"@, @\"<= v\"@ and @\"== v.*\"@ is presented
-- in terms of the other basic syntax.
--
-- For a semantic view use 'asVersionIntervals'.
--
foldVersionRange :: a                         -- ^ @\"-any\"@ version
                 -> (Version -> a)            -- ^ @\"== v\"@
                 -> (Version -> a)            -- ^ @\"> v\"@
                 -> (Version -> a)            -- ^ @\"< v\"@
                 -> (a -> a -> a)             -- ^ @\"_ || _\"@ union
                 -> (a -> a -> a)             -- ^ @\"_ && _\"@ intersection
                 -> VersionRange -> a
foldVersionRange anyv this later earlier union intersect = fold
  where
    fold = cataVersionRange alg

    alg AnyVersionF                     = anyv
    alg (ThisVersionF v)                = this v
    alg (LaterVersionF v)               = later v
    alg (OrLaterVersionF v)             = union (this v) (later v)
    alg (EarlierVersionF v)             = earlier v
    alg (OrEarlierVersionF v)           = union (this v) (earlier v)
    alg (WildcardVersionF v)            = fold (wildcard v)
    alg (MajorBoundVersionF v)          = fold (majorBound v)
    alg (UnionVersionRangesF v1 v2)     = union v1 v2
    alg (IntersectVersionRangesF v1 v2) = intersect v1 v2
    alg (VersionRangeParensF v)         = v

    wildcard v = intersectVersionRanges
                   (orLaterVersion v)
                   (earlierVersion (wildcardUpperBound v))

    majorBound v = intersectVersionRanges
                     (orLaterVersion v)
                     (earlierVersion (majorUpperBound v))

-- | Refold 'VersionRange'
--
-- @since 2.2
hyloVersionRange :: (VersionRangeF VersionRange -> VersionRange)
                 -> (VersionRange -> VersionRangeF VersionRange)
                 -> VersionRange -> VersionRange
hyloVersionRange f g = h where h = f . fmap h . g

-- | Normalise 'VersionRange'.
--
-- In particular collapse @(== v || > v)@ into @>= v@, and so on.
normaliseVersionRange :: VersionRange -> VersionRange
normaliseVersionRange = hyloVersionRange embed projectVersionRange
  where
    -- == v || > v, > v || == v  ==>  >= v
    embed (UnionVersionRangesF (ThisVersion v) (LaterVersion v')) | v == v' =
        orLaterVersion v
    embed (UnionVersionRangesF (LaterVersion v) (ThisVersion v')) | v == v' =
        orLaterVersion v

    -- == v || < v, < v || == v  ==>  <= v
    embed (UnionVersionRangesF (ThisVersion v) (EarlierVersion v')) | v == v' =
        orEarlierVersion v
    embed (UnionVersionRangesF (EarlierVersion v) (ThisVersion v')) | v == v' =
        orEarlierVersion v

    -- otherwise embed normally
    embed vr = embedVersionRange vr

-- |  Remove 'VersionRangeParens' constructors.
--
-- @since 2.2
stripParensVersionRange :: VersionRange -> VersionRange
stripParensVersionRange = hyloVersionRange embed projectVersionRange
  where
    embed (VersionRangeParensF vr) = vr
    embed vr = embedVersionRange vr

-- | Does this version fall within the given range?
--
-- This is the evaluation function for the 'VersionRange' type.
--
withinRange :: Version -> VersionRange -> Bool
withinRange v = foldVersionRange
                   True
                   (\v'  -> v == v')
                   (\v'  -> v >  v')
                   (\v'  -> v <  v')
                   (||)
                   (&&)

----------------------------
-- Wildcard range utilities
--

-- | @since 2.2
wildcardUpperBound :: Version -> Version
wildcardUpperBound = alterVersion $
    \lowerBound -> init lowerBound ++ [last lowerBound + 1]

isWildcardRange :: Version -> Version -> Bool
isWildcardRange ver1 ver2 = check (versionNumbers ver1) (versionNumbers ver2)
  where check (n:[]) (m:[]) | n+1 == m = True
        check (n:ns) (m:ms) | n   == m = check ns ms
        check _      _                 = False

-- | Compute next greater major version to be used as upper bound
--
-- Example: @0.4.1@ produces the version @0.5@ which then can be used
-- to construct a range @>= 0.4.1 && < 0.5@
--
-- @since 2.2
majorUpperBound :: Version -> Version
majorUpperBound = alterVersion $ \numbers -> case numbers of
    []        -> [0,1] -- should not happen
    [m1]      -> [m1,1] -- e.g. version '1'
    (m1:m2:_) -> [m1,m2+1]

-------------------------------------------------------------------------------
-- Parsec & Pretty
-------------------------------------------------------------------------------

instance Pretty VersionRange where
    pretty = fst . cataVersionRange alg
      where
        alg AnyVersionF                     = (Disp.text "-any", 0 :: Int)
        alg (ThisVersionF v)                = (Disp.text "==" <<>> pretty v, 0)
        alg (LaterVersionF v)               = (Disp.char '>'  <<>> pretty v, 0)
        alg (OrLaterVersionF v)             = (Disp.text ">=" <<>> pretty v, 0)
        alg (EarlierVersionF v)             = (Disp.char '<'  <<>> pretty v, 0)
        alg (OrEarlierVersionF v)           = (Disp.text "<=" <<>> pretty v, 0)
        alg (WildcardVersionF v)            = (Disp.text "==" <<>> dispWild v, 0)
        alg (MajorBoundVersionF v)          = (Disp.text "^>=" <<>> pretty v, 0)
        alg (UnionVersionRangesF (r1, p1) (r2, p2)) =
            (punct 1 p1 r1 <+> Disp.text "||" <+> punct 2 p2 r2 , 2)
        alg (IntersectVersionRangesF (r1, p1) (r2, p2)) =
            (punct 0 p1 r1 <+> Disp.text "&&" <+> punct 1 p2 r2 , 1)
        alg (VersionRangeParensF (r, _))         =
            (Disp.parens r, 0)

        dispWild ver =
            Disp.hcat (Disp.punctuate (Disp.char '.') (map Disp.int $ versionNumbers ver))
            <<>> Disp.text ".*"

        punct p p' | p < p'    = Disp.parens
                   | otherwise = id

instance Parsec VersionRange where
    parsec = expr
      where
        expr   = do P.spaces
                    t <- term
                    P.spaces
                    (do _  <- P.string "||"
                        P.spaces
                        e <- expr
                        return (unionVersionRanges t e)
                     <|>
                     return t)
        term   = do f <- factor
                    P.spaces
                    (do _  <- P.string "&&"
                        P.spaces
                        t <- term
                        return (intersectVersionRanges f t)
                     <|>
                     return f)
        factor = parens expr <|> prim

        prim = do
            op <- P.munch1 (`elem` "<>=^-") P.<?> "operator"
            case op of
                "-" -> anyVersion <$ P.string "any" <|> P.string "none" *> noVersion'

                "==" -> do
                    P.spaces
                    (wild, v) <- verOrWild
                    pure $ (if wild then withinVersion else thisVersion) v

                _ -> do
                    P.spaces
                    (wild, v) <- verOrWild
                    when wild $ P.unexpected $
                        "wild-card version after non-== operator: " ++ show op
                    case op of
                        ">="  -> pure $ orLaterVersion v
                        "<"   -> pure $ earlierVersion v
                        "^>=" -> majorBoundVersion' v
                        "<="  -> pure $ orEarlierVersion v
                        ">"   -> pure $ laterVersion v
                        _ -> fail $ "Unknown version operator " ++ show op

        -- Note: There are other features:
        -- && and || since 1.8
        -- x.y.* (wildcard) since 1.6

        -- -none version range is available since 1.22
        noVersion' = do
            csv <- askCabalSpecVersion
            if csv >= CabalSpecV1_22
            then pure noVersion
            else fail $ unwords
                [ "-none version range used."
                , "To use this syntax the package needs to specify at least 'cabal-version: 1.22'."
                , "Alternatively, if broader compatibility is important then use"
                , "<0 or other empty range."
                ]

        -- ^>= is available since 2.0
        majorBoundVersion' v = do
            csv <- askCabalSpecVersion
            if csv >= CabalSpecV2_0
            then pure $ majorBoundVersion v
            else fail $ unwords
                [ "major bounded version syntax (caret, ^>=) used."
                , "To use this syntax the package need to specify at least 'cabal-version: 2.0'."
                , "Alternatively, if broader compatibility is important then use:"
                , prettyShow $ eliminateMajorBoundSyntax $ majorBoundVersion v
                ]
          where
            eliminateMajorBoundSyntax = hyloVersionRange embed projectVersionRange
            embed (MajorBoundVersionF u) = intersectVersionRanges
                (orLaterVersion u) (earlierVersion (majorUpperBound u))
            embed vr = embedVersionRange vr

        -- either wildcard or normal version
        verOrWild :: CabalParsing m => m (Bool, Version)
        verOrWild = do
            x <- P.integral
            verLoop (DList.singleton x)

        -- trailing: wildcard (.y.*) or normal version (optional tags) (.y.z-tag)
        verLoop :: CabalParsing m => DList.DList Int -> m (Bool, Version)
        verLoop acc = verLoop' acc <|> (tags *> pure (False, mkVersion (DList.toList acc)))

        verLoop' :: CabalParsing m => DList.DList Int -> m (Bool, Version)
        verLoop' acc = do
            _ <- P.char '.'
            let digit = P.integral >>= verLoop . DList.snoc acc
            let wild  = (True, mkVersion (DList.toList acc)) <$ P.char '*'
            digit <|> wild

        parens p = P.between
            ((P.char '(' P.<?> "opening paren") >> P.spaces)
            (P.char ')' >> P.spaces)
            (do a <- p
                P.spaces
                return (VersionRangeParens a))

        tags :: CabalParsing m => m ()
        tags = do
            ts <- many $ P.char '-' *> some (P.satisfy isAlphaNum)
            case ts of
                []      -> pure ()
                (_ : _) -> parsecWarning PWTVersionTag "version with tags"

-- | Does the version range have an upper bound?
--
-- @since 1.24.0.0
hasUpperBound :: VersionRange -> Bool
hasUpperBound = foldVersionRange
                False
                (const True)
                (const False)
                (const True)
                (&&) (||)

-- | Does the version range have an explicit lower bound?
--
-- Note: this function only considers the user-specified lower bounds, but not
-- the implicit >=0 lower bound.
--
-- @since 1.24.0.0
hasLowerBound :: VersionRange -> Bool
hasLowerBound = foldVersionRange
                False
                (const True)
                (const True)
                (const False)
                (&&) (||)
