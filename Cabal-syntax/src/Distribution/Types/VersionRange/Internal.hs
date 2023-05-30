{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The only purpose of this module is to prevent the export of
-- 'VersionRange' constructors from
-- "Distribution.Types.VersionRange". To avoid creating orphan
-- instances, a lot of related code had to be moved here too.
module Distribution.Types.VersionRange.Internal
  ( VersionRange (..)
  , anyVersion
  , noVersion
  , thisVersion
  , notThisVersion
  , laterVersion
  , earlierVersion
  , orLaterVersion
  , orEarlierVersion
  , unionVersionRanges
  , intersectVersionRanges
  , withinVersion
  , majorBoundVersion
  , VersionRangeF (..)
  , projectVersionRange
  , embedVersionRange
  , cataVersionRange
  , anaVersionRange
  , hyloVersionRange
  , versionRangeParser
  , majorUpperBound
  , wildcardUpperBound
  ) where

import Distribution.Compat.Prelude
import Distribution.Types.Version
import Prelude ()

import Distribution.CabalSpecVersion
import Distribution.Parsec
import Distribution.Pretty
import Distribution.Utils.Generic (unsnoc)

import qualified Distribution.Compat.CharParsing as P
import qualified Distribution.Compat.DList as DList
import qualified Text.PrettyPrint as Disp

data VersionRange
  = ThisVersion Version -- = version
  | LaterVersion Version -- > version  (NB. not >=)
  | OrLaterVersion Version -- >= version
  | EarlierVersion Version -- < version
  | OrEarlierVersion Version -- <= version
  | MajorBoundVersion Version -- @^>= ver@ (same as >= ver && < MAJ(ver)+1)
  | UnionVersionRanges VersionRange VersionRange
  | IntersectVersionRanges VersionRange VersionRange
  deriving (Data, Eq, Ord, Generic, Read, Show, Typeable)

instance Binary VersionRange
instance Structured VersionRange
instance NFData VersionRange where rnf = genericRnf

-- | The version range @-any@. That is, a version range containing all
-- versions.
--
-- > withinRange v anyVersion = True
anyVersion :: VersionRange
anyVersion = OrLaterVersion (mkVersion [0])

-- | The empty version range @-none@, that is a version range containing no versions.
--
-- This can be constructed using any unsatisfiable version range expression,
-- for example @< 0@.
--
-- > withinRange v noVersion = False
noVersion :: VersionRange
noVersion = EarlierVersion (mkVersion [0])

-- | The version range @== v@.
--
-- > withinRange v' (thisVersion v) = v' == v
thisVersion :: Version -> VersionRange
thisVersion = ThisVersion

-- | The version range @/= v@.
--
-- > withinRange v' (notThisVersion v) = v' /= v
notThisVersion :: Version -> VersionRange
notThisVersion v = UnionVersionRanges (EarlierVersion v) (LaterVersion v)

-- | The version range @> v@.
--
-- > withinRange v' (laterVersion v) = v' > v
laterVersion :: Version -> VersionRange
laterVersion = LaterVersion

-- | The version range @>= v@.
--
-- > withinRange v' (orLaterVersion v) = v' >= v
orLaterVersion :: Version -> VersionRange
orLaterVersion = OrLaterVersion

-- | The version range @< v@.
--
-- > withinRange v' (earlierVersion v) = v' < v
earlierVersion :: Version -> VersionRange
earlierVersion = EarlierVersion

-- | The version range @<= v@.
--
-- > withinRange v' (orEarlierVersion v) = v' <= v
orEarlierVersion :: Version -> VersionRange
orEarlierVersion = OrEarlierVersion

-- | The version range @vr1 || vr2@.
--
-- >   withinRange v' (unionVersionRanges vr1 vr2)
-- > = withinRange v' vr1 || withinRange v' vr2
unionVersionRanges :: VersionRange -> VersionRange -> VersionRange
unionVersionRanges = UnionVersionRanges

-- | The version range @vr1 && vr2@.
--
-- >   withinRange v' (intersectVersionRanges vr1 vr2)
-- > = withinRange v' vr1 && withinRange v' vr2
intersectVersionRanges :: VersionRange -> VersionRange -> VersionRange
intersectVersionRanges = IntersectVersionRanges

-- | The version range @== v.*@.
--
-- For example, for version @1.2@, the version range @== 1.2.*@ is the same as
-- @>= 1.2 && < 1.3@.
--
-- > withinRange v' (withinVersion v) = v' >= v && v' < upper v
-- >   where
-- >     upper (Version lower t) = Version (init lower ++ [last lower + 1]) t
withinVersion :: Version -> VersionRange
withinVersion v =
  intersectVersionRanges
    (orLaterVersion v)
    (earlierVersion (wildcardUpperBound v))

-- | The version range @^>= v@.
--
-- For example, for version @1.2.3.4@, the version range @^>= 1.2.3.4@
-- is the same as @>= 1.2.3.4 && < 1.3@.
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
  = -- | @== version@.
    ThisVersionF Version
  | -- | @>  version@.   NB: not @>=@
    LaterVersionF Version
  | -- | @>= version@.
    OrLaterVersionF Version
  | -- | @<  version@.
    EarlierVersionF Version
  | -- | @<= version@.
    OrEarlierVersionF Version
  | -- | @^>= version@, same as @>= version && < MAJ(version)+1@.
    MajorBoundVersionF Version
  | -- | @||@.
    UnionVersionRangesF a a
  | -- | @&&@.
    IntersectVersionRangesF a a
  deriving
    ( Data
    , Eq
    , Generic
    , Read
    , Show
    , Typeable
    , Functor
    , Foldable
    , Traversable
    )

-- | Generic destructor for 'VersionRange'.
--
-- @since 2.2
projectVersionRange :: VersionRange -> VersionRangeF VersionRange
projectVersionRange (ThisVersion v) = ThisVersionF v
projectVersionRange (LaterVersion v) = LaterVersionF v
projectVersionRange (OrLaterVersion v) = OrLaterVersionF v
projectVersionRange (EarlierVersion v) = EarlierVersionF v
projectVersionRange (OrEarlierVersion v) = OrEarlierVersionF v
projectVersionRange (MajorBoundVersion v) = MajorBoundVersionF v
projectVersionRange (UnionVersionRanges a b) = UnionVersionRangesF a b
projectVersionRange (IntersectVersionRanges a b) = IntersectVersionRangesF a b

-- | Fold 'VersionRange'.
--
-- @since 2.2
cataVersionRange :: (VersionRangeF a -> a) -> VersionRange -> a
cataVersionRange f = c where c = f . fmap c . projectVersionRange

-- | Generic constructor for 'VersionRange'.
--
-- @since 2.2
embedVersionRange :: VersionRangeF VersionRange -> VersionRange
embedVersionRange (ThisVersionF v) = ThisVersion v
embedVersionRange (LaterVersionF v) = LaterVersion v
embedVersionRange (OrLaterVersionF v) = OrLaterVersion v
embedVersionRange (EarlierVersionF v) = EarlierVersion v
embedVersionRange (OrEarlierVersionF v) = OrEarlierVersion v
embedVersionRange (MajorBoundVersionF v) = MajorBoundVersion v
embedVersionRange (UnionVersionRangesF a b) = UnionVersionRanges a b
embedVersionRange (IntersectVersionRangesF a b) = IntersectVersionRanges a b

-- | Unfold 'VersionRange'.
--
-- @since 2.2
anaVersionRange :: (a -> VersionRangeF a) -> a -> VersionRange
anaVersionRange g = a where a = embedVersionRange . fmap a . g

-- | Refold 'VersionRange'.
--
-- @since 2.2
hyloVersionRange
  :: (VersionRangeF VersionRange -> VersionRange)
  -> (VersionRange -> VersionRangeF VersionRange)
  -> VersionRange
  -> VersionRange
hyloVersionRange f g = h where h = f . fmap h . g

-------------------------------------------------------------------------------
-- Parsec & Pretty
-------------------------------------------------------------------------------

-- |
--
-- >>> fmap pretty (simpleParsec' CabalSpecV1_6 "== 3.2.*" :: Maybe VersionRange)
-- Just >=3.2 && <3.3
--
-- >>> fmap (prettyVersioned CabalSpecV1_6) (simpleParsec' CabalSpecV1_6 "== 3.2.*" :: Maybe VersionRange)
-- Just ==3.2.*
--
-- >>> fmap pretty (simpleParsec' CabalSpecV1_6 "-any" :: Maybe VersionRange)
-- Just >=0
--
-- >>> fmap (prettyVersioned CabalSpecV1_6) (simpleParsec' CabalSpecV1_6 "-any" :: Maybe VersionRange)
-- Just >=0
instance Pretty VersionRange where
  pretty = prettyVersioned cabalSpecLatest

  prettyVersioned csv
    | csv > CabalSpecV1_6 = prettyVersionRange
    | otherwise = prettyVersionRange16

prettyVersionRange :: VersionRange -> Disp.Doc
prettyVersionRange vr = cataVersionRange alg vr 0
  where
    alg :: VersionRangeF (Int -> Disp.Doc) -> Int -> Disp.Doc
    alg (ThisVersionF v) _ = Disp.text "==" <<>> pretty v
    alg (LaterVersionF v) _ = Disp.text ">" <<>> pretty v
    alg (OrLaterVersionF v) _ = Disp.text ">=" <<>> pretty v
    alg (EarlierVersionF v) _ = Disp.text "<" <<>> pretty v
    alg (OrEarlierVersionF v) _ = Disp.text "<=" <<>> pretty v
    alg (MajorBoundVersionF v) _ = Disp.text "^>=" <<>> pretty v
    alg (UnionVersionRangesF r1 r2) d =
      parens (d > 0) $
        r1 1 <+> Disp.text "||" <+> r2 0
    alg (IntersectVersionRangesF r1 r2) d =
      parens (d > 1) $
        r1 2 <+> Disp.text "&&" <+> r2 1

    parens True = Disp.parens
    parens False = id

-- | Don't use && and || operators. If possible.
prettyVersionRange16 :: VersionRange -> Disp.Doc
prettyVersionRange16 (IntersectVersionRanges (OrLaterVersion v) (EarlierVersion u))
  | u == wildcardUpperBound v =
      Disp.text "==" <<>> dispWild v
  where
    dispWild ver =
      Disp.hcat
        ( Disp.punctuate
            (Disp.char '.')
            (map Disp.int $ versionNumbers ver)
        )
        <<>> Disp.text ".*"
prettyVersionRange16 vr = prettyVersionRange vr

-- |
--
-- >>> simpleParsec "^>= 3.4" :: Maybe VersionRange
-- Just (MajorBoundVersion (mkVersion [3,4]))
--
-- Small history:
--
-- @-any@ and @-none@ removed in 3.4
-- Use @>=0@ and @<0@ instead.
--
-- >>> map (`simpleParsec'` "-none") [CabalSpecV3_0, CabalSpecV3_4] :: [Maybe VersionRange]
-- [Just (EarlierVersion (mkVersion [0])),Nothing]
--
-- Set operations are introduced in 3.0
--
-- >>> map (`simpleParsec'` "^>= { 1.2 , 1.3 }") [CabalSpecV2_4, CabalSpecV3_0] :: [Maybe VersionRange]
-- [Nothing,Just (UnionVersionRanges (MajorBoundVersion (mkVersion [1,2])) (MajorBoundVersion (mkVersion [1,3])))]
--
-- @^>=@ is introduced in 2.0
--
-- >>> map (`simpleParsec'` "^>=1.2") [CabalSpecV1_24, CabalSpecV2_0] :: [Maybe VersionRange]
-- [Nothing,Just (MajorBoundVersion (mkVersion [1,2]))]
--
-- @-none@ is introduced in 1.22
--
-- >>> map (`simpleParsec'` "-none") [CabalSpecV1_20, CabalSpecV1_22] :: [Maybe VersionRange]
-- [Nothing,Just (EarlierVersion (mkVersion [0]))]
--
-- Operators are introduced in 1.8. Issues only a warning.
--
-- >>> map (`simpleParsecW'` "== 1 || ==2") [CabalSpecV1_6, CabalSpecV1_8] :: [Maybe VersionRange]
-- [Nothing,Just (UnionVersionRanges (ThisVersion (mkVersion [1])) (ThisVersion (mkVersion [2])))]
--
-- Wild-version ranges are introduced in 1.6. Issues only a warning.
--
-- >>> map (`simpleParsecW'` "== 1.2.*") [CabalSpecV1_4, CabalSpecV1_6] :: [Maybe VersionRange]
-- [Nothing,Just (IntersectVersionRanges (OrLaterVersion (mkVersion [1,2])) (EarlierVersion (mkVersion [1,3])))]
instance Parsec VersionRange where
  parsec = askCabalSpecVersion >>= versionRangeParser versionDigitParser

-- | 'VersionRange' parser parametrised by version digit parser.
--
-- - 'versionDigitParser' is used for all 'VersionRange'.
-- - 'P.integral' is used for backward-compat @pkgconfig-depends@
--   versions, 'PkgConfigVersionRange'.
--
-- @since 3.0
versionRangeParser :: forall m. CabalParsing m => m Int -> CabalSpecVersion -> m VersionRange
versionRangeParser digitParser csv = expr
  where
    expr = do
      P.spaces
      t <- term
      P.spaces
      ( do
          _ <- P.string "||"
          checkOp
          P.spaces
          e <- expr
          return (unionVersionRanges t e)
          <|> return t
        )
    term = do
      f <- factor
      P.spaces
      ( do
          _ <- P.string "&&"
          checkOp
          P.spaces
          t <- term
          return (intersectVersionRanges f t)
          <|> return f
        )
    factor = parens expr <|> prim

    prim = do
      op <- P.munch1 isOpChar P.<?> "operator"
      case op of
        "-" -> anyVersion <$ P.string "any" <|> P.string "none" *> noVersion'
        "==" -> do
          P.spaces
          ( do
              (wild, v) <- verOrWild
              checkWild wild
              pure $ (if wild then withinVersion else thisVersion) v
              <|> (verSet' thisVersion =<< verSet)
            )
        "^>=" -> do
          P.spaces
          ( do
              (wild, v) <- verOrWild
              when wild $
                P.unexpected $
                  "wild-card version after ^>= operator"
              majorBoundVersion' v
              <|> (verSet' majorBoundVersion =<< verSet)
            )
        _ -> do
          P.spaces
          (wild, v) <- verOrWild
          when wild $
            P.unexpected $
              "wild-card version after non-== operator: " ++ show op
          case op of
            ">=" -> pure $ orLaterVersion v
            "<" -> pure $ earlierVersion v
            "<=" -> pure $ orEarlierVersion v
            ">" -> pure $ laterVersion v
            _ -> fail $ "Unknown version operator " ++ show op

    -- Cannot be warning
    -- On 2020-03-16 there was around 27400 files on Hackage failing to parse due this
    -- For example https://hackage.haskell.org/package/haxr-3000.0.0/haxr.cabal
    --
    checkOp =
      when (csv < CabalSpecV1_8) $
        parsecWarning PWTVersionOperator $
          unwords
            [ "version operators used."
            , "To use version operators the package needs to specify at least 'cabal-version: >= 1.8'."
            ]

    -- Cannot be warning
    -- On 2020-03-16 there was 46 files on Hackage failing to parse due this
    -- For example https://hackage.haskell.org/package/derive-0.1.2/derive.cabal
    --
    checkWild False = pure ()
    checkWild True =
      when (csv < CabalSpecV1_6) $
        parsecWarning PWTVersionWildcard $
          unwords
            [ "Wildcard syntax used."
            , "To use version wildcards the package needs to specify at least 'cabal-version: >= 1.6'."
            ]

    -- https://gitlab.haskell.org/ghc/ghc/issues/17752
    isOpChar '<' = True
    isOpChar '=' = True
    isOpChar '>' = True
    isOpChar '^' = True
    isOpChar '-' = csv < CabalSpecV3_4
    -- https://github.com/haskell/cabal/issues/6589
    -- Unfortunately we have must not consume the dash,
    -- as otherwise following parts may not be parsed.
    --
    -- i.e. we cannot fail here with good error.
    isOpChar _ = False

    -- -none version range is available since 1.22
    noVersion' =
      if csv >= CabalSpecV1_22
        then pure noVersion
        else
          fail $
            unwords
              [ "-none version range used."
              , "To use this syntax the package needs to specify at least 'cabal-version: 1.22'."
              , "Alternatively, if broader compatibility is important then use"
              , "<0 or other empty range."
              ]

    -- \^>= is available since 2.0
    majorBoundVersion' v =
      if csv >= CabalSpecV2_0
        then pure $ majorBoundVersion v
        else
          fail $
            unwords
              [ "major bounded version syntax (caret, ^>=) used."
              , "To use this syntax the package need to specify at least 'cabal-version: 2.0'."
              , "Alternatively, if broader compatibility is important then use:"
              , prettyShow $ eliminateMajorBoundSyntax $ majorBoundVersion v
              ]
      where
        eliminateMajorBoundSyntax = hyloVersionRange embed projectVersionRange
        embed (MajorBoundVersionF u) =
          intersectVersionRanges
            (orLaterVersion u)
            (earlierVersion (majorUpperBound u))
        embed vr = embedVersionRange vr

    -- version set notation (e.g. "== { 0.0.1.0, 0.0.2.0, 0.1.0.0 }")
    verSet' op vs =
      if csv >= CabalSpecV3_0
        then pure $ foldr1 unionVersionRanges (fmap op vs)
        else
          fail $
            unwords
              [ "version set syntax used."
              , "To use this syntax the package needs to specify at least 'cabal-version: 3.0'."
              , "Alternatively, if broader compatibility is important then use"
              , "a series of single version constraints joined with the || operator:"
              , prettyShow (foldr1 unionVersionRanges (fmap op vs))
              ]

    verSet :: CabalParsing m => m (NonEmpty Version)
    verSet = do
      _ <- P.char '{'
      P.spaces
      vs <- P.sepByNonEmpty (verPlain <* P.spaces) (P.char ',' *> P.spaces)
      _ <- P.char '}'
      pure vs

    -- a plain version without tags or wildcards
    verPlain :: CabalParsing m => m Version
    verPlain = mkVersion <$> toList <$> P.sepByNonEmpty digitParser (P.char '.')

    -- either wildcard or normal version
    verOrWild :: CabalParsing m => m (Bool, Version)
    verOrWild = do
      x <- digitParser
      verLoop (DList.singleton x)

    -- trailing: wildcard (.y.*) or normal version (optional tags) (.y.z-tag)
    verLoop :: CabalParsing m => DList.DList Int -> m (Bool, Version)
    verLoop acc =
      verLoop' acc
        <|> (tags *> pure (False, mkVersion (DList.toList acc)))

    verLoop' :: CabalParsing m => DList.DList Int -> m (Bool, Version)
    verLoop' acc = do
      _ <- P.char '.'
      let digit = digitParser >>= verLoop . DList.snoc acc
      let wild = (True, mkVersion (DList.toList acc)) <$ P.char '*'
      digit <|> wild

    parens p = P.between
      ((P.char '(' P.<?> "opening paren") >> P.spaces)
      (P.char ')' >> P.spaces)
      $ do
        a <- p
        P.spaces
        return a

    tags :: CabalParsing m => m ()
    tags = do
      ts <- many $ P.char '-' *> some (P.satisfy isAlphaNum)
      case ts of
        [] -> pure ()
        (_ : _) -> parsecWarning PWTVersionTag "version with tags"

----------------------------
-- Wildcard range utilities
--

-- | Compute next greater major version to be used as upper bound.
--
-- Example: @0.4.1@ produces the version @0.5@ which then can be used
-- to construct a range @>= 0.4.1 && < 0.5@
--
-- @since 2.2
majorUpperBound :: Version -> Version
majorUpperBound = alterVersion $ \numbers -> case numbers of
  [] -> [0, 1] -- should not happen
  [m1] -> [m1, 1] -- e.g. version '1'
  (m1 : m2 : _) -> [m1, m2 + 1]

-- | Increment the last version number.
--
-- Example: For @1.2@ this returns @1.3@
-- so that it can be used as upper bound when resolving @== 1.2.*@.
-- For @0.4.1@ it returns @0.4.2@.
--
-- @since 2.2
wildcardUpperBound :: Version -> Version
wildcardUpperBound = alterVersion $
  \lowerBound -> case unsnoc lowerBound of
    Nothing -> []
    Just (xs, x) -> xs ++ [x + 1]
