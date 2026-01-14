{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Distribution.Types.VersionRange.Parsec where

import Distribution.Compat.Prelude
import Distribution.Types.Version
import Prelude ()

import Distribution.CabalSpecVersion
import Distribution.Parsec
import Distribution.Pretty
import Distribution.Utils.Generic (unsnoc)

import Distribution.Types.Version.Parsec
import Distribution.Types.VersionRange
import Distribution.Types.VersionRange.Internal
import Distribution.Types.VersionRange.Pretty

import Distribution.Types.Annotation

import Data.Function

import qualified Distribution.Compat.CharParsing as P
import qualified Distribution.Compat.DList as DList
import qualified Text.PrettyPrint as Disp

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
  triviaParsec = askCabalSpecVersion >>= versionRangeTriviaParser versionDigitParser

versionRangeParser digitParser csv = fmap (\(_, x) -> x) $ versionRangeTriviaParser digitParser csv

-- | 'VersionRange' parser parametrised by version digit parser.
--
-- - 'versionDigitParser' is used for all 'VersionRange'.
-- - 'P.integral' is used for backward-compat @pkgconfig-depends@
--   versions, 'PkgConfigVersionRange'.
--
-- @since 3.0
versionRangeTriviaParser :: forall m. CabalParsing m => m Int -> CabalSpecVersion -> m (TriviaTree, VersionRange)
versionRangeTriviaParser digitParser csv = expr
  where
    expr :: m (TriviaTree, VersionRange)
    expr = do
      preSpaces <- P.spaces'
      (tTerm, t) <- term
      postSpaces <- P.spaces'
      let tTerm' = annotateTriviaTree (NSVersionRange t) [PreTrivia preSpaces, PostTrivia postSpaces] tTerm
      ( do
          _ <- P.string "||"
          checkOp
          preSpaces' <- P.spaces'
          (tExpr, e) <- expr
          let eUnion = unionVersionRanges t e
          let tExpr' =
                mark
                  (NSVersionRange eUnion)
                  ( annotateTriviaTree (NSVersionRange e) [PreTrivia preSpaces'] tExpr
                      <> tTerm'
                  )
          return (tExpr', eUnion)
        )
        <|> return (tTerm', t)

    term :: m (TriviaTree, VersionRange)
    term = do
      (tFact, f) <- factor
      postSpaces <- P.spaces'
      let tFact' = annotateTriviaTree (NSVersionRange f) [PostTrivia postSpaces] tFact

      ( do
          _ <- P.string "&&"
          checkOp
          preSpaces' <- P.spaces'
          (tTerm, t) <- term
          let tUnion = intersectVersionRanges f t
          let tTerm' =
                mark
                  (NSVersionRange tUnion)
                  ( annotateTriviaTree (NSVersionRange t) [PreTrivia preSpaces'] tTerm
                      <> tFact
                  )
          return (tTerm', tUnion)
        )
        <|> return (tFact', f)

    factor :: m (TriviaTree, VersionRange)
    factor = parens expr <|> prim

    prim :: m (TriviaTree, VersionRange)
    prim = do
      op <- P.munch1 isOpChar P.<?> "operator"
      case op of
        "-" -> (pure anyVersion) <$ P.string "any" <|> P.string "none" *> (pure <$> noVersion')
        "==" -> do
          preSpaces <- P.spaces'
          ( do
              (wild, v) <- verOrWild
              checkWild wild
              let tVer = annotateTriviaTree (NSVersion v) [PreTrivia preSpaces] mempty
              pure $ (tVer,) $ (if wild then withinVersion else thisVersion) v

              -- ignore braces for now
              <|> (verSet' thisVersion =<< verSet)
            )
        "^>=" -> do
          preSpaces <- P.spaces'
          ( do
              (wild, v) <- verOrWild
              when wild $
                P.unexpected "wild-card version after ^>= operator"

              let tVer = annotateTriviaTree (NSVersion v) [PreTrivia preSpaces] mempty
              (tVer,) <$> majorBoundVersion' v

              -- ignore braces for now
              <|> (verSet' majorBoundVersion =<< verSet)
            )
        _ -> do
          preSpaces <- P.spaces'
          (wild, v) <- verOrWild
          when wild $
            P.unexpected $
              "wild-card version after non-== operator: " ++ show op

          let tVer = annotateTriviaTree (NSVersion v) [PreTrivia preSpaces] mempty
          case op of
            ">=" -> pure $ (tVer, orLaterVersion v)
            "<" -> pure $ (tVer, earlierVersion v)
            "<=" -> pure $ (tVer, orEarlierVersion v)
            ">" -> pure $ (tVer, laterVersion v)
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
    noVersion' :: m VersionRange
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
        then (pure . pure) $ foldr1 unionVersionRanges (fmap op vs)
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
