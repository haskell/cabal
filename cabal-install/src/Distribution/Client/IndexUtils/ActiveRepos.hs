{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Distribution.Client.IndexUtils.ActiveRepos
  ( ActiveRepos (..)
  , defaultActiveRepos
  , filterSkippedActiveRepos
  , ActiveRepoEntry (..)
  , CombineStrategy (..)
  , organizeByRepos
  ) where

import Distribution.Client.Compat.Prelude
import Distribution.Client.Types.RepoName (RepoName (..))
import Prelude ()

import Distribution.Parsec (parsecLeadingCommaNonEmpty)

import qualified Distribution.Compat.CharParsing as P
import qualified Text.PrettyPrint as Disp

-- $setup
-- >>> import Distribution.Parsec

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Ordered list of active repositories.
newtype ActiveRepos = ActiveRepos [ActiveRepoEntry]
  deriving (Eq, Show, Generic)

defaultActiveRepos :: ActiveRepos
defaultActiveRepos = ActiveRepos [ActiveRepoRest CombineStrategyMerge]

-- | Note, this does nothing if 'ActiveRepoRest' is present.
filterSkippedActiveRepos :: ActiveRepos -> ActiveRepos
filterSkippedActiveRepos repos@(ActiveRepos entries)
  | any isActiveRepoRest entries = repos
  | otherwise = ActiveRepos (filter notSkipped entries)
  where
    isActiveRepoRest (ActiveRepoRest _) = True
    isActiveRepoRest _ = False

    notSkipped (ActiveRepo _ CombineStrategySkip) = False
    notSkipped _ = True

instance Binary ActiveRepos
instance Structured ActiveRepos
instance NFData ActiveRepos

instance Pretty ActiveRepos where
  pretty (ActiveRepos []) =
    Disp.text ":none"
  pretty (ActiveRepos repos) =
    Disp.hsep $
      Disp.punctuate Disp.comma $
        map pretty repos

-- | Note: empty string is not valid 'ActiveRepos'.
--
-- >>> simpleParsec "" :: Maybe ActiveRepos
-- Nothing
--
-- >>> simpleParsec ":none" :: Maybe ActiveRepos
-- Just (ActiveRepos [])
--
-- >>> simpleParsec ":rest" :: Maybe ActiveRepos
-- Just (ActiveRepos [ActiveRepoRest CombineStrategyMerge])
--
-- >>> simpleParsec "hackage.haskell.org, :rest, head.hackage:override" :: Maybe ActiveRepos
-- Just (ActiveRepos [ActiveRepo (RepoName "hackage.haskell.org") CombineStrategyMerge,ActiveRepoRest CombineStrategyMerge,ActiveRepo (RepoName "head.hackage") CombineStrategyOverride])
instance Parsec ActiveRepos where
  parsec =
    ActiveRepos [] <$ P.try (P.string ":none")
      <|> do
        repos <- parsecLeadingCommaNonEmpty parsec
        return (ActiveRepos (toList repos))

data ActiveRepoEntry
  = -- | rest repositories, i.e. not explicitly listed as 'ActiveRepo'
    ActiveRepoRest CombineStrategy
  | -- | explicit repository name
    ActiveRepo RepoName CombineStrategy
  deriving (Eq, Show, Generic)

instance Binary ActiveRepoEntry
instance Structured ActiveRepoEntry
instance NFData ActiveRepoEntry

instance Pretty ActiveRepoEntry where
  pretty (ActiveRepoRest s) =
    Disp.text ":rest" <<>> Disp.colon <<>> pretty s
  pretty (ActiveRepo r s) =
    pretty r <<>> Disp.colon <<>> pretty s

instance Parsec ActiveRepoEntry where
  parsec = leadColon <|> leadRepo
    where
      leadColon = do
        _ <- P.char ':'
        token <- P.munch1 isAlpha
        case token of
          "rest" -> ActiveRepoRest <$> strategyP
          "repo" -> P.char ':' *> leadRepo
          _ -> P.unexpected $ "Unknown active repository entry type: " ++ token

      leadRepo = do
        r <- parsec
        s <- strategyP
        return (ActiveRepo r s)

      strategyP = P.option CombineStrategyMerge (P.char ':' *> parsec)

data CombineStrategy
  = -- | skip this repository
    CombineStrategySkip
  | -- | merge existing versions
    CombineStrategyMerge
  | -- | if later repository specifies a package,
    --   all package versions are replaced
    CombineStrategyOverride
  deriving (Eq, Show, Enum, Bounded, Generic)

instance Binary CombineStrategy
instance Structured CombineStrategy
instance NFData CombineStrategy

instance Pretty CombineStrategy where
  pretty CombineStrategySkip = Disp.text "skip"
  pretty CombineStrategyMerge = Disp.text "merge"
  pretty CombineStrategyOverride = Disp.text "override"

instance Parsec CombineStrategy where
  parsec =
    P.choice
      [ CombineStrategySkip <$ P.string "skip"
      , CombineStrategyMerge <$ P.string "merge"
      , CombineStrategyOverride <$ P.string "override"
      ]

-------------------------------------------------------------------------------
-- Organisation
-------------------------------------------------------------------------------

-- | Sort values 'RepoName' according to 'ActiveRepos' list.
--
-- >>> let repos = [RepoName "a", RepoName "b", RepoName "c"]
-- >>> organizeByRepos (ActiveRepos [ActiveRepoRest CombineStrategyMerge]) id repos
-- Right [(RepoName "a",CombineStrategyMerge),(RepoName "b",CombineStrategyMerge),(RepoName "c",CombineStrategyMerge)]
--
-- >>> organizeByRepos (ActiveRepos [ActiveRepo (RepoName "b") CombineStrategyOverride, ActiveRepoRest CombineStrategyMerge]) id repos
-- Right [(RepoName "b",CombineStrategyOverride),(RepoName "a",CombineStrategyMerge),(RepoName "c",CombineStrategyMerge)]
--
-- >>> organizeByRepos (ActiveRepos [ActiveRepoRest CombineStrategyMerge, ActiveRepo (RepoName "b") CombineStrategyOverride]) id repos
-- Right [(RepoName "a",CombineStrategyMerge),(RepoName "c",CombineStrategyMerge),(RepoName "b",CombineStrategyOverride)]
--
-- >>> organizeByRepos (ActiveRepos [ActiveRepoRest CombineStrategyMerge, ActiveRepo (RepoName "d") CombineStrategyOverride]) id repos
-- Left "no repository provided d"
--
-- Note: currently if 'ActiveRepoRest' is provided more than once,
-- rest-repositories will be multiple times in the output.
organizeByRepos
  :: forall a
   . ActiveRepos
  -> (a -> RepoName)
  -> [a]
  -> Either String [(a, CombineStrategy)]
organizeByRepos (ActiveRepos xs0) sel ys0 =
  -- here we use lazyness to do only one traversal
  let (rest, result) = case go rest xs0 ys0 of
        Right (rest', result') -> (rest', Right result')
        Left err -> ([], Left err)
   in result
  where
    go :: [a] -> [ActiveRepoEntry] -> [a] -> Either String ([a], [(a, CombineStrategy)])
    go _rest [] ys = Right (ys, [])
    go rest (ActiveRepoRest s : xs) ys =
      go rest xs ys <&> \(rest', result) ->
        (rest', map (\x -> (x, s)) rest ++ result)
    go rest (ActiveRepo r s : xs) ys = do
      (z, zs) <- extract r ys
      go rest xs zs <&> \(rest', result) ->
        (rest', (z, s) : result)

    extract :: RepoName -> [a] -> Either String (a, [a])
    extract r = loop id
      where
        loop _acc [] = Left $ "no repository provided " ++ prettyShow r
        loop acc (x : xs)
          | sel x == r = Right (x, acc xs)
          | otherwise = loop (acc . (x :)) xs

    (<&>)
      :: Either err ([s], b)
      -> (([s], b) -> ([s], c))
      -> Either err ([s], c)
    (<&>) = flip fmap
