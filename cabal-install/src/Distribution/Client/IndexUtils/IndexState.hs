{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------

-- |
-- Module      :  Distribution.Client.IndexUtils.IndexUtils
-- Copyright   :  (c) 2016 Herbert Valerio Riedel
-- License     :  BSD3
--
-- Package repositories index state.
module Distribution.Client.IndexUtils.IndexState
  ( RepoIndexState (..)
  , TotalIndexState
  , headTotalIndexState
  , makeTotalIndexState
  , lookupIndexState
  , insertIndexState
  ) where

import Distribution.Client.Compat.Prelude
import Distribution.Client.IndexUtils.Timestamp (Timestamp)
import Distribution.Client.Types.RepoName (RepoName (..))

import Distribution.Parsec (parsecLeadingCommaNonEmpty)

import qualified Data.Map.Strict as Map
import qualified Distribution.Compat.CharParsing as P
import qualified Text.PrettyPrint as Disp

-- $setup
-- >>> import Distribution.Parsec

-------------------------------------------------------------------------------
-- Total index state
-------------------------------------------------------------------------------

-- | Index state of multiple repositories
data TotalIndexState = TIS RepoIndexState (Map RepoName RepoIndexState)
  deriving (Eq, Show, Generic)

instance Binary TotalIndexState
instance Structured TotalIndexState
instance NFData TotalIndexState

instance Pretty TotalIndexState where
  pretty (TIS IndexStateHead m)
    | not (Map.null m) =
        Disp.hsep $
          Disp.punctuate
            Disp.comma
            [ pretty rn Disp.<+> pretty idx
            | (rn, idx) <- Map.toList m
            ]
  pretty (TIS def m) = foldl' go (pretty def) (Map.toList m)
    where
      go doc (rn, idx) = doc <<>> Disp.comma Disp.<+> pretty rn Disp.<+> pretty idx

-- |
--
-- >>> simpleParsec "HEAD" :: Maybe TotalIndexState
-- Just (TIS IndexStateHead (fromList []))
--
-- >>> simpleParsec "" :: Maybe TotalIndexState
-- Nothing
--
-- >>> simpleParsec "hackage.haskell.org HEAD" :: Maybe TotalIndexState
-- Just (TIS IndexStateHead (fromList []))
--
-- >>> simpleParsec "2020-02-04T12:34:56Z, hackage.haskell.org HEAD" :: Maybe TotalIndexState
-- Just (TIS (IndexStateTime (TS 1580819696)) (fromList [(RepoName "hackage.haskell.org",IndexStateHead)]))
--
-- >>> simpleParsec "hackage.haskell.org 2020-02-04T12:34:56Z" :: Maybe TotalIndexState
-- Just (TIS IndexStateHead (fromList [(RepoName "hackage.haskell.org",IndexStateTime (TS 1580819696))]))
instance Parsec TotalIndexState where
  parsec = normalise . foldl' add headTotalIndexState <$> parsecLeadingCommaNonEmpty single0
    where
      single0 = startsWithRepoName <|> TokTimestamp <$> parsec
      startsWithRepoName = do
        reponame <- parsec
        -- the "HEAD" is technically a valid reponame...
        if reponame == RepoName "HEAD"
          then return TokHead
          else do
            P.spaces
            TokRepo reponame <$> parsec

      add :: TotalIndexState -> Tok -> TotalIndexState
      add _ TokHead = headTotalIndexState
      add _ (TokTimestamp ts) = TIS (IndexStateTime ts) Map.empty
      add (TIS def m) (TokRepo rn idx) = TIS def (Map.insert rn idx m)

-- used in Parsec TotalIndexState implementation
data Tok
  = TokRepo RepoName RepoIndexState
  | TokTimestamp Timestamp
  | TokHead

-- | Remove non-default values from 'TotalIndexState'.
normalise :: TotalIndexState -> TotalIndexState
normalise (TIS def m) = TIS def (Map.filter (/= def) m)

-- | 'TotalIndexState' where all repositories are at @HEAD@ index state.
headTotalIndexState :: TotalIndexState
headTotalIndexState = TIS IndexStateHead Map.empty

-- | Create 'TotalIndexState'.
makeTotalIndexState :: RepoIndexState -> Map RepoName RepoIndexState -> TotalIndexState
makeTotalIndexState def m = normalise (TIS def m)

-- | Lookup a 'RepoIndexState' for an individual repository from 'TotalIndexState'.
lookupIndexState :: RepoName -> TotalIndexState -> RepoIndexState
lookupIndexState rn (TIS def m) = Map.findWithDefault def rn m

-- | Insert a 'RepoIndexState' to 'TotalIndexState'.
insertIndexState :: RepoName -> RepoIndexState -> TotalIndexState -> TotalIndexState
insertIndexState rn idx (TIS def m)
  | idx == def = TIS def (Map.delete rn m)
  | otherwise = TIS def (Map.insert rn idx m)

-------------------------------------------------------------------------------
-- Repository index state
-------------------------------------------------------------------------------

-- | Specification of the state of a specific repo package index
data RepoIndexState
  = -- | Use all available entries
    IndexStateHead
  | -- | Use all entries that existed at the specified time
    IndexStateTime !Timestamp
  deriving (Eq, Generic, Show)

instance Binary RepoIndexState
instance Structured RepoIndexState
instance NFData RepoIndexState

instance Pretty RepoIndexState where
  pretty IndexStateHead = Disp.text "HEAD"
  pretty (IndexStateTime ts) = pretty ts

instance Parsec RepoIndexState where
  parsec = parseHead <|> parseTime
    where
      parseHead = IndexStateHead <$ P.string "HEAD"
      parseTime = IndexStateTime <$> parsec
