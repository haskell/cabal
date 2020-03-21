{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.IndexUtils.IndexUtils
-- Copyright   :  (c) 2016 Herbert Valerio Riedel
-- License     :  BSD3
--
-- Package repositories index state.
--
module Distribution.Client.IndexUtils.IndexState (
    RepoIndexState(..),
    TotalIndexState,
    headTotalIndexState,
    makeTotalIndexState,
    lookupIndexState,
) where

import Distribution.Client.Compat.Prelude
import Distribution.Client.IndexUtils.Timestamp (Timestamp)
import Distribution.Client.Types                (RepoName (..))

import Distribution.FieldGrammar.Described
import Distribution.Parsec                 (Parsec (..))
import Distribution.Pretty                 (Pretty (..))

import qualified Distribution.Compat.CharParsing as P
import qualified Data.Map.Strict as Map
import qualified Text.PrettyPrint                as Disp

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
        | not (Map.null m)
        = Disp.hsep
            [ pretty rn <<>> Disp.colon <<>> pretty idx
            | (rn, idx) <- Map.toList m
            ]
    pretty (TIS def m) = foldl' go (pretty def) (Map.toList m) where
        go doc (rn, idx) = doc Disp.<+> pretty rn <<>> Disp.colon <<>> pretty idx

instance Parsec TotalIndexState where
    parsec = normalise . foldl' add headTotalIndexState <$> some (single0 <* P.spaces) where
        -- hard to do without try
        -- 2020-03-21T11:22:33Z looks like it begins with
        -- repository name 2020-03-21T11
        --
        -- To make this easy, we could forbid repository names starting with digit
        --
        single0 = P.try single1 <|> TokTimestamp <$> parsec
        single1 = do
            token <- P.munch1 (\c -> isAlphaNum c || c == '_' || c == '-' || c == '.')
            single2 token <|> single3 token

        single2 token = do
            _   <- P.char ':'
            idx <- parsec
            return (TokRepo (RepoName token) idx)

        single3 "HEAD" = return TokHead
        single3 token  = P.unexpected ("Repository " ++ token ++ " without index state (after comma)")

        add :: TotalIndexState -> Tok -> TotalIndexState
        add _           TokHead           = headTotalIndexState
        add _           (TokTimestamp ts) = TIS (IndexStateTime ts) Map.empty
        add (TIS def m) (TokRepo rn idx)  = TIS def (Map.insert rn idx m)

instance Described TotalIndexState where
    describe _ = REMunch1 RESpaces1 $ REUnion
        [ describe (Proxy :: Proxy RepoName) <> reChar ':' <> ris
        , ris
        ]
      where
        ris = describe (Proxy :: Proxy RepoIndexState)

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

-------------------------------------------------------------------------------
-- Repository index state
-------------------------------------------------------------------------------

-- | Specification of the state of a specific repo package index
data RepoIndexState
    = IndexStateHead -- ^ Use all available entries
    | IndexStateTime !Timestamp -- ^ Use all entries that existed at the specified time
    deriving (Eq,Generic,Show)

instance Binary RepoIndexState
instance Structured RepoIndexState
instance NFData RepoIndexState

instance Pretty RepoIndexState where
    pretty IndexStateHead = Disp.text "HEAD"
    pretty (IndexStateTime ts) = pretty ts

instance Parsec RepoIndexState where
    parsec = parseHead <|> parseTime where
        parseHead = IndexStateHead <$ P.string "HEAD"
        parseTime = IndexStateTime <$> parsec

instance Described RepoIndexState where
    describe _ = REUnion
        [ "HEAD"
        , RENamed "timestamp" (describe (Proxy :: Proxy Timestamp))
        ]
