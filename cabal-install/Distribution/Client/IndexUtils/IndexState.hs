{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.IndexUtils.IndexUtils
-- Copyright   :  (c) 2016 Herbert Valerio Riedel
-- License     :  BSD3
--
-- Timestamp type used in package indexes
module Distribution.Client.IndexUtils.IndexState (
    IndexState(..),
) where

import Distribution.Client.Compat.Prelude
import Distribution.Client.IndexUtils.Timestamp (Timestamp)

import Distribution.FieldGrammar.Described
import Distribution.Parsec                 (Parsec (..))
import Distribution.Pretty                 (Pretty (..))

import qualified Distribution.Compat.CharParsing as P
import qualified Text.PrettyPrint                as Disp

-- | Specification of the state of a specific repo package index
data IndexState = IndexStateHead -- ^ Use all available entries
                | IndexStateTime !Timestamp -- ^ Use all entries that existed at
                                            -- the specified time
                deriving (Eq,Generic,Show)

instance Binary IndexState
instance Structured IndexState
instance NFData IndexState

instance Pretty IndexState where
    pretty IndexStateHead = Disp.text "HEAD"
    pretty (IndexStateTime ts) = pretty ts

instance Parsec IndexState where
    parsec = parseHead <|> parseTime where
        parseHead = IndexStateHead <$ P.string "HEAD"
        parseTime = IndexStateTime <$> parsec

instance Described IndexState where
    describe _ = REUnion
        [ "HEAD"
        , RENamed "timestamp" (describe (Proxy :: Proxy Timestamp))
        ]
