{-# LANGUAGE DeriveGeneric #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.BuildReports.Types
-- Copyright   :  (c) Duncan Coutts 2009
-- License     :  BSD-like
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Types related to build reporting
--
-----------------------------------------------------------------------------
module Distribution.Client.BuildReports.Types (
    ReportLevel(..),
  ) where

import qualified Distribution.Compat.CharParsing as P
import qualified Text.PrettyPrint as Disp

import Data.Char as Char
         ( isAlpha, toLower )
import GHC.Generics (Generic)
import Distribution.Compat.Binary (Binary)
import Distribution.Parsec (Parsec (..))
import Distribution.Pretty (Pretty (..))
import Distribution.Utils.Structured (Structured)

data ReportLevel = NoReports | AnonymousReports | DetailedReports
  deriving (Eq, Ord, Enum, Bounded, Show, Generic)

instance Binary ReportLevel
instance Structured ReportLevel

instance Pretty ReportLevel where
  pretty NoReports        = Disp.text "none"
  pretty AnonymousReports = Disp.text "anonymous"
  pretty DetailedReports  = Disp.text "detailed"

instance Parsec ReportLevel where
  parsec = do
    name <- P.munch1 Char.isAlpha
    case lowercase name of
      "none"       -> return NoReports
      "anonymous"  -> return AnonymousReports
      "detailed"   -> return DetailedReports
      _            -> P.unexpected $ "ReportLevel: " ++ name

lowercase :: String -> String
lowercase = map Char.toLower
