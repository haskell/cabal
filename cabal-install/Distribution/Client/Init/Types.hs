{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Init.Types
-- Copyright   :  (c) Brent Yorgey, Benedikt Huber 2009
-- License     :  BSD-like
--
-- Maintainer  :  cabal-devel@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Some types used by the 'cabal init' command.
--
-----------------------------------------------------------------------------
module Distribution.Client.Init.Types where

import Distribution.Simple.Setup
  ( Flag(..) )

import Distribution.Version
import qualified Distribution.Package as P
import Distribution.License
import Distribution.ModuleName

import qualified Text.PrettyPrint as Disp
import qualified Distribution.Compat.ReadP as Parse
import Distribution.Text

import Data.Monoid

-- | InitFlags is really just a simple type to represent certain
--   portions of a .cabal file.  Rather than have a flag for EVERY
--   possible field, we just have one for each field that the user is
--   likely to want and/or that we are likely to be able to
--   intelligently guess.
data InitFlags =
    InitFlags { nonInteractive :: Flag Bool
              , quiet          :: Flag Bool
              , packageDir     :: Flag FilePath
              , noComments     :: Flag Bool
              , minimal        :: Flag Bool

              , packageName  :: Flag String
              , version      :: Flag Version
              , cabalVersion :: Flag VersionRange
              , license      :: Flag License
              , author       :: Flag String
              , email        :: Flag String
              , homepage     :: Flag String

              , synopsis     :: Flag String
              , category     :: Flag (Either String Category)

              , packageType  :: Flag PackageType

              , exposedModules :: Maybe [ModuleName]
              , otherModules   :: Maybe [ModuleName]

              , dependencies :: Maybe [P.Dependency]
              , sourceDirs   :: Maybe [String]
              , buildTools   :: Maybe [String]
              }
  deriving (Show)

data PackageType = Library | Executable
  deriving (Show, Read, Eq)

instance Text PackageType where
  disp = Disp.text . show
  parse = Parse.choice $ map (fmap read . Parse.string . show) [Library, Executable]

instance Monoid InitFlags where
  mempty = InitFlags
    { nonInteractive = mempty
    , quiet          = mempty
    , packageDir     = mempty
    , noComments     = mempty
    , minimal        = mempty
    , packageName    = mempty
    , version        = mempty
    , cabalVersion   = mempty
    , license        = mempty
    , author         = mempty
    , email          = mempty
    , homepage       = mempty
    , synopsis       = mempty
    , category       = mempty
    , packageType    = mempty
    , exposedModules = mempty
    , otherModules   = mempty
    , dependencies   = mempty
    , sourceDirs     = mempty
    , buildTools     = mempty
    }
  mappend  a b = InitFlags
    { nonInteractive = combine nonInteractive
    , quiet          = combine quiet
    , packageDir     = combine packageDir
    , noComments     = combine noComments
    , minimal        = combine minimal
    , packageName    = combine packageName
    , version        = combine version
    , cabalVersion   = combine cabalVersion
    , license        = combine license
    , author         = combine author
    , email          = combine email
    , homepage       = combine homepage
    , synopsis       = combine synopsis
    , category       = combine category
    , packageType    = combine packageType
    , exposedModules = combine exposedModules
    , otherModules   = combine otherModules
    , dependencies   = combine dependencies
    , sourceDirs     = combine sourceDirs
    , buildTools     = combine buildTools
    }
    where combine field = field a `mappend` field b

-- | Some common package categories.
data Category
    = Codec
    | Concurrency
    | Control
    | Data
    | Database
    | Development
    | Distribution
    | Game
    | Graphics
    | Language
    | Math
    | Network
    | Sound
    | System
    | Testing
    | Text
    | Web
    deriving (Read, Show, Eq, Ord, Bounded, Enum)

instance Text Category where
  disp  = Disp.text . show
  parse = Parse.choice $ map (fmap read . Parse.string . show) [Codec .. ]

#if MIN_VERSION_base(3,0,0)
#else
-- Compat instance for ghc-6.6 era
instance Monoid a => Monoid (Maybe a) where
  mempty = Nothing
  Nothing `mappend` m = m
  m `mappend` Nothing = m
  Just m1 `mappend` Just m2 = Just (m1 `mappend` m2)
#endif
