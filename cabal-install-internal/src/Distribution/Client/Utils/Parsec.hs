{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Distribution.Client.Utils.Parsec
  ( remoteRepoGrammar

    -- ** Flag
  , alaFlag
  , Flag' (..)

    -- ** NubList
  , alaNubList
  , alaNubList'
  , NubList' (..)

    -- ** Newtype wrappers
  , module Distribution.Client.Utils.Newtypes
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Data.Coerce (Coercible, coerce)
import Distribution.Client.Types.Repo
import Distribution.Client.Types.RepoName
import Distribution.Client.Utils.Newtypes
import Distribution.FieldGrammar
import Distribution.Simple.Flag
import Distribution.Utils.NubList (NubList (..))
import qualified Distribution.Utils.NubList as NubList

-- | Like 'List' for usage with a 'FieldGrammar', but for 'Flag'.
-- This enables to parse type aliases such as 'FilePath' that do not have 'Parsec' instances
-- by using newtype variants such as 'FilePathNT'.
-- For example, if you need to parse a 'Flag FilePath', you can use 'alaFlag' FilePathNT'.
newtype Flag' b a = Flag' {_getFlag :: Flag a}

-- | 'Flag'' constructor, with additional phantom argument to constrain the resulting type
alaFlag :: (a -> b) -> Flag a -> Flag' b a
alaFlag _ = Flag'

instance (Coercible a b, Parsec b) => Parsec (Flag' b a) where
  parsec = coerce . toFlag . (coerce :: b -> a) <$> parsec

instance (Coercible a b, Pretty b) => Pretty (Flag' b a) where
  pretty = pretty . (coerce :: a -> b) . fromFlag . coerce

-- | Like 'List' for usage with a 'FieldGrammar', but for 'NubList'.
newtype NubList' sep b a = NubList' {_getNubList :: NubList a}

-- | 'alaNubList' and 'alaNubList'' are simply 'NubList'' constructor, with additional phantom
-- arguments to constrain the resulting type
--
-- >>> :t alaNubList VCat
-- alaNubList VCat :: NubList a -> NubList' VCat (Identity a) a
--
-- >>> :t alaNubList' FSep Token
-- alaNubList' FSep Token
--   :: NubList String -> NubList' FSep Token String
alaNubList :: sep -> NubList a -> NubList' sep (Identity a) a
alaNubList _ = NubList'

-- | More general version of 'alaNubList'.
alaNubList' :: sep -> (a -> b) -> NubList a -> NubList' sep b a
alaNubList' _ _ = NubList'

instance (Coercible a b, Ord a, Sep sep, Parsec b) => Parsec (NubList' sep b a) where
  parsec = coerce . NubList.toNubList . map (coerce :: b -> a) <$> parseSep (Proxy :: Proxy sep) parsec

instance (Coercible a b, Sep sep, Pretty b) => Pretty (NubList' sep b a) where
  pretty = prettySep (Proxy :: Proxy sep) . map (pretty . (coerce :: a -> b)) . NubList.fromNubList . coerce

remoteRepoGrammar :: RepoName -> ParsecFieldGrammar RemoteRepo RemoteRepo
remoteRepoGrammar remoteRepoName = do
  remoteRepoURI <- uniqueFieldAla "url" URI_NT remoteRepoURILens
  remoteRepoSecure <- optionalField "secure" remoteRepoSecureLens
  remoteRepoRootKeys <- monoidalFieldAla "root-keys" (alaList' FSep Token) remoteRepoRootKeysLens
  remoteRepoKeyThreshold <- optionalFieldDefAla "key-threshold" KeyThreshold remoteRepoKeyThresholdLens 0
  pure
    RemoteRepo
      { remoteRepoShouldTryHttps = False -- we don't parse remoteRepoShouldTryHttps
      , ..
      }
