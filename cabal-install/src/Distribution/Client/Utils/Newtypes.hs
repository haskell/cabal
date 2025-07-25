{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | This module provides @newtype@ wrappers to be used with "Distribution.FieldGrammar".
-- Whenever we can not provide a Parsec instance for a type, we need to wrap it in a newtype and define the instance.
module Distribution.Client.Utils.Newtypes
  ( NumJobs (..)
  , PackageDBNT (..)
  , AllowNewerNT (..)
  , AllowOlderNT (..)
  , ProjectConstraints (..)
  , MaxBackjumps (..)
  , URI_NT (..)
  , KeyThreshold (..)
  )
where

import Distribution.Client.Compat.Prelude
import Distribution.Client.Targets (UserConstraint)
import Distribution.Client.Types.AllowNewer (AllowNewer (..), AllowOlder (..))
import Distribution.Compat.CharParsing
import Distribution.Compat.Newtype
import Distribution.Parsec
import Distribution.Simple.Compiler (PackageDBCWD, interpretPackageDB, readPackageDb)
import Distribution.Solver.Types.ConstraintSource (ConstraintSource (..))
import Network.URI (URI, parseURI)

newtype PackageDBNT = PackageDBNT {getPackageDBNT :: Maybe PackageDBCWD}

instance Newtype (Maybe PackageDBCWD) PackageDBNT

instance Parsec PackageDBNT where
  parsec = parsecPackageDB

parsecPackageDB :: CabalParsing m => m PackageDBNT
parsecPackageDB = PackageDBNT . fmap (interpretPackageDB Nothing) . readPackageDb <$> parsecToken

newtype NumJobs = NumJobs {getNumJobs :: Maybe Int}

instance Newtype (Maybe Int) NumJobs

instance Parsec NumJobs where
  parsec = parsecNumJobs

parsecNumJobs :: CabalParsing m => m NumJobs
parsecNumJobs = ncpus <|> numJobs
  where
    ncpus = string "$ncpus" >> return (NumJobs Nothing)
    numJobs = do
      num <- integral
      if num < (1 :: Int)
        then do
          parsecWarning PWTOther "The number of jobs should be 1 or more."
          return (NumJobs Nothing)
        else return (NumJobs $ Just num)

newtype URI_NT = URI_NT {getURI_NT :: URI}

instance Newtype (URI) URI_NT

instance Parsec URI_NT where
  parsec = parsecURI_NT

parsecURI_NT :: CabalParsing m => m URI_NT
parsecURI_NT = do
  token <- parsecToken'
  case parseURI token of
    Nothing -> fail $ "failed to parse URI " <> token
    Just uri -> return $ URI_NT uri

newtype KeyThreshold = KeyThreshold {getKeyThreshold :: Int}

instance Newtype Int KeyThreshold

instance Parsec KeyThreshold where
  parsec = KeyThreshold <$> integral

newtype ProjectConstraints = ProjectConstraints {getProjectConstraints :: (UserConstraint, ConstraintSource)}

instance Newtype (UserConstraint, ConstraintSource) ProjectConstraints

instance Parsec ProjectConstraints where
  parsec = parsecProjectConstraints

-- | Parse 'ProjectConstraints'. As the 'CabalParsing' class does not have access to the file we parse,
-- ConstraintSource is first unknown and we set it afterwards
parsecProjectConstraints :: CabalParsing m => m ProjectConstraints
parsecProjectConstraints = do
  userConstraint <- parsec
  return $ ProjectConstraints (userConstraint, ConstraintSourceUnknown)

newtype MaxBackjumps = MaxBackjumps {getMaxBackjumps :: Int}

instance Newtype Int MaxBackjumps

instance Parsec MaxBackjumps where
  parsec = parseMaxBackjumps

parseMaxBackjumps :: CabalParsing m => m MaxBackjumps
parseMaxBackjumps = MaxBackjumps <$> integral

newtype AllowNewerNT = AllowNewerNT {getAllowNewerNT :: Maybe AllowNewer}

instance Newtype (Maybe AllowNewer) AllowNewerNT

instance Parsec AllowNewerNT where
  parsec = parsecAllowNewer

parsecAllowNewer :: CabalParsing m => m AllowNewerNT
parsecAllowNewer = AllowNewerNT . Just <$> parsec

newtype AllowOlderNT = AllowOlderNT {getAllowOlderNT :: Maybe AllowOlder}

instance Newtype (Maybe AllowOlder) AllowOlderNT

instance Parsec AllowOlderNT where
  parsec = parsecAllowOlder

parsecAllowOlder :: CabalParsing m => m AllowOlderNT
parsecAllowOlder = AllowOlderNT . Just <$> parsec
