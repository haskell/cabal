{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Distribution.Simple.GHC.EnvironmentParser (parseGhcEnvironmentFile, readGhcEnvironmentFile, ParseErrorExc (..)) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Simple.Compiler
  ( PackageDB (..)
  )
import Distribution.Simple.GHC.Internal
  ( GhcEnvironmentFileEntry (..)
  )
import Distribution.Types.UnitId
  ( mkUnitId
  )

import qualified Text.Parsec as P
import Text.Parsec.String
  ( Parser
  , parseFromFile
  )

parseEnvironmentFileLine :: Parser GhcEnvironmentFileEntry
parseEnvironmentFileLine =
  GhcEnvFileComment <$> comment
    <|> GhcEnvFilePackageId <$> unitId
    <|> GhcEnvFilePackageDb <$> packageDb
    <|> pure GhcEnvFileClearPackageDbStack <* clearDb
  where
    comment = P.string "--" *> P.many (P.noneOf "\r\n")
    unitId =
      P.try $
        P.string "package-id"
          *> P.spaces
          *> (mkUnitId <$> P.many1 (P.satisfy $ \c -> isAlphaNum c || c `elem` "-_.+"))
    packageDb =
      (P.string "global-package-db" *> pure GlobalPackageDB)
        <|> (P.string "user-package-db" *> pure UserPackageDB)
        <|> (P.string "package-db" *> P.spaces *> (SpecificPackageDB <$> P.many1 (P.noneOf "\r\n") <* P.lookAhead P.endOfLine))
    clearDb = P.string "clear-package-db"

newtype ParseErrorExc = ParseErrorExc P.ParseError
  deriving (Show, Typeable)

instance Exception ParseErrorExc

parseGhcEnvironmentFile :: Parser [GhcEnvironmentFileEntry]
parseGhcEnvironmentFile = parseEnvironmentFileLine `P.sepEndBy` P.endOfLine <* P.eof

readGhcEnvironmentFile :: FilePath -> IO [GhcEnvironmentFileEntry]
readGhcEnvironmentFile path =
  either (throwIO . ParseErrorExc) return
    =<< parseFromFile parseGhcEnvironmentFile path
