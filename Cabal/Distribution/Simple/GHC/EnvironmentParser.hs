{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
module Distribution.Simple.GHC.EnvironmentParser 
    ( readGhcEnvironmentFile, ParseErrorExc(..) ) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Simple.Compiler
    ( PackageDB(..) )
import Distribution.Simple.GHC.Internal
    ( GhcEnvironmentFileEntry(..) )
import Distribution.Types.UnitId
    ( mkUnitId )

import Control.Exception
    ( Exception, throwIO )
import Data.Char
    ( isAlphaNum )
import Data.Typeable
    ( Typeable )
import qualified Text.Parsec as P
import Text.Parsec.String 
    ( Parser, parseFromFile )

parseEnvironmentFileLine :: Parser GhcEnvironmentFileEntry
parseEnvironmentFileLine =      GhcEnvFileComment             <$> comment
                       <|>      GhcEnvFilePackageId           <$> unitId
                       <|>      GhcEnvFilePackageDb           <$> packageDb
                       <|> pure GhcEnvFileClearPackageDbStack <*  clearDb
    where
        comment = P.string "--" *> P.many (P.noneOf "\r\n")
        unitId = P.string "package-id" *> P.spaces *> 
            (mkUnitId <$> P.many1 (P.satisfy $ \c -> isAlphaNum c || c `elem` "-_.+"))
        packageDb = (P.string "global-package-db"      *> pure GlobalPackageDB)
                <|> (P.string "user-package-db"        *> pure UserPackageDB)
                <|> (P.string "package-db" *> P.spaces *> (SpecificPackageDB <$> P.anyChar `P.endBy` P.endOfLine))
        clearDb = P.string "clear-package-db"

newtype ParseErrorExc = ParseErrorExc P.ParseError
                      deriving (Show, Typeable)

instance Exception ParseErrorExc

parseEnvironmentFile :: Parser [GhcEnvironmentFileEntry]
parseEnvironmentFile = parseEnvironmentFileLine `P.endBy` P.endOfLine

readGhcEnvironmentFile :: FilePath -> IO [GhcEnvironmentFileEntry]
readGhcEnvironmentFile path =
    either (throwIO . ParseErrorExc) return =<<
        parseFromFile parseEnvironmentFile path
