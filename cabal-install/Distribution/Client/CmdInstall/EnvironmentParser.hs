{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Distribution.Client.CmdInstall.EnvironmentParser 
    ( readEnvironmentFile, ParseErrorExc(..)
    , environmentFileToSpecifiers
    ) where

import Prelude ()
import Distribution.Client.Compat.Prelude
        
import Distribution.Client.Types
    ( PackageSpecifier(..) )
import Distribution.Simple.Compiler
    ( PackageDB(..) )
import Distribution.Simple.GHC
    ( GhcEnvironmentFileEntry(..) )
import Distribution.Solver.Types.PackageConstraint
    ( PackageProperty(..) )
import Distribution.Types.PackageId
    ( PackageIdentifier(..) )
import Distribution.Types.UnitId
    ( mkUnitId, unUnitId )
import Distribution.Types.VersionRange
    ( thisVersion )

import Control.Exception
    ( Exception, throwIO )
import Data.Char
    ( isAlphaNum )
import Data.Typeable
    ( Typeable )
import Distribution.Text
    ( simpleParse )
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

readEnvironmentFile :: FilePath -> IO [GhcEnvironmentFileEntry]
readEnvironmentFile path =
    either (throwIO . ParseErrorExc) return =<<
        parseFromFile parseEnvironmentFile path

environmentFileToSpecifiers :: [GhcEnvironmentFileEntry] -> [PackageSpecifier a]
environmentFileToSpecifiers = foldMap $ \case
    (GhcEnvFilePackageId unitId) 
        | Just PackageIdentifier{..} <- simpleParse (unUnitId unitId) -> 
            [ NamedPackage pkgName [PackagePropertyVersion (thisVersion pkgVersion)] ]
    _ -> []
