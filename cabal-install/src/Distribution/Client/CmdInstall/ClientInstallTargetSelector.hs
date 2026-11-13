module Distribution.Client.CmdInstall.ClientInstallTargetSelector
  ( WithoutProjectTargetSelector (..)
  , parseWithoutProjectTargetSelector
  , woPackageNames
  , woPackageTargets
  , woPackageSpecifiers
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Network.URI (URI, parseURI)

import Distribution.Client.Errors
import Distribution.Client.TargetSelector
import Distribution.Client.Types
import Distribution.Compat.CharParsing (char, optional)
import Distribution.Package
import Distribution.Simple.LocalBuildInfo (ComponentName (CExeName))
import Distribution.Simple.Utils (dieWithException)

data WithoutProjectTargetSelector
  = WoPackageId PackageId
  | WoPackageComponent PackageId ComponentName
  | WoURI URI
  deriving (Show)

parseWithoutProjectTargetSelector :: Verbosity -> String -> IO WithoutProjectTargetSelector
parseWithoutProjectTargetSelector verbosity input =
  case explicitEitherParsec parser input of
    Right ts -> return ts
    Left err -> case parseURI input of
      Just uri -> return (WoURI uri)
      Nothing -> dieWithException verbosity $ ProjectTargetSelector input err
  where
    parser :: CabalParsing m => m WithoutProjectTargetSelector
    parser = do
      pid <- parsec
      cn <- optional (char ':' *> parsec)
      return $ case cn of
        Nothing -> WoPackageId pid
        Just cn' -> WoPackageComponent pid (CExeName cn')

woPackageNames :: WithoutProjectTargetSelector -> [PackageName]
woPackageNames (WoPackageId pid) = [pkgName pid]
woPackageNames (WoPackageComponent pid _) = [pkgName pid]
woPackageNames (WoURI _) = []

-- | The target selector for a package named on the command line. A URI has no
-- selector yet: the package it names can only be known once its tarball has
-- been fetched and read.
woPackageTargets :: WithoutProjectTargetSelector -> Maybe TargetSelector
woPackageTargets (WoPackageId pid) =
  Just $ TargetPackageNamed (pkgName pid) Nothing
woPackageTargets (WoPackageComponent pid cn) =
  Just $ TargetComponentUnknown (pkgName pid) (Right cn) WholeComponent
woPackageTargets (WoURI _) =
  Nothing

woPackageSpecifiers :: WithoutProjectTargetSelector -> Either URI (PackageSpecifier pkg)
woPackageSpecifiers (WoPackageId pid) = Right (mkNamedPackage pid)
woPackageSpecifiers (WoPackageComponent pid _) = Right (mkNamedPackage pid)
woPackageSpecifiers (WoURI uri) = Left uri
