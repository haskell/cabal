{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- | Utilities for understanding @plan.json@.
module Test.Cabal.Plan (
    Plan(..),
    DistDirOrBinFile(..),
    InstallItem(..),
    ConfiguredGlobal(..),
    Revision(..),
    PkgSrc(..),
    Repo(..),
    planDistDir,
    buildInfoFile,
) where

import Distribution.Parsec (simpleParsec)
import Distribution.Pretty (prettyShow)
import Distribution.Types.ComponentName
import Distribution.Package
import qualified Data.Text as Text
import Data.Aeson
import Data.Aeson.Types
import Control.Monad
import GHC.Generics (Generic)

-- TODO: index this
data Plan = Plan { planInstallPlan :: [InstallItem] }
  deriving Show

data InstallItem
    = APreExisting
    | AConfiguredGlobal ConfiguredGlobal
    | AConfiguredInplace ConfiguredInplace
  deriving Show

-- local or inplace package
data ConfiguredInplace = ConfiguredInplace
    { configuredInplaceDistDir       :: FilePath
    , configuredInplaceBuildInfo     :: Maybe FilePath
    , configuredInplacePackageName   :: PackageName
    , configuredInplaceComponentName :: Maybe ComponentName
    , configuredInplacePkgSrc        :: PkgSrc }
  deriving Show

data ConfiguredGlobal = ConfiguredGlobal
    { configuredGlobalBinFile       :: Maybe FilePath
    , configuredGlobalPackageName   :: PackageName
    , configuredGlobalComponentName :: Maybe ComponentName
    , configuredGlobalPkgSrc        :: PkgSrc }
  deriving Show

newtype Revision = Revision Int
  deriving (Show, Eq, FromJSON)

-- | A stripped-down 'Distribution.Client.Types.PackageLocation.PackageLocation'
data PkgSrc
    = RepoTar { repo :: Repo }
    | PkgSrcOther
  deriving (Show, Generic)

-- | A stripped-down 'Distribution.Client.Types.Repo.Repo', plus revision information
data Repo
    = LocalRepoNoIndex
    | RemoteRepo { pkgRevision :: Revision }
    | SecureRepo { pkgRevision :: Revision }
  deriving (Show, Generic)

instance FromJSON Plan where
    parseJSON (Object v) = fmap Plan (v .: "install-plan")
    parseJSON invalid = typeMismatch "Plan" invalid

instance FromJSON InstallItem where
    parseJSON obj@(Object v) = do
        t <- v .: "type"
        case t :: String of
            "pre-existing" -> return APreExisting
            "configured"   -> do
                s <- v .: "style"
                case s :: String of
                  "global"  -> AConfiguredGlobal `fmap` parseJSON obj
                  "inplace" -> AConfiguredInplace `fmap` parseJSON obj
                  "local"   -> AConfiguredInplace `fmap` parseJSON obj
                  _         -> fail $ "unrecognized value of 'style' field: " ++ s
            _              -> fail "unrecognized value of 'type' field"
    parseJSON invalid = typeMismatch "InstallItem" invalid

instance FromJSON ConfiguredInplace where
    parseJSON (Object v) = do
        dist_dir <- v .: "dist-dir"
        build_info <- v .:? "build-info"
        pkg_name <- v .: "pkg-name"
        component_name <- v .:? "component-name"
        pkg_src <- v .: "pkg-src"
        return (ConfiguredInplace dist_dir build_info pkg_name component_name pkg_src)
    parseJSON invalid = typeMismatch "ConfiguredInplace" invalid

instance FromJSON ConfiguredGlobal where
    parseJSON (Object v) = do
        bin_file <- v .:? "bin-file"
        pkg_name <- v .: "pkg-name"
        component_name <- v .:? "component-name"
        pkg_src <- v .: "pkg-src"
        return (ConfiguredGlobal bin_file pkg_name component_name pkg_src)
    parseJSON invalid = typeMismatch "ConfiguredGlobal" invalid

instance FromJSON PackageName where
    parseJSON (String t) = return (mkPackageName (Text.unpack t))
    parseJSON invalid = typeMismatch "PackageName" invalid

instance FromJSON ComponentName where
    parseJSON (String t) =
        case simpleParsec s of
            Nothing -> fail ("could not parse component-name: " ++ s)
            Just r  -> return r
      where s = Text.unpack t
    parseJSON invalid = typeMismatch "ComponentName" invalid

instance FromJSON PkgSrc where
    parseJSON (Object v) = do
        t <- v .: "type"
        case t :: String of
            "repo-tar" -> RepoTar <$> v .: "repo"
            _ -> return PkgSrcOther
    parseJSON invalid = typeMismatch "PkgSrc" invalid

instance FromJSON Repo where
  parseJSON = genericParseJSON defaultOptions
    { constructorTagModifier = camelTo2 '-'
    , fieldLabelModifier = camelTo2 '-'
    , sumEncoding = TaggedObject "type" ""
    }

data DistDirOrBinFile = DistDir FilePath | BinFile FilePath

planDistDir :: Plan -> PackageName -> ComponentName -> DistDirOrBinFile
planDistDir plan pkg_name cname =
    case concatMap p (planInstallPlan plan) of
        [x] -> x
        []  -> error $ "planDistDir: component " ++ prettyShow cname
                    ++ " of package " ++ prettyShow pkg_name ++ " either does not"
                    ++ " exist in the install plan or does not have a dist-dir nor bin-file"
        _   -> error $ "planDistDir: found multiple copies of component " ++ prettyShow cname
                    ++ " of package " ++ prettyShow pkg_name ++ " in install plan"
  where
    p APreExisting      = []
    p (AConfiguredGlobal conf) = do
        guard (configuredGlobalPackageName conf == pkg_name)
        guard $ case configuredGlobalComponentName conf of
                    Nothing     -> True
                    Just cname' -> cname == cname'
        case configuredGlobalBinFile conf of
            Nothing -> []
            Just bin_file -> return $ BinFile bin_file
    p (AConfiguredInplace conf) = do
        guard (configuredInplacePackageName conf == pkg_name)
        guard $ case configuredInplaceComponentName conf of
                    Nothing     -> True
                    Just cname' -> cname == cname'
        return $ DistDir $ configuredInplaceDistDir conf

buildInfoFile :: Plan -> PackageName -> ComponentName -> FilePath
buildInfoFile plan pkg_name cname =
    case concatMap p (planInstallPlan plan) of
        [Just x] -> x
        [Nothing] -> error $ "buildInfoFile: component " ++ prettyShow cname
                    ++ " of package " ++ prettyShow pkg_name ++ " does not"
                    ++ " have a build info-file"
        []  -> error $ "buildInfoFile: component " ++ prettyShow cname
                    ++ " of package " ++ prettyShow pkg_name ++ " either does not"
                    ++ " exist in the install plan or build info-file"
        _   -> error $ "buildInfoFile: found multiple copies of component " ++ prettyShow cname
                    ++ " of package " ++ prettyShow pkg_name ++ " in install plan"
  where
    p APreExisting      = []
    p (AConfiguredGlobal _) = []
    p (AConfiguredInplace conf) = do
        guard (configuredInplacePackageName conf == pkg_name)
        guard $ case configuredInplaceComponentName conf of
                    Nothing     -> True
                    Just cname' -> cname == cname'
        return $ configuredInplaceBuildInfo conf
