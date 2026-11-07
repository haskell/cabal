{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Utilities for understanding @plan.json@.
module Test.Cabal.Plan
  ( Plan (..)
  , DistDirOrBinFile (..)
  , InstallItem (..)
  , ConfiguredInplace (..)
  , ConfiguredGlobal (..)
  , Machine (..)
  , Revision (..)
  , planDistDir
  , buildInfoFile
  ) where

import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import Distribution.Package
import Distribution.Parsec (eitherParsec, simpleParsec)
import Distribution.Pretty (prettyShow)
import Distribution.Types.ComponentName
import Distribution.Types.Version

-- TODO: index this
data Plan = Plan {planInstallPlan :: [InstallItem]}
  deriving (Show)

data InstallItem
  = APreExisting
  | AConfiguredGlobal ConfiguredGlobal
  | AConfiguredInplace ConfiguredInplace
  deriving (Show)

-- local or inplace package
data ConfiguredInplace = ConfiguredInplace
  { configuredInplaceDistDir :: FilePath
  , configuredInplaceBuildInfo :: Maybe FilePath
  , configuredInplacePackageName :: PackageName
  , configuredInplaceVersion :: Version
  , configuredInplaceRevision :: Revision
  , configuredInplaceComponentName :: Maybe ComponentName
  , configuredInplaceMachine :: Machine
  }
  deriving (Show)

data ConfiguredGlobal = ConfiguredGlobal
  { configuredGlobalBinFile :: Maybe FilePath
  , configuredGlobalPackageName :: PackageName
  , configuredGlobalVersion :: Version
  , configuredGlobalRevision :: Revision
  , configuredGlobalComponentName :: Maybe ComponentName
  , configuredGlobalMachine :: Machine
  }
  deriving (Show)

-- | The build machine of a plan entry, as recorded in the @machine@ field of
-- plan.json. Under cross-compilation the same package can appear at both
-- machines; in an ordinary build everything is on the host machine.
data Machine = Build | Host
  deriving (Eq, Show)

instance FromJSON Machine where
  parseJSON = withText "Machine" $ \t -> case Text.unpack t of
    "build" -> return Build
    "host" -> return Host
    s -> fail ("unrecognized value of 'machine' field: " ++ s)

-- | Older plan.json files have no @machine@ field; everything was host then.
parseMachine :: Object -> Parser Machine
parseMachine v = fromMaybe Host <$> v .:? "machine"

newtype Revision = Revision Int
  deriving (Show, Eq, FromJSON)

instance FromJSON Plan where
  parseJSON (Object v) = fmap Plan (v .: "install-plan")
  parseJSON invalid = typeMismatch "Plan" invalid

instance FromJSON InstallItem where
  parseJSON obj@(Object v) = do
    t <- v .: "type"
    case t :: String of
      "pre-existing" -> return APreExisting
      "configured" -> do
        s <- v .: "style"
        case s :: String of
          "global" -> AConfiguredGlobal `fmap` parseJSON obj
          "inplace" -> AConfiguredInplace `fmap` parseJSON obj
          "local" -> AConfiguredInplace `fmap` parseJSON obj
          _ -> fail $ "unrecognized value of 'style' field: " ++ s
      _ -> fail "unrecognized value of 'type' field"
  parseJSON invalid = typeMismatch "InstallItem" invalid

instance FromJSON ConfiguredInplace where
  parseJSON (Object v) = do
    dist_dir <- v .: "dist-dir"
    build_info <- v .:? "build-info"
    pkg_name <- v .: "pkg-name"
    pkg_version <- v .: "pkg-version"
    pkg_revision <- v .: "pkg-revision"
    component_name <- v .:? "component-name"
    machine <- parseMachine v
    return (ConfiguredInplace dist_dir build_info pkg_name pkg_version pkg_revision component_name machine)
  parseJSON invalid = typeMismatch "ConfiguredInplace" invalid

instance FromJSON ConfiguredGlobal where
  parseJSON (Object v) = do
    bin_file <- v .:? "bin-file"
    pkg_name <- v .: "pkg-name"
    pkg_version <- v .: "pkg-version"
    pkg_revision <- v .: "pkg-revision"
    component_name <- v .:? "component-name"
    machine <- parseMachine v
    return (ConfiguredGlobal bin_file pkg_name pkg_version pkg_revision component_name machine)
  parseJSON invalid = typeMismatch "ConfiguredGlobal" invalid

instance FromJSON PackageName where
  parseJSON (String t) = return (mkPackageName (Text.unpack t))
  parseJSON invalid = typeMismatch "PackageName" invalid

instance FromJSON Version where
  parseJSON = withText "Version" $ either fail pure . eitherParsec . Text.unpack

instance FromJSON ComponentName where
  parseJSON (String t) =
    case simpleParsec s of
      Nothing -> fail ("could not parse component-name: " ++ s)
      Just r -> return r
    where
      s = Text.unpack t
  parseJSON invalid = typeMismatch "ComponentName" invalid

data DistDirOrBinFile = DistDir FilePath | BinFile FilePath

planDistDir :: Plan -> PackageName -> ComponentName -> DistDirOrBinFile
planDistDir plan pkg_name cname =
  -- Under cross-compilation a component can be in the plan on both machines;
  -- the host-machine copy is the one a test normally means (it is what the
  -- user targeted), so prefer it when that resolves the ambiguity.
  case preferHost (concatMap p (planInstallPlan plan)) of
    [x] -> x
    [] ->
      error $
        "planDistDir: component "
          ++ prettyShow cname
          ++ " of package "
          ++ prettyShow pkg_name
          ++ " either does not"
          ++ " exist in the install plan or does not have a dist-dir nor bin-file"
    _ ->
      error $
        "planDistDir: found multiple copies of component "
          ++ prettyShow cname
          ++ " of package "
          ++ prettyShow pkg_name
          ++ " in install plan"
  where
    preferHost xs = case [x | (Host, x) <- xs] of
      [x] -> [x]
      _ -> map snd xs
    p APreExisting = []
    p (AConfiguredGlobal conf) = do
      guard (configuredGlobalPackageName conf == pkg_name)
      guard $ case configuredGlobalComponentName conf of
        Nothing -> True
        Just cname' -> cname == cname'
      case configuredGlobalBinFile conf of
        Nothing -> []
        Just bin_file -> return (configuredGlobalMachine conf, BinFile bin_file)
    p (AConfiguredInplace conf) = do
      guard (configuredInplacePackageName conf == pkg_name)
      guard $ case configuredInplaceComponentName conf of
        Nothing -> True
        Just cname' -> cname == cname'
      return (configuredInplaceMachine conf, DistDir (configuredInplaceDistDir conf))

buildInfoFile :: Plan -> PackageName -> ComponentName -> FilePath
buildInfoFile plan pkg_name cname =
  case concatMap p (planInstallPlan plan) of
    [Just x] -> x
    [Nothing] ->
      error $
        "buildInfoFile: component "
          ++ prettyShow cname
          ++ " of package "
          ++ prettyShow pkg_name
          ++ " does not"
          ++ " have a build info-file"
    [] ->
      error $
        "buildInfoFile: component "
          ++ prettyShow cname
          ++ " of package "
          ++ prettyShow pkg_name
          ++ " either does not"
          ++ " exist in the install plan or build info-file"
    _ ->
      error $
        "buildInfoFile: found multiple copies of component "
          ++ prettyShow cname
          ++ " of package "
          ++ prettyShow pkg_name
          ++ " in install plan"
  where
    p APreExisting = []
    p (AConfiguredGlobal _) = []
    p (AConfiguredInplace conf) = do
      guard (configuredInplacePackageName conf == pkg_name)
      guard $ case configuredInplaceComponentName conf of
        Nothing -> True
        Just cname' -> cname == cname'
      return $ configuredInplaceBuildInfo conf
