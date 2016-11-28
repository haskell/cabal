{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Utilities for understanding @plan.json@.
module Test.Cabal.Plan (
    Plan,
    planDistDir,
) where

import Distribution.Text
import Distribution.Types.ComponentName
import Distribution.Package
import qualified Data.Text as Text
import Data.Aeson
import Data.Aeson.Types
import Control.Monad

-- TODO: index this
data Plan = Plan { planInstallPlan :: [InstallItem] }

data InstallItem
    = APreExisting
    | AConfigured Configured

data Configured = Configured
    { configuredDistDir       :: FilePath
    , configuredPackageName   :: PackageName
    , configuredComponentName :: Maybe ComponentName }

instance FromJSON Plan where
    parseJSON (Object v) = fmap Plan (v .: "install-plan")
    parseJSON invalid = typeMismatch "Plan" invalid

instance FromJSON InstallItem where
    parseJSON obj@(Object v) = do
        t <- v .: "type"
        case t :: String of
            "pre-existing" -> return APreExisting
            "configured"   -> AConfigured `fmap` parseJSON obj
            _              -> fail "unrecognized value of 'type' field"
    parseJSON invalid = typeMismatch "InstallItem" invalid

instance FromJSON Configured where
    parseJSON (Object v) = do
        dist_dir <- v .: "dist-dir"
        pkg_name <- v .: "pkg-name"
        component_name <- v .:? "component-name"
        return (Configured dist_dir pkg_name component_name)
    parseJSON invalid = typeMismatch "Configured" invalid

instance FromJSON PackageName where
    parseJSON (String t) = return (mkPackageName (Text.unpack t))
    parseJSON invalid = typeMismatch "PackageName" invalid

instance FromJSON ComponentName where
    parseJSON (String t) =
        case simpleParse s of
            Nothing -> fail ("could not parse component-name: " ++ s)
            Just r  -> return r
      where s = Text.unpack t
    parseJSON invalid = typeMismatch "ComponentName" invalid

planDistDir :: Plan -> PackageName -> ComponentName -> FilePath
planDistDir plan pkg_name cname =
    case concatMap p (planInstallPlan plan) of
        [x] -> x
        []  -> error $ "planDistDir: could not find component " ++ display cname
                    ++ " of package " ++ display pkg_name ++ " in install plan"
        _   -> error $ "planDistDir: found multiple copies of component " ++ display cname
                    ++ " of package " ++ display pkg_name ++ " in install plan"
  where
    p APreExisting = []
    p (AConfigured conf) = do
        guard (configuredPackageName conf == pkg_name)
        guard $ case configuredComponentName conf of
                    Nothing     -> True
                    Just cname' -> cname == cname'
        return (configuredDistDir conf)
