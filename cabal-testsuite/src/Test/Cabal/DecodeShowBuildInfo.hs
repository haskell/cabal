{-# LANGUAGE DeriveGeneric #-}
module Test.Cabal.DecodeShowBuildInfo where

import           Test.Cabal.Prelude
import qualified Distribution.Simple.Utils as U (cabalVersion)
import           Distribution.Text (display)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Aeson
import           GHC.Generics

runShowBuildInfo :: [String] -> TestM [BuildInfo]
runShowBuildInfo args = do
    r <- cabal' "show-build-info" args
    case eitherDecodeStrict (T.encodeUtf8 . T.pack $ resultOutput r) of
        Left err -> fail $ "Could not parse show-build-info command: " ++ err
        Right buildInfos -> return buildInfos

decodeBuildInfoFile :: FilePath -> TestM [BuildInfo]
decodeBuildInfoFile fp = do
    shouldExist fp
    res <- liftIO $ eitherDecodeFileStrict fp
    case res of
        Left err -> fail $ "Could not parse show-build-info file: " ++ err
        Right buildInfos -> return buildInfos

data BuildInfo = BuildInfo
    { cabalVersion :: String
    , compiler :: CompilerInfo
    , components :: [ComponentInfo]
    } deriving (Generic, Show)

data CompilerInfo = CompilerInfo
    { flavour :: String
    , compilerId :: String
    , path :: String
    } deriving (Generic, Show)

data ComponentInfo = ComponentInfo
    { componentType :: String
    , componentName :: String
    , componentUnitId :: String
    , componentCompilerArgs :: [String]
    , componentModules :: [String]
    , componentSrcFiles :: [String]
    , componentSrcDirs :: [String]
    } deriving (Generic, Show)

instance ToJSON BuildInfo where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON BuildInfo where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '-' }

instance ToJSON CompilerInfo where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON CompilerInfo where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '-' }

instance ToJSON ComponentInfo where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON ComponentInfo where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 10 . camelTo2 '-' }

cabalVersionLibrary :: String
cabalVersionLibrary = display U.cabalVersion
