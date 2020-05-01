{-# LANGUAGE DeriveGeneric #-}
import           Test.Cabal.Prelude

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Aeson
import           GHC.Generics

main = cabalTest $ do
    r <- cabal' "show-build-info" ["exe:A", "-v0"]
    let buildInfoEither = eitherDecodeStrict (T.encodeUtf8 . T.pack $ resultOutput r) :: Either String [BuildInfo]
    case buildInfoEither of
      Left err -> fail $ "Could not parse build-info command" ++ err
      Right buildInfos -> do
        assertEqual "Build Infos, exactly one" 1  (length buildInfos)
        let [buildInfo] = buildInfos
        assertEqual "Cabal Version" cabalVersionString (cabalVersion buildInfo)
        assertEqual "Compiler flavour" "ghc" (flavour $ compiler buildInfo)
        assertBool "Compiler id" (and $ zipWith (==) "ghc" (compilerId $ compiler buildInfo))
        assertBool "Compiler path non-empty" (not . null . path $ compiler buildInfo)
        assertEqual "Components, exactly one" 1 (length $ components buildInfo)
        let [component] = components buildInfo
        assertEqual "Component type" "exe" (componentType component)
        assertEqual "Component name" "exe:A" (componentName component)
        assertEqual "Component unit-id" "A-0.1.0.0-inplace-A" (componentUnitId component)
        assertBool "Component compiler args are non-empty" (not . null $ componentCompilerArgs component)
        assertEqual "Component modules" [] (componentModules component)
        assertEqual "Component source files" ["Main.hs"] (componentSrcFiles component)
        assertEqual "Component source directories" ["src"] (componentSrcDirs component)
    return ()

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