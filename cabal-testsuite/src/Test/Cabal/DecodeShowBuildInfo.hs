{-# LANGUAGE DeriveGeneric #-}
module Test.Cabal.DecodeShowBuildInfo where

import           Test.Cabal.Prelude
import           Distribution.Text (display)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Aeson
import           GHC.Stack.Types
import           GHC.Generics

-- | Run 'show-build-info' silencing all output using '-v0'.
-- This is necessary to make sure no stray output from 'show-build-info' makes
-- parsing impossible.
runShowBuildInfo :: [String] -> TestM BuildInfo
runShowBuildInfo args = do
  r <- cabal' "show-build-info" ("-v0":args)
  decodeShowBuildInfo (resultOutput r)

-- | Same as 'runShowBuildInfo' but does not require the verbosity '-v0'.
-- Uses "-vverbose +markoutput +nowrap" to extract the relevant json output.
runShowBuildInfoWithMarker :: [String] -> TestM BuildInfo
runShowBuildInfoWithMarker args = do
  r <- cabal' "show-build-info" args
  decodeShowBuildInfo (last . lines . getMarkedOutput $ resultOutput r)

decodeShowBuildInfo :: String -> TestM BuildInfo
decodeShowBuildInfo s = case eitherDecodeStrict (T.encodeUtf8 $ T.pack s) of
    Left err -> fail $ "Could not parse show-build-info command: " ++ err
    Right buildInfos -> return buildInfos

decodeBuildInfoFile :: FilePath -> TestM BuildInfo
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
  , componentSrcFiles :: [FilePath]
  , componentHsSrcDirs :: [FilePath]
  , componentSrcDir :: FilePath
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

-- -----------------------------------------------------------
-- Assertion Helpers to define succinct test cases
-- -----------------------------------------------------------

assertCommonBuildInfo :: (HasCallStack, MonadIO m) => BuildInfo -> m ()
assertCommonBuildInfo buildInfo = do
  assertEqual "Cabal Version" (display cabalVersionLibrary) (cabalVersion buildInfo)
  assertEqual "Compiler flavour" "ghc" (flavour $ compiler buildInfo)
  assertBool "Compiler id" (and $ zipWith (==) "ghc" (compilerId $ compiler buildInfo))
  assertBool "Compiler path non-empty" (not . null . path $ compiler buildInfo)

assertExeComponent :: (HasCallStack, MonadIO m) => ComponentInfo -> String -> [String] -> [String] -> m ()
assertExeComponent = assertExecutableComp "exe"

assertExeComponent' :: (HasCallStack, MonadIO m) => ComponentInfo -> String -> [String] -> [String] -> [String] -> m ()
assertExeComponent' component compName modules sourceFiles sourceDirs =
  assertArbitraryComp "exe" compName (not . null) (not . null) modules sourceFiles sourceDirs component

assertLibComponent :: (HasCallStack, MonadIO m) => ComponentInfo -> String -> [String] -> [String] -> m ()
assertLibComponent component compName modules sourceDirs =
  assertArbitraryComp "lib" compName (not . null) (not . null) modules [] sourceDirs component

assertTestComponent :: (HasCallStack, MonadIO m) => ComponentInfo -> String -> [String] -> [String] -> m ()
assertTestComponent = assertExecutableComp "test"

assertBenchComponent :: (HasCallStack, MonadIO m) => ComponentInfo -> String -> [String] -> [String] -> m ()
assertBenchComponent = assertExecutableComp "bench"

assertBenchComponent' :: (HasCallStack, MonadIO m) => ComponentInfo -> String -> [String] -> [String] -> [String] -> m ()
assertBenchComponent' component compName modules sourceFiles sourceDirs =
  assertArbitraryComp "bench" compName (not . null) (not . null) modules sourceFiles sourceDirs component

assertExecutableComp :: (HasCallStack, MonadIO m) => String -> ComponentInfo -> String -> [String] -> [String] -> m ()
assertExecutableComp compType component compName sourceFiles sourceDirs  =
  assertArbitraryComp compType compName (not . null) (not . null) [] sourceFiles sourceDirs component

assertArbitraryComp :: (HasCallStack, MonadIO m) => String -> String ->
  (String -> Bool) -> ([String] -> Bool) -> [String] -> [FilePath] ->
  [FilePath] -> ComponentInfo -> m ()
assertArbitraryComp compType compName unitIdPred compilerArgsPred modules sourceFiles sourceDirs component = do
  assertEqual "Component type" compType (componentType component)
  assertEqual "Component name" compName (componentName component)
  assertBool  "Component Unit Id" (unitIdPred $ componentUnitId component)
  assertBool  "Component compiler args" (compilerArgsPred  $ componentCompilerArgs component)
  assertEqual "Component modules" modules (componentModules component)
  assertEqual "Component source files" sourceFiles (componentSrcFiles component)
  assertEqual "Component source directories" sourceDirs (componentHsSrcDirs component)
