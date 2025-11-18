{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Cabal.DecodeShowBuildInfo where

import Control.Monad.Trans.Reader
import Data.Aeson
import Distribution.Compat.Stack
import Distribution.Package
import Distribution.Pretty (prettyShow)
import Distribution.Text (display)
import Distribution.Types.ComponentName
import Distribution.Types.LibraryName
import Distribution.Types.UnqualComponentName
import GHC.Generics
import System.Exit
import Test.Cabal.Plan
import Test.Cabal.Prelude

-- | Execute 'cabal build --enable-build-info'.
--
-- Results can be read via 'withPlan', 'buildInfoFile' and 'decodeBuildInfoFile'.
runShowBuildInfo :: [String] -> TestM ()
runShowBuildInfo args = noCabalPackageDb $ cabal "build" ("--enable-build-info":args)

-- | Read 'build-info.json' for a given package and component
-- from disk and record the content. Helpful for defining test-cases
-- where the build info matters.
recordBuildInfo :: PackageName -> ComponentName -> TestM ()
recordBuildInfo pkgName cname = do
  Just plan <- fmap testPlan ask
  let fp = buildInfoFile plan pkgName cname
  recordMode RecordAll $ do
    recordHeader ["show-build-info", prettyShow pkgName, prettyShow cname]
    buildInfo <- liftIO $ readFile fp
    recordLog $ Result ExitSuccess "build --enable-build-info" buildInfo

-- | Decode the given filepath into a 'BuildInfo'.
--
-- If the filepath doesn't exist or its contents are not a valid 'BuildInfo'
-- json file, then an error is raised.
decodeBuildInfoFile :: FilePath -> TestM BuildInfo
decodeBuildInfoFile fp = do
  shouldExist fp
  res <- liftIO $ eitherDecodeFileStrict fp
  case res of
    Left err -> fail $ "Could not parse show-build-info file: " ++ err
    Right buildInfos -> return buildInfos

data BuildInfo = BuildInfo
  { cabalLibVersion :: String
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

data ComponentAssertion = ComponentAssertion
  { unitIdPred :: (String -> Bool)
  -- ^ Predicate to apply to a component's unit id.
  , compilerArgsPred :: ([String] -> Bool)
  -- ^ Predicate the compilation arguments must satisfy.
  , modules :: [String]
  -- ^ Which modules should a component contain.
  , sourceFiles :: [FilePath]
  -- ^ Which source files are part of a component.
  , sourceDirs :: [FilePath]
  -- ^ Expected source directories for a component.
  , compType :: String
  -- ^ Type of the component, usually one of 'bench', 'exe', 'test', 'lib', 'flib'
  }

defCompAssertion :: ComponentAssertion
defCompAssertion = ComponentAssertion
  { unitIdPred = not . null
  , compilerArgsPred = not . null
  , modules = []
  , sourceFiles = []
  , sourceDirs = []
  , compType = ""
  }

-- | Assert common build information, such as compiler location, compiler version
-- and cabal library version.
assertCommonBuildInfo :: WithCallStack (BuildInfo -> TestM ())
assertCommonBuildInfo buildInfo = do
  assertEqual "Cabal Version" (display cabalVersionLibrary) (cabalLibVersion buildInfo)
  assertEqual "Compiler flavour" "ghc" (flavour $ compiler buildInfo)
  assertBool "Compiler id" (and $ zipWith (==) "ghc" (compilerId $ compiler buildInfo))
  assertBool "Compiler path non-empty" (not . null . path $ compiler buildInfo)

-- | Pure assertion helper. Check whether the given 'ComponentInfo' satisfy
-- the 'ComponentAssertion'.
assertComponentPure :: WithCallStack (ComponentInfo -> ComponentAssertion -> TestM ())
assertComponentPure component ComponentAssertion{..} = do
  assertEqual "Component type" compType (componentType component)
  assertBool  "Component Unit Id" (unitIdPred $ componentUnitId component)
  assertBool  "Component compiler args" (compilerArgsPred $ componentCompilerArgs component)
  assertEqual "Component modules" modules (componentModules component)
  assertEqual "Component source files" sourceFiles (componentSrcFiles component)
  assertEqual "Component source directories" sourceDirs (componentHsSrcDirs component)

-- | @'assertComponent' pkgName cname assertion@
--
-- Assert that a component identified by 'pkgName' and 'cname', generated
-- a 'build-info.json' and its contents satisfy the assertions specified in 'assertion'.
--
-- This assertion must be wrapped in 'withPlan'.
assertComponent :: WithCallStack (PackageName -> ComponentName -> ComponentAssertion -> TestM ())
assertComponent pkgName cname assert = do
  Just plan <- fmap testPlan ask
  let fp = buildInfoFile plan pkgName cname
  buildInfo <- decodeBuildInfoFile fp
  assertCommonBuildInfo buildInfo

  let component = findComponentInfo buildInfo
  let assertWithCompType = assert { compType = compTypeStr cname }
  assertComponentPure component assertWithCompType
  where
    compTypeStr :: ComponentName -> String
    compTypeStr (CLibName _)   = "lib"
    compTypeStr (CFLibName _) = "flib"
    compTypeStr (CExeName _) = "exe"
    compTypeStr (CTestName _) = "test"
    compTypeStr (CBenchName _) = "bench"

    findComponentInfo :: BuildInfo -> ComponentInfo
    findComponentInfo buildInfo =
      case filter (\c -> prettyShow cname == componentName c) (components buildInfo) of
        [x] -> x
        [] ->  error $ "findComponentInfo: component " ++ prettyShow cname ++ " does not"
                    ++ " exist in build info-file"
        _   -> error $ "findComponentInfo: found multiple copies of component " ++ prettyShow cname
                    ++ " in build info plan"

-- | Helper function to create an executable component name.
exe :: String -> ComponentName
exe = CExeName . mkUnqualComponentName

-- | Helper function to create a named sub-library component name.
lib :: String -> ComponentName
lib = CLibName . LSubLibName . mkUnqualComponentName

-- | Helper function to create an test component name.
test :: String -> ComponentName
test = CTestName . mkUnqualComponentName

-- | Helper function to create an benchmark component name.
bench :: String -> ComponentName
bench = CBenchName . mkUnqualComponentName

-- | Helper function to create a main library component name.
mainLib :: ComponentName
mainLib = CLibName LMainLibName
