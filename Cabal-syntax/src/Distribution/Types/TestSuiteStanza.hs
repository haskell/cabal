{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Distribution.Types.TestSuiteStanza where

import Distribution.Compat.Lens
import Distribution.Compat.Prelude
import Prelude ()

import Distribution.CabalSpecVersion
import Distribution.Fields.ParseResult
import Distribution.ModuleName (ModuleName)
import Distribution.Parsec
import Distribution.Pretty (prettyShow)
import Distribution.Types.BuildInfo
import qualified Distribution.Types.BuildInfo.Lens as L
import Distribution.Types.TestSuite
import Distribution.Types.TestSuiteInterface
import Distribution.Types.TestType
import Distribution.Utils.Path

-- | An intermediate type just used for parsing the test-suite stanza.
-- After validation it is converted into the proper 'TestSuite' type.
data TestSuiteStanza = TestSuiteStanza
  { _testStanzaTestType :: Maybe TestType
  , _testStanzaMainIs :: Maybe (RelativePath Source File)
  , _testStanzaTestModule :: Maybe ModuleName
  , _testStanzaBuildInfo :: BuildInfo
  , _testStanzaCodeGenerators :: [String]
  }
  deriving (Show, Eq, Data, Generic)

instance Binary TestSuiteStanza
instance Structured TestSuiteStanza
instance NFData TestSuiteStanza where rnf = genericRnf

instance L.HasBuildInfo TestSuiteStanza where
  buildInfo = testStanzaBuildInfo

validateTestSuite :: Position -> TestSuiteStanza -> ParseResult src ()
validateTestSuite pos stanza = case _testStanzaTestType stanza of
  Nothing -> pure ()
  Just (TestTypeUnknown _ _) -> pure ()
  Just tt | tt `notElem` knownTestTypes -> pure ()
  Just tt@(TestTypeExe _ver) -> case _testStanzaMainIs stanza of
    Nothing -> parseFailure pos (missingField "main-is" tt)
    Just _file ->
      when (isJust (_testStanzaTestModule stanza)) $
        parseWarning pos PWTExtraBenchmarkModule (extraField "test-module" tt)
  Just tt@(TestTypeLib _ver) -> case _testStanzaTestModule stanza of
    Nothing ->
      parseFailure pos (missingField "test-module" tt)
    Just _module ->
      when (isJust (_testStanzaMainIs stanza)) $
        parseWarning pos PWTExtraMainIs (extraField "main-is" tt)
  where
    missingField name tt =
      "The '"
        ++ name
        ++ "' field is required for the "
        ++ prettyShow tt
        ++ " test suite type."

    extraField name tt =
      "The '"
        ++ name
        ++ "' field is not used for the '"
        ++ prettyShow tt
        ++ "' test suite type."

-- | Convert a previously validated 'TestSuiteStanza' to 'GenericPackageDescription''s 'TestSuite' type
-- We do not check the validity here
convertTestSuite :: TestSuiteStanza -> TestSuite
convertTestSuite stanza = case _testStanzaTestType stanza of
  Nothing -> basicTestSuite
  Just tt@(TestTypeUnknown _ _) ->
    basicTestSuite
      { testInterface = TestSuiteUnsupported tt
      }
  Just tt
    | tt `notElem` knownTestTypes ->
        basicTestSuite
          { testInterface = TestSuiteUnsupported tt
          }
  Just (TestTypeExe ver) -> case _testStanzaMainIs stanza of
    Nothing -> failedToConvert
    Just file ->
      basicTestSuite
        { testInterface = TestSuiteExeV10 ver file
        }
  Just (TestTypeLib ver) -> case _testStanzaTestModule stanza of
    Nothing -> failedToConvert
    Just module_ ->
      basicTestSuite
        { testInterface = TestSuiteLibV09 ver module_
        }
  where
    failedToConvert =
      error $
        "Unexpected: the conversion from TestSuiteStanza to TestSuite failed\n"
          <> "Did you mess with `GenericPackageDescription`?"

    basicTestSuite =
      emptyTestSuite
        { testBuildInfo = _testStanzaBuildInfo stanza
        , testCodeGenerators = _testStanzaCodeGenerators stanza
        }

unvalidateTestSuite :: TestSuite -> TestSuiteStanza
unvalidateTestSuite t =
  TestSuiteStanza
    { _testStanzaTestType = ty
    , _testStanzaMainIs = ma
    , _testStanzaTestModule = mo
    , _testStanzaBuildInfo = testBuildInfo t
    , _testStanzaCodeGenerators = testCodeGenerators t
    }
  where
    (ty, ma, mo) = case testInterface t of
      TestSuiteExeV10 ver file -> (Just $ TestTypeExe ver, Just file, Nothing)
      TestSuiteLibV09 ver modu -> (Just $ TestTypeLib ver, Nothing, Just modu)
      _ -> (Nothing, Nothing, Nothing)

-- | We try to guess the TestSuiteType if it's not specified
patchTestSuiteType :: CabalSpecVersion -> TestSuiteStanza -> TestSuiteStanza
patchTestSuiteType cabalSpecVersion stanza =
  stanza
    { _testStanzaTestType =
        _testStanzaTestType stanza
          <|> do
            guard (cabalSpecVersion >= CabalSpecV3_8)
            testTypeExe <$ _testStanzaMainIs stanza
          <|> testTypeLib <$ _testStanzaTestModule stanza
    }

testStanzaTestType :: Lens' TestSuiteStanza (Maybe TestType)
testStanzaTestType f s = fmap (\x -> s{_testStanzaTestType = x}) (f (_testStanzaTestType s))
{-# INLINE testStanzaTestType #-}

testStanzaMainIs :: Lens' TestSuiteStanza (Maybe (RelativePath Source File))
testStanzaMainIs f s = fmap (\x -> s{_testStanzaMainIs = x}) (f (_testStanzaMainIs s))
{-# INLINE testStanzaMainIs #-}

testStanzaTestModule :: Lens' TestSuiteStanza (Maybe ModuleName)
testStanzaTestModule f s = fmap (\x -> s{_testStanzaTestModule = x}) (f (_testStanzaTestModule s))
{-# INLINE testStanzaTestModule #-}

testStanzaBuildInfo :: Lens' TestSuiteStanza BuildInfo
testStanzaBuildInfo f s = fmap (\x -> s{_testStanzaBuildInfo = x}) (f (_testStanzaBuildInfo s))
{-# INLINE testStanzaBuildInfo #-}

testStanzaCodeGenerators :: Lens' TestSuiteStanza [String]
testStanzaCodeGenerators f s = fmap (\x -> s{_testStanzaCodeGenerators = x}) (f (_testStanzaCodeGenerators s))
{-# INLINE testStanzaCodeGenerators #-}
