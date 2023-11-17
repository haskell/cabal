{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module UnitTests.Distribution.Client.ScriptUtils (tests) where

import Distribution.Client.ScriptUtils
import Distribution.Client.Config
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck.Instances.Cabal
import System.FilePath

tests :: [TestTree]
tests =
  [ testGroup
      "ScriptUtils"
      [ testProperty "valid_script_path" testScriptPath
      ]
  ]

-- ------------------------------------------------------------

-- * Unit tests

-- ------------------------------------------------------------

testScriptPath :: ShortPath -> Property
testScriptPath (ShortPath p) = withMaxSuccess 10000 $ ioProperty $ do
  hashed_path <- getScriptCacheDirectory p
  script_build_dir <- defaultScriptBuildsDir
  return $ and
    -- 1. Is it a valid path at all
    [ isValid hashed_path
    -- 2. Is the computed hashed path in the expected directory?
    , (script_build_dir </> takeFileName hashed_path) `equalFilePath` hashed_path
    ]

