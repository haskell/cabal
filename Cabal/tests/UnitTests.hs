{-# LANGUAGE DeriveDataTypeable #-}
module Main
    ( main
    ) where

import Test.Tasty
import Test.Tasty.Options

import Data.Proxy
import Data.Typeable

import Distribution.Simple.Utils
import Distribution.Verbosity
import Distribution.Compat.Time

import qualified UnitTests.Distribution.Compat.CreatePipe
import qualified UnitTests.Distribution.Compat.Time
import qualified UnitTests.Distribution.Compat.Graph
import qualified UnitTests.Distribution.Simple.Glob
import qualified UnitTests.Distribution.Simple.Program.GHC
import qualified UnitTests.Distribution.Simple.Program.Internal
import qualified UnitTests.Distribution.Simple.Utils
import qualified UnitTests.Distribution.System
import qualified UnitTests.Distribution.Utils.CharSet
import qualified UnitTests.Distribution.Utils.Generic
import qualified UnitTests.Distribution.Utils.NubList
import qualified UnitTests.Distribution.Utils.ShortText
import qualified UnitTests.Distribution.Utils.Structured
import qualified UnitTests.Distribution.Version (versionTests)
import qualified UnitTests.Distribution.PkgconfigVersion (pkgconfigVersionTests)
import qualified UnitTests.Distribution.SPDX (spdxTests)
import qualified UnitTests.Distribution.Described
import qualified UnitTests.Distribution.Types.GenericPackageDescription

tests :: Int -> TestTree
tests mtimeChangeCalibrated =
  askOption $ \(OptionMtimeChangeDelay mtimeChangeProvided) ->
  askOption $ \(GhcPath ghcPath) ->
  let mtimeChange = if mtimeChangeProvided /= 0
                    then mtimeChangeProvided
                    else mtimeChangeCalibrated
  in
  testGroup "Unit Tests"
    [ testGroup "Distribution.Compat.CreatePipe"
        UnitTests.Distribution.Compat.CreatePipe.tests
    , testGroup "Distribution.Compat.Time"
        (UnitTests.Distribution.Compat.Time.tests mtimeChange)
    , testGroup "Distribution.Compat.Graph"
        UnitTests.Distribution.Compat.Graph.tests
    , testGroup "Distribution.Simple.Glob"
        UnitTests.Distribution.Simple.Glob.tests
    , UnitTests.Distribution.Simple.Program.GHC.tests
    , testGroup "Distribution.Simple.Program.Internal"
        UnitTests.Distribution.Simple.Program.Internal.tests
    , testGroup "Distribution.Simple.Utils" $
        UnitTests.Distribution.Simple.Utils.tests ghcPath
    , testGroup "Distribution.Utils.Generic"
        UnitTests.Distribution.Utils.Generic.tests
    , testGroup "Distribution.Utils.NubList"
        UnitTests.Distribution.Utils.NubList.tests
    , testGroup "Distribution.Utils.ShortText"
        UnitTests.Distribution.Utils.ShortText.tests
    , testGroup "Distribution.System"
        UnitTests.Distribution.System.tests
    , testGroup "Distribution.Types.GenericPackageDescription"
        UnitTests.Distribution.Types.GenericPackageDescription.tests
    , testGroup "Distribution.Version"
        UnitTests.Distribution.Version.versionTests
    , testGroup "Distribution.Types.PkgconfigVersion(Range)"
        UnitTests.Distribution.PkgconfigVersion.pkgconfigVersionTests
    , testGroup "Distribution.SPDX"
        UnitTests.Distribution.SPDX.spdxTests
    , UnitTests.Distribution.Utils.CharSet.tests
    , UnitTests.Distribution.Utils.Structured.tests
    , UnitTests.Distribution.Described.tests
    ]

extraOptions :: [OptionDescription]
extraOptions =
  [ Option (Proxy :: Proxy OptionMtimeChangeDelay)
  , Option (Proxy :: Proxy GhcPath)
  ]

newtype OptionMtimeChangeDelay = OptionMtimeChangeDelay Int
  deriving Typeable

instance IsOption OptionMtimeChangeDelay where
  defaultValue   = OptionMtimeChangeDelay 0
  parseValue     = fmap OptionMtimeChangeDelay . safeRead
  optionName     = return "mtime-change-delay"
  optionHelp     = return $ "How long to wait before attempting to detect"
                   ++ "file modification, in microseconds"

newtype GhcPath = GhcPath FilePath
  deriving Typeable

instance IsOption GhcPath where
  defaultValue = GhcPath "ghc"
  optionName   = return "with-ghc"
  optionHelp   = return "The ghc compiler to use"
  parseValue   = Just . GhcPath

main :: IO ()
main = do
  (mtimeChange, mtimeChange') <- calibrateMtimeChangeDelay
  let toMillis :: Int -> Double
      toMillis x = fromIntegral x / 1000.0
  notice normal $ "File modification time resolution calibration completed, "
    ++ "maximum delay observed: "
    ++ (show . toMillis $ mtimeChange ) ++ " ms. "
    ++ "Will be using delay of " ++ (show . toMillis $ mtimeChange')
    ++ " for test runs."
  defaultMainWithIngredients
         (includingOptions extraOptions : defaultIngredients)
         (tests mtimeChange')
