module Main
    ( main
    ) where

import Test.Tasty
import Test.Tasty.Options

import Data.Proxy

import qualified UnitTests.Distribution.Compat.Time
import qualified UnitTests.Distribution.Compat.Graph
import qualified UnitTests.Distribution.PackageDescription.Check
import qualified UnitTests.Distribution.Simple.Command
import qualified UnitTests.Distribution.Simple.Glob
import qualified UnitTests.Distribution.Simple.Program.GHC
import qualified UnitTests.Distribution.Simple.Program.Internal
import qualified UnitTests.Distribution.Simple.Utils
import qualified UnitTests.Distribution.System
import qualified UnitTests.Distribution.Utils.CharSet
import qualified UnitTests.Distribution.Utils.Generic
import qualified UnitTests.Distribution.Utils.Json
import qualified UnitTests.Distribution.Utils.NubList
import qualified UnitTests.Distribution.Utils.ShortText
import qualified UnitTests.Distribution.Utils.Structured
import qualified UnitTests.Distribution.Version (versionTests)
import qualified UnitTests.Distribution.PkgconfigVersion (pkgconfigVersionTests)
import qualified UnitTests.Distribution.SPDX (spdxTests)
import qualified UnitTests.Distribution.Described
import qualified UnitTests.Distribution.CabalSpecVersion
import qualified UnitTests.Distribution.Types.GenericPackageDescription

tests :: TestTree
tests =
  askOption $ \(OptionMtimeChangeDelay mtimeChange) ->
  askOption $ \(GhcPath ghcPath) ->
  testGroup "Unit Tests"
    [ testGroup "Distribution.Compat.Time"
        (UnitTests.Distribution.Compat.Time.tests mtimeChange)
    , testGroup "Distribution.Compat.Graph"
        UnitTests.Distribution.Compat.Graph.tests
    , testGroup "Distribution.Simple.Command"
        UnitTests.Distribution.Simple.Command.tests
    , testGroup "Distribution.Simple.Glob"
        UnitTests.Distribution.Simple.Glob.tests
    , UnitTests.Distribution.Simple.Program.GHC.tests
    , testGroup "Distribution.Simple.Program.Internal"
        UnitTests.Distribution.Simple.Program.Internal.tests
    , testGroup "Distribution.Simple.Utils" $
        UnitTests.Distribution.Simple.Utils.tests ghcPath
    , testGroup "Distribution.Utils.Generic"
        UnitTests.Distribution.Utils.Generic.tests
    , testGroup "Distribution.Utils.Json" $
        UnitTests.Distribution.Utils.Json.tests
    , testGroup "Distribution.Utils.NubList"
        UnitTests.Distribution.Utils.NubList.tests
    , testGroup "Distribution.PackageDescription.Check"
        UnitTests.Distribution.PackageDescription.Check.tests
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
    , UnitTests.Distribution.CabalSpecVersion.tests
    ]

extraOptions :: [OptionDescription]
extraOptions =
  [ Option (Proxy :: Proxy OptionMtimeChangeDelay)
  , Option (Proxy :: Proxy GhcPath)
  ]

newtype OptionMtimeChangeDelay = OptionMtimeChangeDelay Int

instance IsOption OptionMtimeChangeDelay where
  defaultValue = OptionMtimeChangeDelay 10000
  showDefaultValue (OptionMtimeChangeDelay v) = Just (show v)
  parseValue     = fmap OptionMtimeChangeDelay . safeRead
  optionName     = return "mtime-change-delay"
  optionHelp     = return $ "How long to wait before attempting to detect"
                   ++ "file modification, in microseconds"

newtype GhcPath = GhcPath FilePath

instance IsOption GhcPath where
  defaultValue = GhcPath "ghc"
  optionName   = return "with-ghc"
  optionHelp   = return "The ghc compiler to use"
  parseValue   = Just . GhcPath

main :: IO ()
main =
  defaultMainWithIngredients
    (includingOptions extraOptions : defaultIngredients)
    tests
