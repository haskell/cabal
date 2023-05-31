-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      :  Distribution.Client.Init.Defaults
-- Copyright   :  (c) Brent Yorgey 2009
-- License     :  BSD-like
--
-- Maintainer  :  cabal-devel@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Default values to use in cabal init (if not specified in config/flags).
module Distribution.Client.Init.Defaults
  ( -- * default init values
    defaultApplicationDir
  , defaultSourceDir
  , defaultCabalVersion
  , defaultCabalVersions
  , defaultPackageType
  , defaultLicense
  , defaultLicenseIds
  , defaultMainIs
  , defaultChangelog
  , defaultCategories
  , defaultInitFlags
  , defaultLanguage
  , defaultVersion
  , defaultTestDir

    -- * MyLib defaults
  , myLibModule
  , myLibTestFile
  , myLibFile
  , myLibHs
  , myExeHs
  , myLibExeHs
  , myTestHs
  ) where

import Distribution.CabalSpecVersion (CabalSpecVersion (..))
import Distribution.Client.Init.Types (HsFilePath, InitFlags (..), PackageType (..), toHsFilePath)
import Distribution.FieldGrammar.Newtypes
import Distribution.ModuleName (ModuleName)
import qualified Distribution.ModuleName as ModuleName (fromString)
import qualified Distribution.SPDX.License as SPDX
import qualified Distribution.SPDX.LicenseId as SPDX
import Distribution.Simple (Language (..), License (..))
import Distribution.Simple.Flag (toFlag)
import Distribution.Types.Version
import Distribution.Verbosity (normal)

-- -------------------------------------------------------------------- --
-- Default flag and init values

defaultVersion :: Version
defaultVersion = mkVersion [0, 1, 0, 0]

defaultApplicationDir :: String
defaultApplicationDir = "app"

defaultSourceDir :: String
defaultSourceDir = "src"

defaultTestDir :: String
defaultTestDir = "test"

defaultCabalVersion :: CabalSpecVersion
defaultCabalVersion = CabalSpecV3_0

defaultPackageType :: PackageType
defaultPackageType = Executable

defaultChangelog :: FilePath
defaultChangelog = "CHANGELOG.md"

defaultLicense :: CabalSpecVersion -> SpecLicense
defaultLicense csv
  | csv < CabalSpecV2_2 = SpecLicense $ Right AllRightsReserved
  | otherwise = SpecLicense $ Left SPDX.NONE

defaultMainIs :: HsFilePath
defaultMainIs = toHsFilePath "Main.hs"

defaultLanguage :: Language
defaultLanguage = Haskell2010

defaultLicenseIds :: [SPDX.LicenseId]
defaultLicenseIds =
  [ SPDX.BSD_2_Clause
  , SPDX.BSD_3_Clause
  , SPDX.Apache_2_0
  , SPDX.MIT
  , SPDX.MPL_2_0
  , SPDX.ISC
  , SPDX.GPL_2_0_only
  , SPDX.GPL_3_0_only
  , SPDX.LGPL_2_1_only
  , SPDX.LGPL_3_0_only
  , SPDX.AGPL_3_0_only
  , SPDX.GPL_2_0_or_later
  , SPDX.GPL_3_0_or_later
  , SPDX.LGPL_2_1_or_later
  , SPDX.LGPL_3_0_or_later
  , SPDX.AGPL_3_0_or_later
  ]

defaultCategories :: [String]
defaultCategories =
  [ "Codec"
  , "Concurrency"
  , "Control"
  , "Data"
  , "Database"
  , "Development"
  , "Distribution"
  , "Game"
  , "Graphics"
  , "Language"
  , "Math"
  , "Network"
  , "Sound"
  , "System"
  , "Testing"
  , "Text"
  , "Web"
  ]

defaultCabalVersions :: [CabalSpecVersion]
defaultCabalVersions =
  [ CabalSpecV1_24
  , CabalSpecV2_0
  , CabalSpecV2_2
  , CabalSpecV2_4
  , CabalSpecV3_0
  , CabalSpecV3_4
  ]

defaultInitFlags :: InitFlags
defaultInitFlags = mempty{initVerbosity = toFlag normal}

-- -------------------------------------------------------------------- --
-- MyLib defaults

myLibModule :: ModuleName
myLibModule = ModuleName.fromString "MyLib"

myLibTestFile :: HsFilePath
myLibTestFile = toHsFilePath "MyLibTest.hs"

myLibFile :: HsFilePath
myLibFile = toHsFilePath "MyLib.hs"

-- | Default MyLib.hs file.  Used when no Lib.hs exists.
myLibHs :: String
myLibHs =
  unlines
    [ "module MyLib (someFunc) where"
    , ""
    , "someFunc :: IO ()"
    , "someFunc = putStrLn \"someFunc\""
    ]

myExeHs :: [String]
myExeHs =
  [ "module Main where"
  , ""
  , "main :: IO ()"
  , "main = putStrLn \"Hello, Haskell!\""
  ]

myLibExeHs :: [String]
myLibExeHs =
  [ "module Main where"
  , ""
  , "import qualified MyLib (someFunc)"
  , ""
  , "main :: IO ()"
  , "main = do"
  , "  putStrLn \"Hello, Haskell!\""
  , "  MyLib.someFunc"
  ]

-- | Default MyLibTest.hs file.
myTestHs :: String
myTestHs =
  unlines
    [ "module Main (main) where"
    , ""
    , "main :: IO ()"
    , "main = putStrLn \"Test suite not yet implemented.\""
    ]
