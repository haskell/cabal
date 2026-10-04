-- | Set up the project for building with @buck2@
module Distribution.Client.Buck2.Setup
  ( checkBuck2Prelude
  , ensureBuckconfigAndPackage
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import System.Directory (doesDirectoryExist, doesFileExist)
import System.FilePath ((</>))

import Distribution.Simple.Utils (dieWithException, notice)

import Distribution.Client.Errors (CabalInstallException (Buck2NoPrelude))

-- | Check that the @buck2@ support code is at @buck2\/@
checkBuck2Prelude :: Verbosity -> FilePath -> IO ()
checkBuck2Prelude verbosity projectRoot = do
  exists <- doesDirectoryExist (projectRoot </> "buck2")
  unless exists $ dieWithException verbosity Buck2NoPrelude

-- | Copy @.buckconfig@ and @PACKAGE@ if they don't exist
ensureBuckconfigAndPackage :: Verbosity -> FilePath -> IO ()
ensureBuckconfigAndPackage verbosity projectRoot = do
  copyIfMissing verbosity (exampleDir </> ".buckconfig") (projectRoot </> ".buckconfig")
  copyIfMissing verbosity (exampleDir </> "PACKAGE") (projectRoot </> "PACKAGE")
  where
    exampleDir = projectRoot </> "buck2" </> "example"

copyIfMissing :: Verbosity -> FilePath -> FilePath -> IO ()
copyIfMissing verbosity src dest = do
  exists <- doesFileExist dest
  unless exists $ do
    contents <- readFile src
    writeFile dest contents
    notice verbosity $ "cabal buck2: created " ++ dest ++ " (from " ++ src ++ ")"
