-- | Utilities to implemenet cabal @v2-sdist@.
module Distribution.Client.SrcDist (
    allPackageSourceFiles,
)  where


import Control.Exception (IOException, evaluate)
import System.Directory  (getTemporaryDirectory)
import System.FilePath   ((</>))

import Distribution.Compat.Exception                 (catchIO)
import Distribution.Package                          (packageName)
import Distribution.PackageDescription.Configuration (flattenPackageDescription)
import Distribution.PackageDescription.Parsec        (readGenericPackageDescription)
import Distribution.Pretty                           (prettyShow)
import Distribution.Simple.Setup                     (Flag (..), defaultSDistFlags, sdistCommand)
import Distribution.Simple.Utils                     (warn, withTempDirectory)
import Distribution.Verbosity                        (Verbosity, lessVerbose, normal)
import Distribution.Version                          (intersectVersionRanges, mkVersion, orLaterVersion)

import Distribution.Client.Setup        (SDistFlags (..))
import Distribution.Client.SetupWrapper (SetupScriptOptions (..), setupWrapper)
import Distribution.Client.Utils        (tryFindAddSourcePackageDesc)

-- | List all source files of a given add-source dependency. Exits with error if
-- something is wrong (e.g. there is no .cabal file in the given directory).
allPackageSourceFiles :: Verbosity -> SetupScriptOptions -> FilePath
                         -> IO [FilePath]
allPackageSourceFiles verbosity setupOpts0 packageDir = do
  pkg <- do
    let err = "Error reading source files of package."
    desc <- tryFindAddSourcePackageDesc verbosity packageDir err
    flattenPackageDescription `fmap` readGenericPackageDescription verbosity desc
  globalTmp <- getTemporaryDirectory
  withTempDirectory verbosity globalTmp "cabal-list-sources." $ \tempDir -> do
    let file      = tempDir </> "cabal-sdist-list-sources"
        flags     = defaultSDistFlags {
          sDistVerbosity   = Flag $ if verbosity == normal
                                    then lessVerbose verbosity else verbosity,
          sDistListSources = Flag file
          }
        setupOpts = setupOpts0 {
          -- 'sdist --list-sources' was introduced in Cabal 1.18.
          useCabalVersion = intersectVersionRanges
                              (orLaterVersion $ mkVersion [1,18,0])
                              (useCabalVersion setupOpts0),
          useWorkingDir = Just packageDir
          }

        doListSources :: IO [FilePath]
        doListSources = do
          setupWrapper verbosity setupOpts (Just pkg) sdistCommand (const flags) (const [])
          fmap lines . readFile $ file

        onFailedListSources :: IOException -> IO ()
        onFailedListSources e = do
          warn verbosity $
            "Could not list sources of the package '"
            ++ prettyShow (packageName pkg) ++ "'."
          warn verbosity $
            "Exception was: " ++ show e

    -- Run setup sdist --list-sources=TMPFILE
    r <- doListSources `catchIO` (\e -> onFailedListSources e >> return [])
    -- Ensure that we've closed the 'readFile' handle before we exit the
    -- temporary directory.
    _ <- evaluate (length r)
    return r
