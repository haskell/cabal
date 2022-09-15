import Test.Cabal.Prelude

import System.Directory
import System.FilePath

main = withShorterPathForNewBuildStore $ \storeDir -> cabalTest $ withRepo "repo" $ do
  cabalG ["--store-dir=" ++ storeDir] "v2-build"
    [ "example"
    , "--enable-documentation"
    , "--haddock-quickjump"
    ]
  liftIO $ do
    libDir <- findDependencyInStore storeDir "indef"
    assertFileDoesContain (libDir </> "cabal-hash.txt") "haddock-quickjump: True"
    docIndexJsonExists <- doesFileExist (libDir </> "share" </> "doc" </> "html" </> "doc-index.json")
    assertBool "doc-index.json doesn't exist, --quickjump is probably not passed to haddock" docIndexJsonExists
