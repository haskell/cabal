import Test.Cabal.Prelude

import System.Directory
import System.FilePath

main = cabalTest $ withShorterPathForNewBuildStore $ withRepo "repo" $ do
  cabal "v2-build"
    [ "example"
    , "--enable-documentation"
    , "--haddock-quickjump"
    ]
  libDir <- findDependencyInStore "indef"
  assertFileDoesContain (libDir </> "cabal-hash.txt") "haddock-quickjump: True"
  docIndexJsonExists <- liftIO $ doesFileExist (libDir </> "share" </> "doc" </> "html" </> "doc-index.json")
  assertBool "doc-index.json doesn't exist, --quickjump is probably not passed to haddock" docIndexJsonExists
