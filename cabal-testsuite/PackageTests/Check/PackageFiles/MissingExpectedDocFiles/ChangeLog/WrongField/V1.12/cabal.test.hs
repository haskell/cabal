import Test.Cabal.Prelude

import System.Directory (createDirectoryIfMissing)

-- Included ChangeLog.md but not in extra-doc-files
main = cabalTest $ do
  cabal "check" []
