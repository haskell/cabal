import Test.Cabal.Prelude

import System.Directory (createDirectoryIfMissing)

-- Omitting ChangeLog.md but not README in extra-doc-files
main = cabalTest $ do
  cabal "check" []
