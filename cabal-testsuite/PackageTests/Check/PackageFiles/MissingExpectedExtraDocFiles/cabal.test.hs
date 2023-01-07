import Test.Cabal.Prelude

import System.Directory (createDirectoryIfMissing)

-- Omitting README but not ChangeLog.md in extra-doc-files
main = cabalTest $ do
  cabal "check" []
