import Test.Cabal.Prelude

import System.Directory (createDirectoryIfMissing)

-- Omitting README in extra-doc-files
main = cabalTest $ do
  cabal "check" []
