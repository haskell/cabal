import Test.Cabal.Prelude

import System.Directory (createDirectoryIfMissing)

-- Path too long for .tar distribution.
main = cabalTest $ do
  fails $ cabal "check" []
