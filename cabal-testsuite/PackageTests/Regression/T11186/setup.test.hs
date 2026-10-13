import Test.Cabal.Prelude

-- Regression test for #11186: install-includes should accept a relative
-- ("local") path and install the header to the include directory.
main = setupAndCabalTest $ withPackageDb $ do
  setup_install []
  env <- getTestEnv
  shouldExist $
    testLibInstallDir env
      </> "install-includes-local-0.1.0.0"
      </> "include"
      </> "cbits"
      </> "clib.h"
