import Test.Cabal.Prelude
import Data.List
import qualified Data.Char as Char
main = setupAndCabalTest $ do
    skipUnless =<< ghcVersionIs (>= mkVersion [8,1])
    withPackageDb $ do
      containers_id <- getIPID "containers"
      withDirectory "repo/sigs-0.1.0.0" $ setup_install_with_docs ["--ipid", "sigs-0.1.0.0"]
      withDirectory "repo/indef-0.1.0.0" $ setup_install_with_docs ["--ipid", "indef-0.1.0.0"]
      withDirectory "repo/sigs-0.1.0.0" $ do
        -- NB: this REUSES the dist directory that we typechecked
        -- indefinitely, but it's OK; the recompile checker should get it.
        setup_install_with_docs ["--ipid", "sigs-0.1.0.0",
                       "--instantiate-with", "Data.Map=" ++ containers_id ++ ":Data.Map"]
      withDirectory "repo/indef-0.1.0.0" $ do
        -- Ditto.
        setup_install_with_docs ["--ipid", "indef-0.1.0.0",
                       "--instantiate-with", "Data.Map=" ++ containers_id ++ ":Data.Map"]
      withDirectory "repo/exe-0.1.0.0" $ do
        setup_install []
        runExe' "exe" [] >>= assertOutputContains "fromList [(0,2),(2,4)]"

