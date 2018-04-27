import Control.Monad.IO.Class
import System.IO.Temp (withSystemTempDirectory)
import Test.Cabal.Prelude
main = setupAndCabalTest $ withSystemTempDirectory "cabal-testsuite.included-in-sdist" $ \dir -> do
  let fn = dir </> "sources"
  cabal "sdist" ["--list-sources=" ++ fn]
  liftIO $ putStrLn =<< readFile fn
  assertFileDoesContain fn "data/blah/a.dat"
  assertFileDoesContain fn "extra-src/blah/a.html"
  assertFileDoesContain fn "extra-doc/blah/a.tex"
