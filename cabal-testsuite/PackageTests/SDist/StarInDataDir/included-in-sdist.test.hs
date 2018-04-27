import Control.Monad.IO.Class
import System.FilePath (normalise)
import System.IO.Temp (withSystemTempDirectory)
import Test.Cabal.Prelude
main = setupAndCabalTest $ withSystemTempDirectory "cabal-testsuite.included-in-sdist" $ \dir -> do
  let fn = dir </> "sources"
  cabal "sdist" ["--list-sources=" ++ fn]
  files <- liftIO $ fmap (fmap normalise . lines) $ readFile fn
  assertBool "has */a.dat" ("*/a.dat" `elem` files)
  assertBool "hasn't a/a.dat" $ not ("a/a.dat" `elem` files)
