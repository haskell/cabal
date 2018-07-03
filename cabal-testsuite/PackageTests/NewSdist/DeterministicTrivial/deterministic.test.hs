import Test.Cabal.Prelude
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as BS16
import qualified Crypto.Hash.SHA256 as SHA256
import System.FilePath
    ( (</>) )

main = cabalTest $ do
    cabal "new-sdist" ["deterministic"]
    env <- getTestEnv
    let dir = testCurrentDir env
        knownSdist = dir </> "deterministic-0.tar.gz"
        mySdist = dir </> "dist-newstyle" </> "sdist" </> "deterministic-0.tar.gz"
    
    known <- liftIO (BS.readFile knownSdist)
    unknown <- liftIO (BS.readFile mySdist)

    assertEqual "hashes didn't match for sdist" (BS16.encode $ SHA256.hash known) (BS16.encode $ SHA256.hash unknown)
