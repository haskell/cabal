import Test.Cabal.Prelude

import Control.Applicative ((<$>))
import System.Directory ( createDirectoryIfMissing )
import qualified Data.ByteString.Char8 as BS

main = cabalTest . withSourceCopy $ do
    limit <- getOpenFilesLimit
    cwd <- testCurrentDir <$> getTestEnv

    case limit of
        Just n -> do
            liftIO $ createDirectoryIfMissing False (cwd </> "data")
            forM_ [1 .. n + 100] $ \i -> 
                liftIO $ BS.writeFile (cwd </> "data" </> ("data-file-" ++ show i) <.> "txt") (BS.pack "a data file\n")
            cabal "new-sdist" ["many-data-files"]
        Nothing -> skip
