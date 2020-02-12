import Test.Cabal.Prelude
import qualified Data.ByteString.Char8 as BS8

main = setupAndCabalTest $ do
    env <- getTestEnv
    let mode = testRecordMode env

    setup_build []
    let autogenDir = testDistDir env </> "build" </> "autogen"

    defaultRecordMode RecordAll $ recordHeader ["cabal_macros.h"]
    contents <- liftIO $ BS8.readFile $ autogenDir </> "cabal_macros.h"
    -- we are only interested in CURRENT_ lines
    -- others change a lot based on available tools in the environment
    let contents' = BS8.unlines
                  $ filter (BS8.isInfixOf $ BS8.pack "CURRENT")
                  $ BS8.lines contents
    liftIO $ BS8.appendFile (testActualFile env) contents'
