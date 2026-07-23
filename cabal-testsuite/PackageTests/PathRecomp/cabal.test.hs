import Test.Cabal.Prelude
import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)

main = cabalTest $ recordMode DoNotRecord $ do
    -- First run cabal repl with the normal PATH
    originalPath <- fromMaybe "" <$> liftIO (lookupEnv "PATH")

    -- Run repl with original PATH
    cabalWithStdin "repl" ["lib:PathRecomp"] "" >>= assertOutputContains "module loaded"

    -- Now modify the PATH by prefixing with a dummy directory
    -- This simulates a user modifying their PATH between cabal commands
    let modifiedPath = "/dummy/path:" ++ originalPath
    withEnv [("PATH", Just modifiedPath)] $ do
        -- Run repl with modified PATH - this should still work
        r <- cabalWithStdin "repl" ["lib:PathRecomp"] "(Prelude.fmap . Prelude.fmap) (\"/dummy/path\" `Data.List.isInfixOf`) (System.Environment.lookupEnv \"PATH\")"
        assertOutputContains "module loaded" r
        assertOutputContains "Just True" r
