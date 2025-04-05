import Test.Cabal.Prelude
import qualified Data.Map as Map
import qualified Control.Monad.IO.Class as IO
import System.Environment (getEnvironment)

main = cabalTest $ recordMode DoNotRecord $ do
    -- First run cabal repl with the normal PATH
    env <- IO.liftIO getEnvironment
    let originalPath = maybe "" id (lookup "PATH" env)

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
