import Test.Cabal.Prelude
import System.Environment (setEnv)
import Distribution.Client.ScriptUtils (getScriptCacheDirectory)

main = cabalTest . void $ do
    cabal' "v2-clean" ["script.hs"]
