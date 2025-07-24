import System.Environment
import Control.Monad

main :: IO ()
main = putStrLn . unwords =<< getArgs
