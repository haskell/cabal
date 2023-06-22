import Distribution.Simple
import Module (message)

main = putStrLn ("Setup.hs: " ++ message) >> defaultMain
