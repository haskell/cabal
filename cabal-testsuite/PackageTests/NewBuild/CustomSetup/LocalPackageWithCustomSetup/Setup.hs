import Distribution.Simple
import SetupDep (message)

main = putStrLn ("pkg Setup.hs: " ++ message) >> defaultMain
