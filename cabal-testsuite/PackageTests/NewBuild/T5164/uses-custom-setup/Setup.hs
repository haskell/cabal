import Distribution.Simple (defaultMain)
import SetupLib (printExampleTxt)

main :: IO ()
main = do
  printExampleTxt
  defaultMain
