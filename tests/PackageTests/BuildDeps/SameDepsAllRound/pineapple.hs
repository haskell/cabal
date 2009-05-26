import qualified Data.ByteString.Char8 as C
import System.Time

main = do
    getClockTime
    let text = "pineapple"
    C.putStrLn $ C.pack text
