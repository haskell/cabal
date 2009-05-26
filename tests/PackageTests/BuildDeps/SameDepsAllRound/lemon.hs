import qualified Data.ByteString.Char8 as C
import System.Time

main = do
    getClockTime
    let text = "lemon"
    C.putStrLn $ C.pack text
