import MyLib (message)
import OtherLib (otherMessage)

main :: IO ()
main = putStrLn message >> putStrLn otherMessage
