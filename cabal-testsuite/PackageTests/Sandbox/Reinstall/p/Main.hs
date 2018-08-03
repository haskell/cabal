import Q (message)

main :: IO ()
main = do
  putStrLn "-----BEGIN CABAL OUTPUT-----"
  putStrLn message
  putStrLn "-----END CABAL OUTPUT-----"
