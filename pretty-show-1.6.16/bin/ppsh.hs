import Text.Show.Pretty
import System.Environment
import System.IO(hPutStrLn,stderr)

main :: IO ()
main =
  do as <- getArgs
     case as of
       ["--test"] -> interactLn (show . selftest1)

       ["--html"] ->
         do txt <- getContents
            case parseValue txt of
              Just v  ->
                do dir <- getDataDir
                   let opts = defaultHtmlOpts { dataDir = dir }
                   putStrLn (valToHtmlPage opts v)
              Nothing -> hPutStrLn stderr "Failed to parse value."

       []         -> interactLn $ \s -> case parseValue s of
                                          Just v  -> show (valToDoc v)
                                          Nothing -> s
       _ -> hPutStrLn stderr $ unlines
              [ "usage: ppsh < showed_value > pretty_value"
              , "   --html      Generate HTML."
              , "   --test      Self test: True means we passed."
              ]


interactLn :: (String -> String) -> IO ()
interactLn f = interact f >> putStrLn ""

selftest :: Value -> Bool
selftest v = case parseValue $ show $ valToDoc v of
               Just v1  -> v1 == v
               Nothing  -> False

selftest1 :: String -> Bool
selftest1 txt = case parseValue txt of
                  Just v  -> selftest v
                  Nothing -> True



