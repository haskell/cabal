module Main where

import System (getArgs)
import Data.Char   (isDigit)

main = do
    args <- getArgs
    case length args of
      1 -> do n <- saferead (head args)
              putStrLn ("module Text.XML.HaXml."++constructor 1 n++" where\n")
              putStrLn ("import Text.XML.HaXml.Xml2Haskell\n")
              putStrLn (mkOneOf n)
      2 -> do n <- saferead (args!!0)
              m <- saferead (args!!1)
              putStrLn ("module Text.XML.HaXml.OneOfN where\n")
              putStrLn ("import Text.XML.HaXml.Xml2Haskell\n")
              mapM_ (putStrLn . mkOneOf) [n..m]
      _ -> error "Usage: MkOneOf n [m]"

---- main text-generating function ----
mkOneOf :: Int -> String
mkOneOf n =
    "data "++ typename n 12
    ++ "\n   "++ format 3 78 3 " = " " | "
                        (zipWith (\m v->constructor m n++" "++v)
                                 [1..n]
                                 (take n variables))
    ++ "\n    deriving (Eq,Show)"
    ++ "\n\ninstance "++ format 10 78 10 "(" ","
                                (map ("XmlContent "++) (take n variables))
    ++ ")\n    => XmlContent ("++ typename n 26 ++")\n  where"
    ++ "\n    fromElem cs ="
    ++ "\n       "++ format 7 78 7 " (" " $ "
                            (map (\v->"choice "++constructor v n) [1..n])
    ++ "\n        $ (\\c->(Nothing,c))) cs"
    ++ concatMap (\v->"\n    toElem ("++constructor v n++" x) = toElem x")
                 [1..n]
    ++ "\n\n----"

---- constructor names ----
typename :: Int -> Int -> String
typename n pos = constructor 1 n ++ format pos 78 pos " " " " (take n variables)

constructor :: Int -> Int -> String
constructor n m = ordinal n ++"Of" ++ show m

ordinal :: Int -> String
ordinal n | n <= 20   = ordinals!!n
ordinal n | otherwise = "Choice"++show n

ordinals = ["Zero","One","Two","Three","Four","Five","Six","Seven","Eight"
           ,"Nine","Ten","Eleven","Twelve","Thirteen","Fourteen","Fifteen"
           ,"Sixteen","Seventeen","Eighteen","Nineteen","Twenty"]

---- variable names ----
variables = [ v:[] | v <- ['a'..'z']]
            ++ [ v:w:[] | v <- ['a'..'z'], w <- ['a'..'z']]

---- simple pretty-printing ----

format :: Int		-- current position on page
       -> Int		-- maximum width of page
       -> Int		-- amount to indent when a newline is emitted
       -> String	-- text to precede first value
       -> String	-- text to precede subsequent values
       -> [String]	-- list of values to format
        -> String
format cur max ind s0 s1 []     = ""
format cur max ind s0 s1 (x:xs)
    | sameline < max  = s0 ++ x ++ format sameline max ind s1 s1 xs
    | otherwise       = "\n" ++ replicate ind ' ' ++
                        s0 ++ x ++ format newline max ind s1 s1 xs
                where sameline = cur + length s0 + length x
                      newline  = ind + length s0 + length x

---- safe integer parsing ----
saferead :: String -> IO Int
saferead s | all isDigit s = return (read s)
saferead s | otherwise     = error ("expected a number on the commandline, "
                                    ++"but got \""++s++"\" instead")
