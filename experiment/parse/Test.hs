module Main where

import IndexUtils

import qualified OldParse
import Distribution.Simple.Utils (fromUTF8)

import qualified Parser
import qualified LexerMonad as Parser

import System.Environment
import Control.Exception (evaluate)
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Char
import Distribution.Simple.Utils (fromUTF8)

{-
old <- parseOldFile "diff/2.cabal" 
new <- parseNewFile "diff/2.cabal" 
let old'  = normaliseOldFields old
let new'  = normaliseNewFields new
let new'' = toOldFields new'
-}

parseOldFile :: FilePath -> IO [OldParse.Field]
parseOldFile file = do
    OldParse.ParseOk _ fs <- fmap parse $ readFile file
    return fs
  where
    parse  = OldParse.readFields . fromUTF8

parseNewFile :: FilePath -> IO [Parser.Field]
parseNewFile file = do
    Right fs <- fmap parse $ BS.readFile file
    return fs
  where
    parse  = Parser.readFields

toOldFields :: [Parser.Field] -> [OldParse.Field]
toOldFields [] = []
toOldFields (Parser.Field n ls            : rest) = OldParse.F (lineOfName n) (toOldName n) (toOldFieldContent ls)
                                                  : toOldFields rest
toOldFields (Parser.Section n@(Parser.Name _ sif)   as fs  :
             Parser.Section (Parser.Name _ selse) _  fs' : rest)
             | isIf sif, isElse selse             = OldParse.IfBlock (lineOfName n) (toOldArgs as) (toOldFields fs) (toOldFields fs')
                                                  : toOldFields rest
toOldFields (Parser.Section n@(Parser.Name _ sif) as fs  : rest)
            | isIf sif                            = OldParse.IfBlock (lineOfName n) (toOldArgs as) (toOldFields fs) []
                                                  : toOldFields rest
toOldFields (Parser.Section n      as fs  : rest) = OldParse.Section (lineOfName n) (toOldName n) (toOldArgs as) (toOldFields fs)
                                                  : toOldFields rest

isIf   s = BS.map toLower s == BS.pack "if"
isElse s = BS.map toLower s == BS.pack "else"

lineOfName (Parser.Name (Parser.Position l _) _) = l
toOldName (Parser.Name _ n) = map toLower (BS.unpack n)
toOldArgs            = concatMap toOldArg
  where
    toOldArg (Parser.SecArgName  _ a) = BS.unpack a
    toOldArg (Parser.SecArgStr   _ a) = a
    toOldArg (Parser.SecArgNum   _ a) = BS.unpack a
    toOldArg (Parser.SecArgOther _ a) = BS.unpack a

toOldFieldContent ls = intercalate "\n"
                     . normaliseFieldContent
                     $ [ BS.unpack l | Parser.FieldLine _ l <- ls ]

normaliseFieldContent =
    dropLeadingEmpty
  . dotToBlank
  . trimLines
  . mungeBraces
  . map fromUTF8
  where
    trimLines = map trim
    dropLeadingEmpty ("":ls) = ls
    dropLeadingEmpty     ls  = ls
    dotToBlank (l:ls) = l : map (\l -> if l == "." then "" else l)
                                (filter (/="") ls)
    dotToBlank []     = []

    -- The old parser did funny things with {}'s inside fields
    mungeBraces = concatMap (splitOn (\c -> c == '{' || c == '}'))

trim :: String -> String
trim = reverse . dropWhile (\c -> c == ' ' || c == '\t')
     . reverse . dropWhile (\c -> c == ' ' || c == '\t')

splitOn _ "" = [""]
splitOn p  s = unfoldr step s
  where
    step s | null s    = Nothing
           | otherwise = Just (l,drop 1 s') where (l,s') = break p s

normaliseOldFields :: [OldParse.Field] -> [OldParse.Field]
normaliseOldFields = filter (not . isCommentSection)
                   . map normaliseOldField
  where
    isCommentSection (OldParse.Section _ "--" _ []) = True
    isCommentSection _                              = False

normaliseOldField :: OldParse.Field -> OldParse.Field
normaliseOldField (OldParse.F       p n   v)  = OldParse.F       p n   v
normaliseOldField (OldParse.Section p n a fs) = OldParse.Section p n (normaliseOldSectionArgs a) (normaliseOldFields fs)
normaliseOldField (OldParse.IfBlock p c t e)  = OldParse.IfBlock p   (normaliseOldSectionArgs c) (normaliseOldFields t) (normaliseOldFields e)

normaliseOldSectionArgs n@(_:_:_) | head n == '"' && last n == '"' = init (tail n)
normaliseOldSectionArgs n = filter (/=' ') n

main = do
  [indexfile] <- getArgs
  cabalFiles <- IndexUtils.readPackageIndexFile indexfile
  sequence_
    [ do putStrLn $ "writing " ++ outfile
         LBS.writeFile outfile cabalFile
         writeFile (outfile ++ ".old") (show old)
         writeFile (outfile ++ ".new") (show new)
    | ((cabalFile, old, new), n) <- zip (diffFiles cabalFiles) [0..]
    , let outfile = "diff/" ++ show n ++ ".cabal" ]


diffFiles cabalFiles =
    [ (cabalFile, old', new')
    | let oldParse = OldParse.readFields . fromUTF8 . LBS.unpack
          newParse = Parser.readFields . toStrict
    , (cabalFile, OldParse.ParseOk _ old, Right new) <- map (\f -> (f, oldParse f, newParse f)) cabalFiles
    , let old'  = normaliseOldFields old
          new'  = toOldFields new
    , old' /= new'
    ]
  where
    toStrict = BS.concat . LBS.toChunks

diffFields as bs =
  [ (a,b) | (a,b) <- zip as bs, a/=b ]

