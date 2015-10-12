{-# LANGUAGE OverloadedStrings #-}
-- | This module is only to verify that Old and New parsers agree
module PostParser where

import qualified Distribution.ParseUtils as O (Field(..))
import qualified Parser as N (Field(..), Name(..), FieldLine(..), SectionArg(..), getName)

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.Char as C
import qualified Data.Text as T
import qualified Data.Foldable as F
import qualified Data.Text.Encoding as T

toUtf8 :: String -> ByteString
toUtf8 = T.encodeUtf8 . T.pack

postProcessFields :: [O.Field] -> [N.Field ()]
postProcessFields [] = []
postProcessFields (O.F line name "" : xs) =
    let field = N.Field (toName name) [ N.FieldLine () $ toUtf8 "" ]
        rest = postProcessFields xs
    in field : rest
postProcessFields (O.F line name content : xs) =
    let field = N.Field (toName name) [ N.FieldLine () $ toUtf8 c | c <- lines content ]
        rest = postProcessFields xs
    in field : rest
postProcessFields (O.Section line name arg fields : xs) =
    let field = N.Section (toName name) [N.SecArgName () $ toUtf8 arg | not $ null arg] (postProcessFields fields)
        rest = postProcessFields xs
    in field : rest
postProcessFields (O.IfBlock line arg fields fields' : xs) =
    let field  = N.IfElseBlock [N.SecArgName () $ toUtf8 arg] (postProcessFields fields) (postProcessFields fields')
        rest = postProcessFields xs
    in field : rest

postProcessFields2 :: [N.Field a] -> [N.Field a]
postProcessFields2 = map f
  where f (N.Field name lines)
           | N.getName name == "description"  = N.Field name $ concatLines $ filter p' $ map g $ lines
           | otherwise                        = N.Field name $ filter p  $ map g $ lines
        f (N.Section name args fields) = N.Section name (flattenArgs args) $ postProcessFields2 fields
        f (N.IfElseBlock args t e) = N.IfElseBlock (flattenArgs args) (postProcessFields2 t) (postProcessFields2 e)
        p  (N.FieldLine _ content) = not $ B.null content
        p' (N.FieldLine _ content) = not (B.null content) && content /= "."
        -- Description normalising is quite a trick
        g (N.FieldLine ann content)= N.FieldLine ann (trimB8 content)
        concatLines [] = []
        concatLines lines@(N.FieldLine ann _ : _) = [N.FieldLine ann $ B.concat (map lineContent lines) ]
        lineContent (N.FieldLine ann content) = trimB8 $ B8.filter notCurly content
        notCurly '{' = False
        notCurly '}' = False
        notCurly ' ' = False
        notCurly _   = True
        trimB8         = B.reverse . B8.dropWhile C.isSpace . B.reverse . B8.dropWhile C.isSpace

flattenArgs [] = []
flattenArgs xs@(first : _) = [ N.SecArgName (g first) $ B8.filter (not . C.isSpace) $ F.foldMap f xs]
  where f (N.SecArgName  _ x) = x
        f (N.SecArgStr   _ x) = B8.pack ("\"" ++ x ++ "\"")
        f (N.SecArgNum   _ x) = x
        f (N.SecArgOther _ x) = x
        g (N.SecArgName  p x) = p
        g (N.SecArgStr   p x) = p
        g (N.SecArgNum   p x) = p
        g (N.SecArgOther p x) = p

toName :: String -> N.Name ()
toName = N.Name () . toUtf8
