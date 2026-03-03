{-# LANGUAGE LambdaCase #-}
module Main (main) where

import Control.Applicative
import Control.Monad          (void)
import Data.Foldable          (for_)
import Data.List              (sortBy)
import Data.Maybe             (fromMaybe)
import Data.Ord               (comparing)
import Language.Haskell.Lexer (PosToken, Token (..), lexerPass0)
import System.Environment     (getArgs)
import Text.Regex.Applicative (RE)

import qualified Data.Map.Strict        as Map
import qualified Text.Regex.Applicative as RE

main :: IO ()
main = do
    args <- getArgs

    data_ <- traverse processFile args

    putStrLn "Modules"
    let modules = sortBy (flip $ comparing snd) $ Map.toList $ Map.fromListWith (+)
          [ (mn, 1 :: Int)
          | xs <- data_
          , (mn, _) <- xs
          ]

    for_ (take 30 modules) $ \(mn, n) ->
        putStrLn $ mn ++ "  " ++ show n

    putStrLn ""

    putStrLn "Symbols"
    let symbols = sortBy (flip $ comparing snd) $ Map.toList $ Map.fromListWith (+)
          [ ((mn,sym), 1 :: Int)
          | xs <- data_
          , (mn, syms) <- xs
          , sym <- syms
          ]

    for_ (take 50 symbols) $ \((mn,sym), n) ->
        putStrLn $ mn ++ "." ++ sym ++ "  " ++ show n

processFile :: FilePath -> IO [(String, [String])]
processFile fp = do
    contents <- readFile fp
    let tokens = filter (\(t, _) -> t `notElem` [Whitespace, Comment, Commentstart, NestedComment])
               $ lexerPass0 contents

    return $ fromMaybe [] $ RE.match (somewhere imports) tokens

imports :: RE PosToken (String, [String])
imports = (,)
    <$ reservedid "import" <*> (conid <|> qconid) <*> msymbols
  where
    msymbols :: RE PosToken [String]
    msymbols =special "(" *> symbols <* special ")" <|> pure []

    symbols :: RE PosToken [String]
    symbols = liftA2 (:) symbol $ many (special "," *> symbol)

    symbol :: RE PosToken String
    symbol = varid <|> special "(" *> varsym <* special ")"


-------------------------------------------------------------------------------
-- regex-applicative + haskell-lexer
-------------------------------------------------------------------------------

anything :: RE s ()
anything = void $ RE.few RE.anySym

somewhere :: RE s a -> RE s [a]
somewhere re = anything *> RE.few (re <* anything)

reservedid :: String -> RE PosToken ()
reservedid k = RE.msym $ \case
    (Reservedid, (_, k')) | k == k' -> Just ()
    _                               -> Nothing

special :: String -> RE PosToken ()
special k = RE.msym $ \case
    (Special, (_, k')) | k == k' -> Just ()
    _                            -> Nothing

conid :: RE PosToken String
conid = RE.msym $ \case
    (Conid, (_, k)) -> Just k
    _               -> Nothing

qconid :: RE PosToken String
qconid = RE.msym $ \case
    (Qconid, (_, k)) -> Just k
    _                -> Nothing

varid :: RE PosToken String
varid = RE.msym $ \case
    (Varid, (_, k)) -> Just k
    _               -> Nothing

varsym :: RE PosToken String
varsym = RE.msym $ \case
    (Varsym, (_, k)) -> Just k
    _                -> Nothing
