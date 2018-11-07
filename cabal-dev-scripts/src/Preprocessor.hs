{-# LANGUAGE DeriveFunctor #-}
module Main (main) where

import Control.Applicative
import Control.Monad       (ap, unless)
import Data.Char           (isSpace)
import System.Environment  (getArgs)
import System.IO           (hPutStrLn, stderr)

import qualified Data.Map              as Map
import qualified System.Console.GetOpt as O

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

type Flag = String
type Var  = String

data Token
    = Raw String
    | Var Int Var
    | If Flag
    | Else
    | Endif
    | Def Var
    | Enddef
    | Comment
    | Error String
  deriving Show

data Tree
    = TRaw String
    | TVar Int Var
    | TDef Var Forest
    | TIf Flag Forest Forest
  deriving Show

type Forest = [Tree]

-------------------------------------------------------------------------------
-- Lex
-------------------------------------------------------------------------------

tokens :: String -> [Token]
tokens = map classify . lines where
    classify :: String -> Token
    classify s0 = case s1 of
        []        -> Raw ""
        ('#' : _) -> Comment
        ('$' : s) -> Var (length ws) s
        ('%' : s)
            | Just s' <- "if"     `isPrefixOf'` s -> If $ dropWhile isSpace s'
            | Just _  <- "else"   `isPrefixOf'` s -> Else
            | Just _  <- "endif"  `isPrefixOf'` s -> Endif
            | Just s' <- "def"    `isPrefixOf'` s -> Def $ dropWhile isSpace s'
            | Just _  <- "enddef" `isPrefixOf'` s -> Enddef
            | otherwise                           -> Error $ "Unknown command: " ++ s
        _ -> Raw s0
      where
        (ws, s1) = span isSpace s0

-- | 'isPrefixOf' returning the rest of the string
isPrefixOf' :: String -> String -> Maybe String
isPrefixOf' []     ys                 = Just ys
isPrefixOf' (_:_)  []                 = Nothing
isPrefixOf' (x:xs) (y:ys) | x == y    = isPrefixOf' xs ys
                          | otherwise = Nothing

-------------------------------------------------------------------------------
-- Parse
-------------------------------------------------------------------------------

newtype P a = P { unP :: [Token] -> Either String ([Token], a) }
  deriving Functor

instance Applicative P where
    pure x = P $ \toks -> Right (toks, x)
    (<*>)  = ap

instance Alternative P where
    empty = P $ \_ -> Left "empty"
    x <|> y = P $ \toks -> case unP x toks of
        Right res -> Right res
        Left  _   -> unP y toks

instance Monad P where
    return = pure
    m >>= k = P $ \toks0 -> do
        (toks1, x) <- unP m toks0
        unP (k x) toks1

runP :: P a -> [Token] -> Either String a
runP (P p) = fmap snd . p

forest :: [Token] -> Either String Forest
forest = runP (forestP <* eof) where
    forestP = many tree
    tree    = rawP <|> varP <|> ifP <|> defP

    ifP :: P Tree
    ifP = do
        f <- if_
        c <- forestP
        a <- else_ *> forestP <|> pure []
        endif_
        return (TIf f c a)

    defP :: P Tree
    defP = do
        v <- def_
        t <- forestP
        enddef_
        return (TDef v t)

    rawP :: P Tree
    rawP = token "Raw " $ \tok -> case tok of
        Raw s -> Just (TRaw s)
        _     -> Nothing

    varP :: P Tree
    varP = token "Var" $ \toks -> case toks of
        Var n v -> Just (TVar n v)
        _       -> Nothing

    if_ :: P Flag
    if_ = token "If" $ \toks -> case toks of
        If f -> Just f
        _    -> Nothing

    def_ :: P Var
    def_ = token "Def" $ \toks -> case toks of
        Def v -> Just v
        _     -> Nothing

    else_ :: P ()
    else_ = token "Else" $ \toks -> case toks of
        Else -> Just ()
        _    -> Nothing

    enddef_ :: P ()
    enddef_ = token "Enddef" $ \toks -> case toks of
        Enddef -> Just ()
        _      -> Nothing

    endif_ :: P ()
    endif_ = token "Endif" $ \toks -> case toks of
        Endif -> Just ()
        _     -> Nothing

    token :: String -> (Token -> Maybe a) -> P a
    token name f = P $ \toks -> case toks of
        (tok : toks') | Just x <- f tok -> Right (toks', x)
        _                               -> Left $ "Expected " ++ name

    eof :: P ()
    eof = P $ \toks ->
        if null toks
        then Right (toks, ())
        else Left $ "expected end-of-input, got: " ++ show (take 1 toks)


-------------------------------------------------------------------------------
-- Process
-------------------------------------------------------------------------------

process :: [Flag] -> String -> String
process flags
    = either error (unlines . go Map.empty)
    . forest
    . filter (not . isComment)
    . tokens
  where
    isComment Comment = True
    isComment _       = False

    go :: Map.Map Var Forest -> Forest -> [String]
    go _    []                = []
    go vars (TRaw s : toks)   = s : go vars toks
    go vars (TVar _ v : toks) = case Map.lookup v vars of
        Nothing -> go vars toks
        Just f  -> go vars (f ++ toks)
    go vars (TIf f c a : toks)
        | f `elem` flags      = go vars (c ++ toks)
        | otherwise           = go vars (a ++ toks)
    go vars (TDef n f : toks) = go (Map.insert n f vars) toks

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

data Opts = Opts
    { optsFlags  :: [Flag]
    , optsOutput :: Maybe FilePath
    }

emptyOpts :: Opts
emptyOpts = Opts [] Nothing

optDescrs :: [O.OptDescr (Opts -> Opts)]
optDescrs =
    [ O.Option "o" []
        (O.ReqArg (\arg opts -> opts { optsOutput = Just arg }) "OUTPUT")
        "Output"
    , O.Option "f" []
        (O.ReqArg (\arg opts -> opts { optsFlags = arg : optsFlags opts }) "FLAG")
        "Flag"
    ]

main :: IO ()
main = do
    (optEndos, args, errs) <- O.getOpt O.RequireOrder optDescrs <$> getArgs
    let opts = foldr ($) emptyOpts optEndos :: Opts
    unless (null errs) $ do
        mapM_ (hPutStrLn stderr) errs
        fail "errors"
    case args of
        [fp] -> do
            contents <- readFile fp
            let output = process (optsFlags opts) contents
            maybe putStr writeFile (optsOutput opts) output
        _ -> fail "Expecting exactly one argument"
