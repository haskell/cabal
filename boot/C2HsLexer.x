{
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- | This contains a partial lexer for @.chs@ files; enough to extract
-- information from @{\#import\#}@ declarations.
--
-- This is lenient in that it will not reject things like
-- {# import const Data.Char #}
module Distribution.C2Hs.Lexer ( getImports ) where

import Control.Applicative ((<$>))
}

%wrapper "monad"

$module = [A-Za-z\.]

tokens :-

    $white+                      ;
    <0> "--".*                   ;

    <0> "{-"                     { \_ _ -> nested_comment }

    <chs> "import"               { \_ _ -> alex Import }
    <chs> "qualified"            ;
    <chs> "#}"                   { begin 0 }
    <0> "{#"                     { begin chs }
    <chs> $module+               { tok (\_ s -> alex (Module s)) }

    <0> [^\{]+                   ;
    <0> $printable               ;
    <chs> [^\#$module]+          ;

{

data Token = Import
           | Module String
           | End

tok f (p,_,_,s) len = f p (take len s)

alex :: a -> Alex a
alex = pure

alexEOF :: Alex Token
alexEOF = pure End

-- | Given a 'String' containing C2Hs, return a list of modules it @{\#import\#}@s.
getImports :: String -> Either String [String]
getImports = fmap extractDeps . lexC

-- from: https://github.com/simonmar/alex/blob/master/examples/haskell.x#L128
nested_comment :: Alex Token
nested_comment = go 1 =<< alexGetInput

    where go :: Int -> AlexInput -> Alex Token
          go 0 input = alexSetInput input *> alexMonadScan
          go n input =
            case alexGetByte input of
                Nothing -> err input
                Just (c, input') ->
                    case c of
                        45 ->
                            case alexGetByte input' of
                                Nothing -> err input'
                                Just (125,input_) -> go (n-1) input_
                                Just (_,input_) -> go n input_
                        123 ->
                            case alexGetByte input' of
                                Nothing -> err input'
                                Just (c',input_) -> go (addLevel c' $ n) input_
                        _ -> go n input'

          addLevel c' = if c' == 45 then (+1) else id

          err (pos,_,_,_) =
            let (AlexPn _ line col) = pos in
                alexError ("Error in nested comment at line " ++ show line ++ ", column " ++ show col)

extractDeps :: [Token] -> [String]
extractDeps []                   = []
extractDeps (Import:Module s:xs) = s : extractDeps xs
extractDeps (_:xs)               = extractDeps xs

lexC :: String -> Either String [Token]
lexC = flip runAlex loop

loop :: Alex [Token]
loop = do
    tok' <- alexMonadScan
    case tok' of
        End -> pure []
        _ -> (tok' :) <$> loop

}
