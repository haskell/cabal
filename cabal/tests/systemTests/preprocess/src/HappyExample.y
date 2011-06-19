{
module HappyExample where
import Data.Char
}

%name calc
%tokentype { Token }
%expect 0


%token 
	let		{ TokenLet }
	in		{ TokenIn }
	int		{ TokenInt $$ }
	var		{ TokenVar $$ }
	'='		{ TokenEq }
	'+'		{ TokenPlus }
	'-'		{ TokenMinus }
	'*'		{ TokenTimes }
	'/'		{ TokenDiv }
	'('		{ TokenOB }
	')'		{ TokenCB }


%%

Exp :: { Exp }
Exp : let var '=' Exp in Exp	{ Let $2 $4 $6 }
    | Exp1			{ Exp1 $1 }

Exp1 : Exp1 '+' Term		{ Plus $1 $3 }
     | Exp1 '-' Term		{ Minus $1 $3 }
     | Term			{ Term $1 }

Term : Term '*' Factor	{ Times $1 $3 }
     | Term '/' Factor	{ Div $1 $3 }
     | Factor			{ Factor $1 }

Factor : int			{ Int $1 }
	 | var			{ Var $1 }
	 | '(' Exp ')'		{ Brack $2 }


{

happyError :: [Token] -> a
happyError _ = error ("Parse error\n")


data Exp  = Let String Exp Exp | Exp1 Exp1 
data Exp1 = Plus Exp1 Term | Minus Exp1 Term | Term Term 
data Term = Times Term Factor | Div Term Factor | Factor Factor 
data Factor = Int Int | Var String | Brack Exp 


data Token
	= TokenLet
	| TokenIn
	| TokenInt Int
	| TokenVar String
	| TokenEq
	| TokenPlus
	| TokenMinus
	| TokenTimes
	| TokenDiv
	| TokenOB
	| TokenCB

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs) 
	| isSpace c = lexer cs
	| isAlpha c = lexVar (c:cs)
	| isDigit c = lexNum (c:cs)
lexer ('=':cs) = TokenEq : lexer cs
lexer ('+':cs) = TokenPlus : lexer cs
lexer ('-':cs) = TokenMinus : lexer cs
lexer ('*':cs) = TokenTimes : lexer cs
lexer ('/':cs) = TokenDiv : lexer cs
lexer ('(':cs) = TokenOB : lexer cs
lexer (')':cs) = TokenCB : lexer cs

lexNum cs = TokenInt (read num) : lexer rest
	where (num,rest) = span isDigit cs

lexVar cs =
   case span isAlpha cs of
	("let",rest) -> TokenLet : lexer rest
	("in",rest)  -> TokenIn : lexer rest
	(var,rest)   -> TokenVar var : lexer rest

}
