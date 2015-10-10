{
{-# LANGUAGE MultiParamTypeClasses, BangPatterns #-}
module Parser where

import Lexer
import LexerMonad (unLex, LexState(..), LexResult(..), Position(..))
}

%name parse test
%tokentype { LToken }
%error { parseError }
%monad { Lex }
%lexer { lexToken }{ L _ EOF }

