module Main where

import System (getArgs)
import IO

import Text.XML.HaXml.Lex      (xmlLex)
import Text.XML.HaXml.Wrappers (fix2Args)

-- Debug the HaXml library by showing what the lexer generates.
main =
  fix2Args >>= \(inf,outf)->
  ( if inf=="-" then getContents
    else readFile inf )            >>= \content->
  ( if outf=="-" then return stdout
    else openFile outf WriteMode ) >>= \o->
  mapM_ ( hPutStrLn o . show ) (xmlLex inf content)

