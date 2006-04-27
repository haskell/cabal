module Main where

import System (getArgs)
import IO
import Maybe
import List   (isSuffixOf)

import Text.XML.HaXml.Types      (DocTypeDecl(..))
import Text.XML.HaXml.Parse      (dtdParse)
import Text.XML.HaXml.Pretty     (markupdecl)
import Text.XML.HaXml.Wrappers   (fix2Args)
import Text.PrettyPrint.HughesPJ (render,vcat)

-- This is another trivial application that reads an XML DTD from
-- a file (or stdin) and writes it back to another file (or stdout).
-- It should deal with the external subset fully, collecting and
-- in-lining all the individual files associated with the DTD.
-- Note that PE references used in definitions are also expanded
-- fully in the output.

main =
  fix2Args >>= \(inf,outf)->
  ( if inf=="-" then getContents
    else readFile inf )            >>= \content->
  ( if outf=="-" then return stdout
    else openFile outf WriteMode ) >>= \o->
  ( hPutStrLn o . render . vcat . map markupdecl . fromDTD . dtdParse inf)
        content

fromDTD Nothing = error "no DTD found"
fromDTD (Just (DTD _ _ ds)) = ds

