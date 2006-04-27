module Main where

-- This program is provided to convert an XML file containing a DTD
-- into a Haskell module containing data/newtype definitions which
-- mirror the DTD.  Once you have used this program to generate your type
-- definitions, you should import Xml2Haskell wherever you intend
-- to read and write XML files with your Haskell programs.

import System
import IO
import List (nub,takeWhile,dropWhile)

import Text.XML.HaXml.Wrappers   (fix2Args)
import Text.XML.HaXml.Types      (DocTypeDecl(..))
import Text.XML.HaXml.Parse      (dtdParse)
import Text.XML.HaXml.DtdToHaskell.TypeDef  (TypeDef,ppTypeDef,mangle)
import Text.XML.HaXml.DtdToHaskell.Convert  (dtd2TypeDef)
import Text.XML.HaXml.DtdToHaskell.Instance (mkInstance)
import Text.PrettyPrint.HughesPJ (render,vcat)

main =
  fix2Args >>= \(inf,outf)->
  ( if inf=="-" then getContents
    else readFile inf )           >>= \content->
  ( if outf=="-" then return stdout
    else openFile outf WriteMode ) >>= \o->
  let (DTD name _ markup) = (getDtd . dtdParse inf) content
      decls = (nub . dtd2TypeDef) markup
      realname = if outf/="-" then mangle (trim outf)
                 else if null name then mangle (trim inf)
                 else mangle name
  in
  do hPutStrLn o ("module "++realname
                  ++" where\n\nimport Text.XML.HaXml.Xml2Haskell"
                  ++"\nimport Text.XML.HaXml.OneOfN")
     hPutStrLn o "\n\n{-Type decls-}\n"
     (hPutStrLn o . render . vcat . map ppTypeDef) decls
     hPutStrLn o "\n\n{-Instance decls-}\n"
     (hPutStrLn o . render . vcat . map mkInstance) decls
     hPutStrLn o "\n\n{-Done-}"


getDtd (Just dtd) = dtd
getDtd (Nothing)  = error "No DTD in this document"

trim name | '/' `elem` name  = (trim . tail . dropWhile (/='/')) name
          | '.' `elem` name  = takeWhile (/='.') name
          | otherwise        = name

