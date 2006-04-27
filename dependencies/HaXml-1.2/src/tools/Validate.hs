module Main where

import System (getArgs)
import System.IO
import Data.List   (isSuffixOf)
import Data.Maybe  (fromJust)

import Text.XML.HaXml.Types     (Document(..),Content(..))
import Text.XML.HaXml.Parse     (xmlParse,dtdParse)
import Text.XML.HaXml.Validate  (validate)
import Text.XML.HaXml.Wrappers  (fix2Args)

-- This is a fairly trivial application that reads a DTD from a file,
-- an XML document from another file (or stdin), and writes any validation
-- errors to stdout.

main = do
  (dtdf,xmlf) <- fix2Args
  dtdtext     <- ( if dtdf=="-" then error "Usage: validate dtdfile [xmlfile]"
                   else readFile dtdf )
  content     <- ( if xmlf=="-" then getContents else readFile xmlf )
  let dtd  = dtdParse dtdf dtdtext
      Document _ _ xml  = xmlParse xmlf content
      errs = validate (fromJust dtd) xml
  mapM_ putStrLn errs

