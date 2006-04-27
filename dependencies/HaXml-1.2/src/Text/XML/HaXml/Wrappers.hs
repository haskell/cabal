module Text.XML.HaXml.Wrappers
  ( fix2Args
  , processXmlWith
  ) where

-- imports required for processXmlWith and fix2Args
import System
import System.IO
import Data.List (isSuffixOf)

import Text.XML.HaXml.Types       (Document(..),Content(..))
import Text.XML.HaXml.Combinators (CFilter)
import Text.XML.HaXml.Parse       (xmlParse)
import Text.XML.HaXml.Html.Parse  (htmlParse)
import Text.XML.HaXml.Pretty      (element)
import Text.PrettyPrint.HughesPJ  (render)


-- | This useful auxiliary checks the commandline arguments for two
--   filenames, the input and output file respectively.  If either
--   is missing, it is replaced by '-', which can be interpreted by the
--   caller as stdin and\/or stdout.
fix2Args :: IO (String,String)
fix2Args = do
  args <- getArgs
  case length args of
    0 -> return ("-",     "-")
    1 -> return (args!!0, "-")
    2 -> return (args!!0, args!!1)
    _ -> do prog <- getProgName
            putStrLn ("Usage: "++prog++" [xmlfile] [outfile]")
            exitFailure


-- | The wrapper @processXmlWith@ returns an IO () computation
--   that collects the filenames (or stdin\/stdout) to use when
--   reading\/writing XML documents.  Its CFilter argument
--   is applied to transform the XML document from the input and
--   write it to the output.  No DTD is attached to the output.
--
--   If the input filename ends with .html or .htm, it is parsed using
--   the error-correcting HTML parser rather than the strict XML parser.
processXmlWith :: CFilter -> IO ()
processXmlWith f = do
  (inf,outf) <- fix2Args
  input      <- if inf=="-" then getContents else readFile inf
  o          <- if outf=="-" then return stdout else openFile outf WriteMode
  parse      <- if ".html" `isSuffixOf` inf || ".htm" `isSuffixOf` inf
                then return (htmlParse inf)
                else do hPutStrLn o "<?xml version='1.0'?>"
                        return (xmlParse inf)
  ( hPutStrLn o . render . ppContent . f . getContent . parse) input

  where
    getContent (Document _ _ e) = CElem e

    ppContent [CElem e] = element e
    ppContent []  = error "produced no output"
    ppContent _   = error "produced more than one output"

