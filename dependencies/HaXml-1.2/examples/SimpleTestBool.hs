module Main where

import List (isPrefixOf)
import Text.XML.HaXml.Haskell2Xml
import Text.XML.HaXml.Types
import Text.PrettyPrint.HughesPJ (render)
import Text.XMl.HaXml.Pretty     (document)

-- Test stuff
--value1 :: ([(Bool,Int)],(String,Maybe Char))
value1 = True

--main = do (putStrLn . render . document . toXml) value2

main = writeXml "/dev/tty" value1
        
