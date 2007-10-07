module Network.Hackage.CabalInstall.Utils where

import Distribution.Compat.ReadP (ReadP, readP_to_S)
import Distribution.ParseUtils
import Distribution.Verbosity
import Network.Hackage.CabalInstall.Types

import Control.Exception
import Control.Monad (foldM, guard)
import Data.Char (isSpace)
import Data.Maybe (listToMaybe)
import System.IO.Error (isDoesNotExistError)
import Text.PrettyPrint.HughesPJ


isVerbose cfg = configVerbose cfg >= verbose


readFileIfExists :: FilePath -> IO (Maybe String)
readFileIfExists path = 
    catchJust fileNotFoundExceptions 
                  (fmap Just (readFile path)) 
                  (\_ -> return Nothing)

fileNotFoundExceptions :: Exception -> Maybe IOError
fileNotFoundExceptions e = 
    ioErrors e >>= \ioe -> guard (isDoesNotExistError ioe) >> return ioe


showPError :: PError -> String
showPError err = let (ml,msg) = locatedErrorMsg err
                  in maybe "" (\l -> "On line " ++ show l ++ ": ") ml ++ msg



readPToMaybe :: ReadP r a -> String -> Maybe a
readPToMaybe p str = listToMaybe [ r | (r,s) <- readP_to_S p str, all isSpace s ]

ignoreWarnings :: ParseResult a -> ParseResult a
ignoreWarnings (ParseOk _ x) = ParseOk [] x
ignoreWarnings r = r 

parseBasicStanza :: [FieldDescr a] -> a -> String -> ParseResult a
parseBasicStanza fields empty inp = 
    readFields inp >>= foldM (setField fields) empty

setField :: [FieldDescr a]
         -> a
         -> Field
         -> ParseResult a
setField fs x (F line f val) =
    case lookupFieldDescr fs f of
      Nothing -> 
          do warning ("Unrecognized field " ++ f ++ " on line " ++ show line)
             return x
      Just (FieldDescr _ _ set) -> set line val x
setField _ x s = 
    do warning ("Unrecognized stanza on line " ++ show (lineNo s))
       return x

lookupFieldDescr :: [FieldDescr a] -> String -> Maybe (FieldDescr a)
lookupFieldDescr fs n = listToMaybe [f | f@(FieldDescr name _ _) <- fs, name == n]


showFields :: [FieldDescr a] -> a -> String
showFields fs x = render $ vcat [ text name <> text ":" <+> get x | FieldDescr name get _ <- fs]
