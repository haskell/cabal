module Hackage.Utils where

import Distribution.Package (Dependency(..), showDependency)
import Distribution.Simple.Utils (intercalate)

import Control.Monad (guard)
import Control.Exception (Exception, catchJust, ioErrors)
import System.IO.Error (isDoesNotExistError)

readFileIfExists :: FilePath -> IO (Maybe String)
readFileIfExists path =
    catchJust fileNotFoundExceptions 
                  (fmap Just (readFile path))
                  (\_ -> return Nothing)

fileNotFoundExceptions :: Exception -> Maybe IOError
fileNotFoundExceptions e = 
    ioErrors e >>= \ioe -> guard (isDoesNotExistError ioe) >> return ioe

showDependencies :: [Dependency] -> String
showDependencies = intercalate ", " . map (show . showDependency)
