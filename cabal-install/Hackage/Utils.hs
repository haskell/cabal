module Hackage.Utils where

import Distribution.ParseUtils (showDependency)
import Distribution.Version (Dependency(..))
import Distribution.Simple.Utils (intercalate, readTextFile)

import Control.Monad (guard)
import Control.Exception (Exception, catchJust, ioErrors)
import System.IO.Error (isDoesNotExistError)

readTextFileIfExists :: FilePath -> IO (Maybe String)
readTextFileIfExists path =
    catchJust fileNotFoundExceptions 
                  (fmap Just (readTextFile path))
                  (\_ -> return Nothing)

fileNotFoundExceptions :: Exception -> Maybe IOError
fileNotFoundExceptions e = 
    ioErrors e >>= \ioe -> guard (isDoesNotExistError ioe) >> return ioe

showDependencies :: [Dependency] -> String
showDependencies = intercalate ", " . map (show . showDependency)
