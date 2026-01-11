module Distribution.Compat.ResponseFile (expandResponse, escapeArgs) where

import Distribution.Compat.Prelude

import GHC.ResponseFile (escapeArgs, unescapeArgs)

import Prelude ()

import System.FilePath
import System.IO (hPutStrLn, stderr)
import System.IO.Error

-- | This is a more elaborate version of 'GHC.ResponseFile.expandResponse',
-- which not only substitute @\@foo@ with the contents of file @foo@,
-- but performs such substitution recursively.
-- In doing so we keep closer to the reference implementation
-- of @expandargv@ in @argv.c@ from @binutils@, although this additional functionality
-- likely remains unused by Haskell tooling.
expandResponse :: [String] -> IO [String]
expandResponse = go recursionLimit "."
  where
    recursionLimit = 100

    go :: Int -> FilePath -> [String] -> IO [String]
    go n dir
      | n >= 0 = fmap concat . traverse (expand n dir)
      | otherwise = const $ hPutStrLn stderr "Error: response file recursion limit exceeded." >> exitFailure

    expand :: Int -> FilePath -> String -> IO [String]
    expand n dir arg@('@' : f) = readRecursively n (dir </> f) `catchIOError` const (print "?" >> return [arg])
    expand _n _dir x = return [x]

    readRecursively :: Int -> FilePath -> IO [String]
    readRecursively n f = go (n - 1) (takeDirectory f) =<< unescapeArgs <$> readFile f
