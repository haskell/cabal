-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Hackage.CabalInstall.TarUtils
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Utility functions for manipulating tar archives.
-----------------------------------------------------------------------------
module Network.Hackage.CabalInstall.TarUtils
    ( tarballGetFiles
    , locateFile
    , locateFileExt
    , extractTarFile
    ) where

import Distribution.Compat.FilePath (splitFileName, splitFileExt, breakFilePath)
import System.IO (hClose, hGetContents)
import System.Process (runInteractiveProcess, runProcess, waitForProcess)
import System.Exit (ExitCode(..))
import Text.Printf (printf)
import Data.List (find, sortBy)
import Data.Maybe (listToMaybe)


-- |List the files in a gzipped tar archive. Throwing an exception on failure.
tarballGetFiles :: FilePath -- ^Path to the 'tar' binary.
                -> FilePath -- ^Path to the .tgz archive.
                -> IO [FilePath]
tarballGetFiles tarProg tarFile
    = do (inch,out,_,handle) <- runInteractiveProcess tarProg args Nothing Nothing
         hClose inch
         files <- hGetContents out
         length files `seq` hClose out
         eCode <- waitForProcess handle
         case eCode of
           ExitFailure err -> error $ printf "Failed to get filelist from '%s': %s." tarFile (show err)
           _ -> return (lines files)
    where args = ["--list"
                 ,"--gunzip"
                 ,"--file"
                 ,tarFile]

{-|
Find a file in a given directory.

@
 locateFile [\"somedir\/jalla.txt\"] \"somedir\" [\"jalla.txt\"]
     => Just \"somedir\/jalla.txt\"
 locateFile [\"somepkg\/pkg.cabal\", \"somepkg\/Setup.hs\"] \"somepkg\" [\"Setup.lhs\", \"Setup.hs\"]
     => Just \"somedir\/Setup.hs\"
@
-}
locateFile :: [FilePath] -- ^File list.
           -> FilePath   -- ^Base directory.
           -> [FilePath] -- ^List of filenames to locate.
           -> Maybe FilePath
locateFile files dir names
    = find findFile files
    where findFile file
              = let (root,name) = splitFileName file
                in root == dir && name `elem` names

{-|
Locate all files with a given extension and return the shortest result.

@
  locateFileExt [\"somedir\/test.cabal\"] \"cabal\"
     => Just \"somedir\/test.cabal\"
@
-}
locateFileExt :: [FilePath] -> String -> Maybe FilePath
locateFileExt files fileExt
    = let okExts = filter (\f -> let (_,ext) = splitFileExt f
                                    in ext == fileExt) files
      in (listToMaybe (sortBy sortFn okExts))
    where comparing f a b = f a `compare` f b
          sortFn = comparing (length.breakFilePath)

-- |Extract a given archive in the directory where it's placed.
extractTarFile :: FilePath -- ^Path to the 'tar' binary.
               -> FilePath -- ^Path to the .tgz archive.
               -> IO ()
extractTarFile tarProg tarFile
    = do tarHandle <- runProcess tarProg args (Just dir) Nothing Nothing Nothing Nothing
         eCode  <- waitForProcess tarHandle
         case eCode of
           ExitFailure err -> error $ printf "Failed to extract tar file '%s': %s." tarFile (show err)
           _ -> return ()
    where args = ["-xzf",tarFile]
          (dir,_) = splitFileName tarFile
