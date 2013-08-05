{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Program.Ar
-- Copyright   :  Duncan Coutts 2009
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This module provides an library interface to the @ar@ program.

module Distribution.Simple.Program.Ar (
    createArLibArchive,
    multiStageProgramInvocation,
  ) where

import Control.Applicative ((<$>))
import Control.Exception (evaluate)
import Control.Monad (when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import Data.Char (isSpace)
import Distribution.Simple.Program.Types
         ( ConfiguredProgram(..) )
import Distribution.Simple.Program.Run
         ( programInvocation, multiStageProgramInvocation
         , runProgramInvocation )
import Distribution.System
         ( OS(..), buildOS )
import Distribution.Verbosity
         ( Verbosity, deafening, verbose )
import System.Directory
         ( renameFile, doesFileExist, removeFile )
import System.FilePath
         ( (<.>) )
import System.IO
         ( IOMode(ReadMode, ReadWriteMode), SeekMode(AbsoluteSeek)
         , hSeek, withBinaryFile, hFileSize )

-- | Call @ar@ to create a library archive from a bunch of object files.
--
createArLibArchive :: Verbosity -> ConfiguredProgram
                   -> FilePath -> [FilePath] -> IO ()
createArLibArchive verbosity ar target files = do

  -- The args to use with "ar" are actually rather subtle and system-dependent.
  -- In particular we have the following issues:
  --
  --  -- On OS X, "ar q" does not make an archive index. Archives with no
  --     index cannot be used.
  --
  --  -- GNU "ar r" will not let us add duplicate objects, only "ar q" lets us
  --     do that. We have duplicates because of modules like "A.M" and "B.M"
  --     both make an object file "M.o" and ar does not consider the directory.
  --
  -- Our solution is to use "ar r" in the simple case when one call is enough.
  -- When we need to call ar multiple times we use "ar q" and for the last
  -- call on OSX we use "ar qs" so that it'll make the index.
  --
  -- "ar" by default writes file modification time stamps, which would
  -- generates different outputs for same inputs and breaks re-linking
  -- avoidance. We set these time stamps to 0 ourselves.
  --
  -- If there is an old target file and the are produces the very same output,
  -- we avoid touching the old target file to help tools like GHC and make
  -- exiting early.

  let simpleArgs  = case buildOS of
             OSX -> ["-r", "-s"]
             _   -> ["-r"]

      initialArgs = ["-q"]
      finalArgs   = case buildOS of
             OSX -> ["-q", "-s"]
             _   -> ["-q"]

      tmpTarget   = target <.> "tmp"

      extraArgs   = verbosityOpts verbosity ++ [tmpTarget]

      simple  = programInvocation ar (simpleArgs  ++ extraArgs)
      initial = programInvocation ar (initialArgs ++ extraArgs)
      middle  = initial
      final   = programInvocation ar (finalArgs   ++ extraArgs)

  -- Delete old .a.tmp file (we use -r, which fails if the file is malformed)
  tmpExists <- doesFileExist tmpTarget
  when tmpExists $ removeFile tmpTarget

  sequence_
    [ runProgramInvocation verbosity inv
    | inv <- multiStageProgramInvocation
               simple (initial, middle, final) files ]

  -- If this "ar" invocation has actually created something new,
  -- copy the temporary file to the target.

  -- First wipe off the timestamp from the temporary .a archive.
  -- We could use "ar -D", but many platforms don't support that.
  arFileWipeTimeStamps tmpTarget

  writeTarget <- do
    oldExists <- doesFileExist target
    if not oldExists then return True
                     else not <$> filesEqual target tmpTarget

  when writeTarget $ renameFile tmpTarget target

  where
    verbosityOpts v | v >= deafening = ["-v"]
                    | v >= verbose   = []
                    | otherwise      = ["-c"]


-- | Compares two files for equality.
-- Uses lazy ByteStrings to not load them into memory.
filesEqual :: FilePath -> FilePath -> IO Bool
filesEqual f1 f2 =
  withBinaryFile f1 ReadMode $ \h1 ->
    withBinaryFile f2 ReadMode $ \h2 -> do
      c1 <- BSL.hGetContents h1
      c2 <- BSL.hGetContents h2
      evaluate (c1 == c2)


-- | Removes the time stamps of all files in the .a file.
arFileWipeTimeStamps :: FilePath -> IO ()
arFileWipeTimeStamps path = do

  -- Check for file existence (ReadWriteMode would create one otherwise)
  exsists <- doesFileExist path
  when (not exsists) $ error $ "arFileWipeTimeStamps: No such file: " ++ path

  withBinaryFile path ReadWriteMode $ \h -> do

    -- We iterate through the archive stepping from one file header to the next,
    -- setting the time stamp field to zero.
    -- The size field tells us where the next header is.
    -- See: http://en.wikipedia.org/wiki/Ar_%28Unix%29.

    archiveSize <- hFileSize h

    let go entryOffset | entryOffset == archiveSize = return () -- done, at end
                       | entryOffset >  archiveSize = die "Archive truncated"
                       -- Headers are aligned to even bytes
                       | odd entryOffset            = go (entryOffset + 1)
                       | otherwise = do

          -- Sanity check
          magic <- goto 58 >> BS.hGet h 2
          when (magic /= "\x60\x0a") $ die "Bad ar magic"

          -- Get size (to find following file)
          size <- goto 48 >> parseSize . BS8.unpack <$> BS.hGet h 10

          -- Wipe time stamp
          goto 16 >> BS.hPut h "0           " -- 12 chars

          -- Seek to next file at header + file size
          go (entryOffset + 60 + size)

          where
            goto pos = hSeek h AbsoluteSeek (entryOffset + pos)

            parseSize x = case reads x of
              [(s, r)] | all isSpace r -> s
              _                        -> die "Malformed header"

            die msg = error $ "arFileWipeTimeStamps: " ++ path ++ ": "
                              ++ msg ++ " at offset " ++ show entryOffset

    go 8 -- 8 == size of global header, before first file header
