{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Utils
-- Copyright   :  Isaac Jones, Simon Marlow 2003-2004
-- License     :  BSD3
--                portions Copyright (c) 2007, Galois Inc.
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- A large and somewhat miscellaneous collection of utility functions used
-- throughout the rest of the Cabal lib and in other tools that use the Cabal
-- lib like @cabal-install@. It has a very simple set of logging actions. It
-- has low level functions for running programs, a bunch of wrappers for
-- various directory and file functions that do extra logging.

module Distribution.Utils.Generic (
        -- * reading and writing files safely
        withFileContents,
        writeFileAtomic,

        -- * Unicode
        fromUTF8,
        fromUTF8BS,
        fromUTF8LBS,
        toUTF8,
        readUTF8File,
        withUTF8FileContents,
        writeUTF8File,
        normaliseLineEndings,

        -- * BOM
        startsWithBOM,
        fileHasBOM,
        ignoreBOM,

        -- * generic utils
        dropWhileEndLE,
        takeWhileEndLE,
        equating,
        comparing,
        isInfixOf,
        intercalate,
        lowercase,
        listUnion,
        listUnionRight,
        ordNub,
        ordNubRight,
        safeTail,
        unintersperse,
        wrapText,
        wrapLine,

        -- * FilePath stuff
        isAbsoluteOnAnyPlatform,
        isRelativeOnAnyPlatform,
  ) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Utils.String

import Data.Bits
    ( Bits((.|.), (.&.), shiftL, shiftR) )
import Data.List
    ( isInfixOf )
import Data.Ord
    ( comparing )
import qualified Data.ByteString.Lazy as BS
import qualified Data.Set as Set

import qualified Data.ByteString as SBS

import System.Directory
    ( removeFile )
import System.FilePath
    ( (<.>), splitFileName )
import System.Directory
    ( renameFile )
import System.IO
    ( openFile, openBinaryFile, openBinaryTempFileWithDefaultPermissions
    , IOMode(ReadMode), hGetContents, hClose )
import qualified Control.Exception as Exception

-- -----------------------------------------------------------------------------
-- Helper functions

-- | Wraps text to the default line width. Existing newlines are preserved.
wrapText :: String -> String
wrapText = unlines
         . map (intercalate "\n"
              . map unwords
              . wrapLine 79
              . words)
         . lines

-- | Wraps a list of words to a list of lines of words of a particular width.
wrapLine :: Int -> [String] -> [[String]]
wrapLine width = wrap 0 []
  where wrap :: Int -> [String] -> [String] -> [[String]]
        wrap 0   []   (w:ws)
          | length w + 1 > width
          = wrap (length w) [w] ws
        wrap col line (w:ws)
          | col + length w + 1 > width
          = reverse line : wrap 0 [] (w:ws)
        wrap col line (w:ws)
          = let col' = col + length w + 1
             in wrap col' (w:line) ws
        wrap _ []   [] = []
        wrap _ line [] = [reverse line]

-----------------------------------
-- Safely reading and writing files

-- | Gets the contents of a file, but guarantee that it gets closed.
--
-- The file is read lazily but if it is not fully consumed by the action then
-- the remaining input is truncated and the file is closed.
--
withFileContents :: FilePath -> (String -> NoCallStackIO a) -> NoCallStackIO a
withFileContents name action =
  Exception.bracket (openFile name ReadMode) hClose
                    (\hnd -> hGetContents hnd >>= action)

-- | Writes a file atomically.
--
-- The file is either written successfully or an IO exception is raised and
-- the original file is left unchanged.
--
-- On windows it is not possible to delete a file that is open by a process.
-- This case will give an IO exception but the atomic property is not affected.
--
writeFileAtomic :: FilePath -> BS.ByteString -> NoCallStackIO ()
writeFileAtomic targetPath content = do
  let (targetDir, targetFile) = splitFileName targetPath
  Exception.bracketOnError
    (openBinaryTempFileWithDefaultPermissions targetDir $ targetFile <.> "tmp")
    (\(tmpPath, handle) -> hClose handle >> removeFile tmpPath)
    (\(tmpPath, handle) -> do
        BS.hPut handle content
        hClose handle
        renameFile tmpPath targetPath)

-- ------------------------------------------------------------
-- * Unicode stuff
-- ------------------------------------------------------------

-- This is a modification of the UTF8 code from gtk2hs and the
-- utf8-string package.

fromUTF8 :: String -> String
fromUTF8 []     = []
fromUTF8 (c:cs)
  | c <= '\x7F' = c : fromUTF8 cs
  | c <= '\xBF' = replacementChar : fromUTF8 cs
  | c <= '\xDF' = twoBytes c cs
  | c <= '\xEF' = moreBytes 3 0x800     cs (ord c .&. 0xF)
  | c <= '\xF7' = moreBytes 4 0x10000   cs (ord c .&. 0x7)
  | c <= '\xFB' = moreBytes 5 0x200000  cs (ord c .&. 0x3)
  | c <= '\xFD' = moreBytes 6 0x4000000 cs (ord c .&. 0x1)
  | otherwise   = replacementChar : fromUTF8 cs
  where
    twoBytes c0 (c1:cs')
      | ord c1 .&. 0xC0 == 0x80
      = let d = ((ord c0 .&. 0x1F) `shiftL` 6)
             .|. (ord c1 .&. 0x3F)
         in if d >= 0x80
               then  chr d           : fromUTF8 cs'
               else  replacementChar : fromUTF8 cs'
    twoBytes _ cs' = replacementChar : fromUTF8 cs'

    moreBytes :: Int -> Int -> [Char] -> Int -> [Char]
    moreBytes 1 overlong cs' acc
      | overlong <= acc && acc <= 0x10FFFF
     && (acc < 0xD800 || 0xDFFF < acc)
     && (acc < 0xFFFE || 0xFFFF < acc)
      = chr acc : fromUTF8 cs'

      | otherwise
      = replacementChar : fromUTF8 cs'

    moreBytes byteCount overlong (cn:cs') acc
      | ord cn .&. 0xC0 == 0x80
      = moreBytes (byteCount-1) overlong cs'
          ((acc `shiftL` 6) .|. ord cn .&. 0x3F)

    moreBytes _ _ cs' _
      = replacementChar : fromUTF8 cs'

    replacementChar = '\xfffd'

fromUTF8BS :: SBS.ByteString -> String
fromUTF8BS = decodeStringUtf8 . SBS.unpack

fromUTF8LBS :: BS.ByteString -> String
fromUTF8LBS = decodeStringUtf8 . BS.unpack

toUTF8 :: String -> String
toUTF8 []        = []
toUTF8 (c:cs)
  | c <= '\x07F' = c
                 : toUTF8 cs
  | c <= '\x7FF' = chr (0xC0 .|. (w `shiftR` 6))
                 : chr (0x80 .|. (w .&. 0x3F))
                 : toUTF8 cs
  | c <= '\xFFFF'= chr (0xE0 .|.  (w `shiftR` 12))
                 : chr (0x80 .|. ((w `shiftR` 6)  .&. 0x3F))
                 : chr (0x80 .|.  (w .&. 0x3F))
                 : toUTF8 cs
  | otherwise    = chr (0xf0 .|.  (w `shiftR` 18))
                 : chr (0x80 .|. ((w `shiftR` 12)  .&. 0x3F))
                 : chr (0x80 .|. ((w `shiftR` 6)  .&. 0x3F))
                 : chr (0x80 .|.  (w .&. 0x3F))
                 : toUTF8 cs
  where w = ord c

-- | Whether BOM is at the beginning of the input
startsWithBOM :: String -> Bool
startsWithBOM ('\xFEFF':_) = True
startsWithBOM _            = False

-- | Check whether a file has Unicode byte order mark (BOM).
fileHasBOM :: FilePath -> NoCallStackIO Bool
fileHasBOM f = fmap (startsWithBOM . fromUTF8)
             . hGetContents =<< openBinaryFile f ReadMode

-- | Ignore a Unicode byte order mark (BOM) at the beginning of the input
--
ignoreBOM :: String -> String
ignoreBOM ('\xFEFF':string) = string
ignoreBOM string            = string

-- | Reads a UTF8 encoded text file as a Unicode String
--
-- Reads lazily using ordinary 'readFile'.
--
readUTF8File :: FilePath -> NoCallStackIO String
readUTF8File f = fmap (ignoreBOM . fromUTF8)
               . hGetContents =<< openBinaryFile f ReadMode

-- | Reads a UTF8 encoded text file as a Unicode String
--
-- Same behaviour as 'withFileContents'.
--
withUTF8FileContents :: FilePath -> (String -> IO a) -> IO a
withUTF8FileContents name action =
  Exception.bracket
    (openBinaryFile name ReadMode)
    hClose
    (\hnd -> hGetContents hnd >>= action . ignoreBOM . fromUTF8)

-- | Writes a Unicode String as a UTF8 encoded text file.
--
-- Uses 'writeFileAtomic', so provides the same guarantees.
--
writeUTF8File :: FilePath -> String -> NoCallStackIO ()
writeUTF8File path = writeFileAtomic path . BS.pack . encodeStringUtf8

-- | Fix different systems silly line ending conventions
normaliseLineEndings :: String -> String
normaliseLineEndings [] = []
normaliseLineEndings ('\r':'\n':s) = '\n' : normaliseLineEndings s -- windows
normaliseLineEndings ('\r':s)      = '\n' : normaliseLineEndings s -- old OS X
normaliseLineEndings (  c :s)      =   c  : normaliseLineEndings s

-- ------------------------------------------------------------
-- * Common utils
-- ------------------------------------------------------------

-- | @dropWhileEndLE p@ is equivalent to @reverse . dropWhile p . reverse@, but
-- quite a bit faster. The difference between "Data.List.dropWhileEnd" and this
-- version is that the one in "Data.List" is strict in elements, but spine-lazy,
-- while this one is spine-strict but lazy in elements. That's what @LE@ stands
-- for - "lazy in elements".
--
-- Example:
--
-- @
-- > tail $ Data.List.dropWhileEnd (<3) [undefined, 5, 4, 3, 2, 1]
-- *** Exception: Prelude.undefined
-- > tail $ dropWhileEndLE (<3) [undefined, 5, 4, 3, 2, 1]
-- [5,4,3]
-- > take 3 $ Data.List.dropWhileEnd (<3) [5, 4, 3, 2, 1, undefined]
-- [5,4,3]
-- > take 3 $ dropWhileEndLE (<3) [5, 4, 3, 2, 1, undefined]
-- *** Exception: Prelude.undefined
-- @
dropWhileEndLE :: (a -> Bool) -> [a] -> [a]
dropWhileEndLE p = foldr (\x r -> if null r && p x then [] else x:r) []

-- | @takeWhileEndLE p@ is equivalent to @reverse . takeWhile p . reverse@, but
-- is usually faster (as well as being easier to read).
takeWhileEndLE :: (a -> Bool) -> [a] -> [a]
takeWhileEndLE p = fst . foldr go ([], False)
  where
    go x (rest, done)
      | not done && p x = (x:rest, False)
      | otherwise = (rest, True)

-- | Like "Data.List.nub", but has @O(n log n)@ complexity instead of
-- @O(n^2)@. Code for 'ordNub' and 'listUnion' taken from Niklas Hambüchen's
-- <http://github.com/nh2/haskell-ordnub ordnub> package.
ordNub :: (Ord a) => [a] -> [a]
ordNub l = go Set.empty l
  where
    go _ [] = []
    go s (x:xs) = if x `Set.member` s then go s xs
                                      else x : go (Set.insert x s) xs

-- | Like "Data.List.union", but has @O(n log n)@ complexity instead of
-- @O(n^2)@.
listUnion :: (Ord a) => [a] -> [a] -> [a]
listUnion a b = a ++ ordNub (filter (`Set.notMember` aSet) b)
  where
    aSet = Set.fromList a

-- | A right-biased version of 'ordNub'.
--
-- Example:
--
-- @
-- > ordNub [1,2,1]
-- [1,2]
-- > ordNubRight [1,2,1]
-- [2,1]
-- @
ordNubRight :: (Ord a) => [a] -> [a]
ordNubRight = fst . foldr go ([], Set.empty)
  where
    go x p@(l, s) = if x `Set.member` s then p
                                        else (x:l, Set.insert x s)

-- | A right-biased version of 'listUnion'.
--
-- Example:
--
-- @
-- > listUnion [1,2,3,4,3] [2,1,1]
-- [1,2,3,4,3]
-- > listUnionRight [1,2,3,4,3] [2,1,1]
-- [4,3,2,1,1]
-- @
listUnionRight :: (Ord a) => [a] -> [a] -> [a]
listUnionRight a b = ordNubRight (filter (`Set.notMember` bSet) a) ++ b
  where
    bSet = Set.fromList b

-- | A total variant of 'tail'.
safeTail :: [a] -> [a]
safeTail []     = []
safeTail (_:xs) = xs

equating :: Eq a => (b -> a) -> b -> b -> Bool
equating p x y = p x == p y

lowercase :: String -> String
lowercase = map toLower

unintersperse :: Char -> String -> [String]
unintersperse mark = unfoldr unintersperse1 where
  unintersperse1 str
    | null str = Nothing
    | otherwise =
        let (this, rest) = break (== mark) str in
        Just (this, safeTail rest)

-- ------------------------------------------------------------
-- * FilePath stuff
-- ------------------------------------------------------------

-- | 'isAbsoluteOnAnyPlatform' and 'isRelativeOnAnyPlatform' are like
-- 'System.FilePath.isAbsolute' and 'System.FilePath.isRelative' but have
-- platform independent heuristics.
-- The System.FilePath exists in two versions, Windows and Posix. The two
-- versions don't agree on what is a relative path and we don't know if we're
-- given Windows or Posix paths.
-- This results in false positives when running on Posix and inspecting
-- Windows paths, like the hackage server does.
-- System.FilePath.Posix.isAbsolute \"C:\\hello\" == False
-- System.FilePath.Windows.isAbsolute \"/hello\" == False
-- This means that we would treat paths that start with \"/\" to be absolute.
-- On Posix they are indeed absolute, while on Windows they are not.
--
-- The portable versions should be used when we might deal with paths that
-- are from another OS than the host OS. For example, the Hackage Server
-- deals with both Windows and Posix paths while performing the
-- PackageDescription checks. In contrast, when we run 'cabal configure' we
-- do expect the paths to be correct for our OS and we should not have to use
-- the platform independent heuristics.
isAbsoluteOnAnyPlatform :: FilePath -> Bool
-- C:\\directory
isAbsoluteOnAnyPlatform (drive:':':'\\':_) = isAlpha drive
-- UNC
isAbsoluteOnAnyPlatform ('\\':'\\':_) = True
-- Posix root
isAbsoluteOnAnyPlatform ('/':_) = True
isAbsoluteOnAnyPlatform _ = False

-- | @isRelativeOnAnyPlatform = not . 'isAbsoluteOnAnyPlatform'@
isRelativeOnAnyPlatform :: FilePath -> Bool
isRelativeOnAnyPlatform = not . isAbsoluteOnAnyPlatform
