{-# LANGUAGE CPP, ForeignFunctionInterface #-}
{-# OPTIONS_GHC -cpp -fffi #-}
{-# OPTIONS_NHC98 -cpp #-}
{-# OPTIONS_JHC -fcpp -fffi #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Utils
-- Copyright   :  Isaac Jones, Simon Marlow 2003-2004
-- 
-- Maintainer  :  Isaac Jones <ijones@syntaxpolice.org>
-- Stability   :  alpha
-- Portability :  portable
--
-- Explanation: Misc. Utilities, especially file-related utilities.
-- Stuff used by multiple modules that doesn't fit elsewhere.

{- All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Isaac Jones nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. -}

module Distribution.Simple.Utils (
        cabalVersion,

        -- * logging and errors
        die,
        dieWithLocation,
        warn, notice, setupMessage, info, debug,
        chattyTry,
        breaks,

        -- * running programs
        rawSystemExit,
        rawSystemStdout,
	rawSystemStdout',
        maybeExit,
        xargs,

        -- * copying files
        smartCopySources,
        createDirectoryIfMissingVerbose,
        copyFileVerbose,
        copyDirectoryRecursiveVerbose,
        copyFiles,

        -- * file names
        currentDir,
        dotToSep,

        -- * finding files
	findFile,
        findFileWithExtension,
        findFileWithExtension',

        -- * files
        withTempFile,
        writeFileAtomic,

        -- * .cabal and .buildinfo files
        defaultPackageDesc,
        findPackageDesc,
	defaultHookedPackageDesc,
	findHookedPackageDesc,

        -- * Unicode
        fromUTF8,
        toUTF8,
        readTextFile,
        writeTextFile,

        -- * generic utils
        equating,
        comparing,
        isInfixOf,
        intercalate,
        lowercase,
        wrapText,
  ) where

import Control.Monad
    ( when, unless )
import Data.List
    ( nub, unfoldr, isPrefixOf, tails, intersperse )
import Data.Char as Char
    ( toLower, chr, ord )
import Data.Bits
    ( (.|.), (.&.), shiftL, shiftR )

import System.Directory
    ( getDirectoryContents, getCurrentDirectory, doesDirectoryExist
    , doesFileExist, removeFile )
import System.Environment
    ( getProgName )
import System.Cmd
    ( rawSystem )
import System.Exit
    ( exitWith, ExitCode(..) )
import System.FilePath
    ( takeDirectory, splitFileName, takeExtension, (</>), (<.>), pathSeparator )
import System.Directory
    ( copyFile, createDirectoryIfMissing, renameFile )
import System.IO
    ( hPutStrLn, hPutStr, hClose  , stderr, hFlush, stdout )
import System.IO.Error as IO.Error
    ( try )
import qualified Control.Exception as Exception
    ( bracket, bracketOnError, catch )

import Distribution.Package
    (PackageIdentifier(..), showPackageId)
import Distribution.Version
    (Version(..))

#ifdef __GLASGOW_HASKELL__
import Control.Concurrent (forkIO)
import Control.Exception (evaluate)
import System.Process (runInteractiveProcess, waitForProcess)
import System.IO (hGetContents)
#else
import System.Cmd (system)
import System.Directory (getTemporaryDirectory)
#endif
import System.IO (Handle)

import Distribution.Compat.TempFile (openTempFile)
import Distribution.Verbosity

-- We only get our own version number when we're building with ourselves
cabalVersion :: Version
#ifdef CABAL_VERSION
cabalVersion = Version [CABAL_VERSION] []
#else
cabalVersion = error "Cabal was not bootstrapped correctly"
#endif

-- ------------------------------------------------------------------------------- Utils for setup

dieWithLocation :: FilePath -> (Maybe Int) -> String -> IO a
dieWithLocation fname Nothing msg = die (fname ++ ": " ++ msg)
dieWithLocation fname (Just n) msg = die (fname ++ ":" ++ show n ++ ": " ++ msg)

die :: String -> IO a
die msg = do
  hFlush stdout
  pname <- getProgName
  hPutStrLn stderr $ toUTF8 (pname ++ ": " ++ msg)
  exitWith (ExitFailure 1)

-- | Non fatal conditions that may be indicative of an error or problem.
--
-- We display these at the 'normal' verbosity level.
--
warn :: Verbosity -> String -> IO ()
warn verbosity msg = 
  when (verbosity >= normal) $ do
    hFlush stdout
    hPutStrLn stderr ("Warning: " ++ toUTF8 msg)

-- | Useful status messages.
--
-- We display these at the 'normal' verbosity level.
--
-- This is for the ordinary helpful status messages that users see. Just
-- enough information to know that things are working but not floods of detail.
--
notice :: Verbosity -> String -> IO ()
notice verbosity msg =
  when (verbosity >= normal) $
    putStrLn (toUTF8 msg)

setupMessage :: Verbosity -> String -> PackageIdentifier -> IO ()
setupMessage verbosity msg pkgid =
    notice verbosity (msg ++ ' ':showPackageId pkgid ++ "...")

-- | More detail on the operation of some action.
-- 
-- We display these messages when the verbosity level is 'verbose'
--
info :: Verbosity -> String -> IO ()
info verbosity msg =
  when (verbosity >= verbose) $
    putStrLn (toUTF8 msg)

-- | Detailed internal debugging information
--
-- We display these messages when the verbosity level is 'deafening'
--
debug :: Verbosity -> String -> IO ()
debug verbosity msg =
  when (verbosity >= deafening) $
    putStrLn (toUTF8 msg)

-- | Perform an IO action, catching any IO exceptions and printing an error
--   if one occurs.
chattyTry :: String  -- ^ a description of the action we were attempting
          -> IO ()   -- ^ the action itself
          -> IO ()
chattyTry desc action =
  Exception.catch action $ \exception ->
    putStrLn $ toUTF8 $ "Error while " ++ desc ++ ": " ++ show exception

-- -----------------------------------------------------------------------------
-- Helper functions

breaks :: (a -> Bool) -> [a] -> [[a]]
breaks _ [] = []
breaks f xs = case span f xs of
                  (_, xs') ->
                      case break f xs' of
                          (v, xs'') ->
                              v : breaks f xs''

-- Wraps a list of words text to a list of lines of a particular width.
wrapText :: Int -> [String] -> [String]
wrapText width = map unwords . wrap 0 []
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

-- -----------------------------------------------------------------------------
-- rawSystem variants
maybeExit :: IO ExitCode -> IO ()
maybeExit cmd = do
  res <- cmd
  unless (res == ExitSuccess) $ exitWith res

printRawCommandAndArgs :: Verbosity -> FilePath -> [String] -> IO ()
printRawCommandAndArgs verbosity path args
 | verbosity >= deafening = print (path, args)
 | verbosity >= verbose   = putStrLn $ unwords (path : args)
 | otherwise              = return ()

-- Exit with the same exitcode if the subcommand fails
rawSystemExit :: Verbosity -> FilePath -> [String] -> IO ()
rawSystemExit verbosity path args = do
  printRawCommandAndArgs verbosity path args
  hFlush stdout
  maybeExit $ rawSystem path args

-- | Run a command and return its output.
--
-- The output is assumed to be encoded as UTF8.
--
rawSystemStdout :: Verbosity -> FilePath -> [String] -> IO String
rawSystemStdout verbosity path args = do
  (output, exitCode) <- rawSystemStdout' verbosity path args
  unless (exitCode == ExitSuccess) $ exitWith exitCode
  return output

rawSystemStdout' :: Verbosity -> FilePath -> [String] -> IO (String, ExitCode)
rawSystemStdout' verbosity path args = do
  printRawCommandAndArgs verbosity path args

#ifdef __GLASGOW_HASKELL__
  Exception.bracket
     (runInteractiveProcess path args Nothing Nothing)
     (\(inh,outh,errh,_) -> hClose inh >> hClose outh >> hClose errh)
    $ \(_,outh,errh,pid) -> do

      -- fork off a thread to pull on (and discard) the stderr
      -- so if the process writes to stderr we do not block.
      -- NB. do the hGetContents synchronously, otherwise the outer
      -- bracket can exit before this thread has run, and hGetContents
      -- will fail.
      err <- hGetContents errh 
      forkIO $ do evaluate (length err); return ()

      -- wait for all the output
      output <- fromUTF8 `fmap` hGetContents outh
      evaluate (length output)

      -- wait for the program to terminate
      exitcode <- waitForProcess pid

      return (output, exitcode)
#else
  tmpDir <- getTemporaryDirectory
  withTempFile tmpDir ".cmd.stdout" $ \tmpName tmpHandle -> do
    hClose tmpHandle
    let quote name = "'" ++ name ++ "'"
    exitCode <- system $ unwords (map quote (path:args)) ++ " >" ++ quote tmpName
    output <- readTextFile tmpName
    length output `seq` return (output, exitCode)
#endif

-- | Like the unix xargs program. Useful for when we've got very long command
-- lines that might overflow an OS limit on command line length and so you
-- need to invoke a command multiple times to get all the args in.
--
-- Use it with either of the rawSystem variants above. For example:
-- 
-- > xargs (32*1024) (rawSystemExit verbosity) prog fixedArgs bigArgs
--
xargs :: Int -> ([String] -> IO ())
      -> [String] -> [String] -> IO ()
xargs maxSize rawSystemFun fixedArgs bigArgs =
  let fixedArgSize = sum (map length fixedArgs) + length fixedArgs
      chunkSize = maxSize - fixedArgSize
   in mapM_ (rawSystemFun . (fixedArgs ++)) (chunks chunkSize bigArgs)

  where chunks len = unfoldr $ \s ->
          if null s then Nothing
                    else Just (chunk [] len s)

        chunk acc _   []     = (reverse acc,[])
        chunk acc len (s:ss)
          | len' < len = chunk (s:acc) (len-len'-1) ss
          | otherwise  = (reverse acc, s:ss)
          where len' = length s

-- ------------------------------------------------------------
-- * File Utilities
-- ------------------------------------------------------------

findFile :: [FilePath]    -- ^search locations
         -> FilePath      -- ^File Name
         -> IO FilePath
findFile searchPath fileName =
  findFirstFile id
    [ path </> fileName
    | path <- nub searchPath]
  >>= maybe (die $ fileName ++ " doesn't exist") return

findFileWithExtension :: [String]
                      -> [FilePath]
                      -> FilePath
                      -> IO (Maybe FilePath)
findFileWithExtension extensions searchPath baseName =
  findFirstFile id
    [ path </> baseName <.> ext
    | path <- nub searchPath
    , ext <- nub extensions ]

findFileWithExtension' :: [String]
                       -> [FilePath]
                       -> FilePath
                       -> IO (Maybe (FilePath, FilePath))
findFileWithExtension' extensions searchPath baseName =
  findFirstFile (uncurry (</>))
    [ (path, baseName <.> ext)
    | path <- nub searchPath
    , ext <- nub extensions ]

findFirstFile :: (a -> FilePath) -> [a] -> IO (Maybe a)
findFirstFile file = findFirst
  where findFirst []     = return Nothing
        findFirst (x:xs) = do exists <- doesFileExist (file x)
                              if exists
                                then return (Just x)
                                else findFirst xs

dotToSep :: String -> String
dotToSep = map dts
  where
    dts '.' = pathSeparator
    dts c   = c

-- |Copy the source files into the right directory.  Looks in the
-- build prefix for files that look like the input modules, based on
-- the input search suffixes.  It copies the files into the target
-- directory.

smartCopySources :: Verbosity -- ^verbosity
            -> [FilePath] -- ^build prefix (location of objects)
            -> FilePath -- ^Target directory
            -> [String] -- ^Modules
            -> [String] -- ^search suffixes
            -> IO ()
smartCopySources verbosity srcDirs targetDir sources searchSuffixes
    = mapM moduleToFPErr sources >>= copyFiles verbosity targetDir

    where moduleToFPErr m
              = findFileWithExtension' searchSuffixes srcDirs (dotToSep m)
            >>= maybe notFound return
            where notFound = die $ "Error: Could not find module: " ++ m
                                ++ " with any suffix: " ++ show searchSuffixes

createDirectoryIfMissingVerbose :: Verbosity -> Bool -> FilePath -> IO ()
createDirectoryIfMissingVerbose verbosity parentsToo dir = do
  let msgParents = if parentsToo then " (and its parents)" else ""
  info verbosity ("Creating " ++ dir ++ msgParents)
  createDirectoryIfMissing parentsToo dir

copyFileVerbose :: Verbosity -> FilePath -> FilePath -> IO ()
copyFileVerbose verbosity src dest = do
  info verbosity ("copy " ++ src ++ " to " ++ dest)
  copyFile src dest

-- | Copies a bunch of files to a target directory, preserving the directory
-- structure in the target location. The target directories are created if they
-- do not exist.
--
-- The files are identified by a pair of base directory and a path relative to
-- that base. It is only the relative part that is preserved in the
-- destination.
--
-- For example:
--
-- > copyFiles normal "dist/src"
-- >    [("", "src/Foo.hs"), ("dist/build/", "src/Bar.hs")]
--
-- This would copy \"src\/Foo.hs\" to \"dist\/src\/src\/Foo.hs\" and
-- copy \"dist\/build\/src\/Bar.hs\" to \"dist\/src\/src\/Bar.hs\".
--
-- This operation is not atomic. Any IO failure during the copy (including any
-- missing source files) leaves the target in an unknown state so it is best to
-- use it with a freshly created directory so that it can be simply deleted if
-- anything goes wrong.
--
copyFiles :: Verbosity -> FilePath -> [(FilePath, FilePath)] -> IO ()
copyFiles verbosity targetDir srcFiles = do

  -- Create parent directories for everything
  let dirs = map (targetDir </>) . nub . map (takeDirectory . snd) $ srcFiles
  mapM_ (createDirectoryIfMissingVerbose verbosity True) dirs

  -- Copy all the files
  sequence_ [ let src  = srcBase   </> srcFile
                  dest = targetDir </> srcFile
               in copyFileVerbose verbosity src dest
            | (srcBase, srcFile) <- srcFiles ]

-- adaptation of removeDirectoryRecursive
copyDirectoryRecursiveVerbose :: Verbosity -> FilePath -> FilePath -> IO ()
copyDirectoryRecursiveVerbose verbosity srcDir destDir = do
  info verbosity ("copy directory '" ++ srcDir ++ "' to '" ++ destDir ++ "'.")
  let aux src dest =
         let cp :: FilePath -> IO ()
             cp f = let srcFile  = src  </> f
                        destFile = dest </> f
                    in  do success <- try (copyFileVerbose verbosity srcFile destFile)
                           case success of
                              Left e  -> do isDir <- doesDirectoryExist srcFile
                                            -- If f is not a directory, re-throw the error
                                            unless isDir $ ioError e
                                            aux srcFile destFile
                              Right _ -> return ()
         in  do createDirectoryIfMissingVerbose verbosity False dest
                getDirectoryContentsWithoutSpecial src >>= mapM_ cp
   in aux srcDir destDir

  where getDirectoryContentsWithoutSpecial =
            fmap (filter (not . flip elem [".", ".."]))
          . getDirectoryContents

-- | Use a temporary filename that doesn't already exist.
--
withTempFile :: FilePath -- ^ Temp dir to create the file in
             -> String   -- ^ File name template. See 'openTempFile'.
             -> (FilePath -> Handle -> IO a) -> IO a
withTempFile tmpDir template action =
  Exception.bracket
    (openTempFile tmpDir template)
    (\(name, handle) -> hClose handle >> removeFile name)
    (uncurry action)

-- | Writes a file atomically.
--
-- The file is either written sucessfully or an IO exception is raised and
-- the original file is left unchanged.
--
-- * Warning: On Windows this operation is very nearly but not quite atomic.
--   See below.
--
-- On Posix it works by writing a temporary file and atomically renaming over
-- the top any pre-existing target file with the temporary one.
--
-- On Windows it is not possible to rename over an existing file so the target
-- file has to be deleted before the temporary file is renamed to the target.
-- Therefore there is a race condition between the existing file being removed
-- and the temporary file being renamed. Another thread could write to the
-- target or change the permission on the target directory between the deleting
-- and renaming steps. An exception would be raised but the target file would
-- either no longer exist or have the content as written by the other thread.
--
-- On windows it is not possible to delete a file that is open by a process.
-- This case will give an IO exception but the atomic property is not affected.
--
writeFileAtomic :: FilePath -> String -> IO ()
writeFileAtomic targetFile content = do
  Exception.bracketOnError
    (openTempFile targetDir template)
    (\(tmpFile, tmpHandle) -> IO.Error.try (hClose tmpHandle)
                           >> IO.Error.try (removeFile tmpFile))
    $ \(tmpFile, tmpHandle) -> do
      hPutStr tmpHandle content
      hClose tmpHandle
#if mingw32_HOST_OS || mingw32_TARGET_OS
      renameFile tmpFile targetFile
        -- If the targetFile exists then renameFile will fail
        `Exception.catch` \err -> do
          exists <- fileExists targetFile
          if exists
            then do deleteFile targetFile
                    -- Big fat hairy race condition
                    renameFile tmpFile targetFile
                    -- If the deleteFile succeeds and the renameFile fails
                    -- then we've lost the 
            else ioError err
#else
      renameFile tmpFile targetFile
#endif
  where
    template = targetName <.> "tmp"
    targetDir | null targetDir_ = currentDir
              | otherwise       = targetDir_
    --TODO: remove this when takeDirectory/splitFileName is fixed
    --      to always return a valid dir
    (targetDir_,targetName) = splitFileName targetFile


-- | The path name that represents the current directory.
-- In Unix, it's @\".\"@, but this is system-specific.
-- (E.g. AmigaOS uses the empty string @\"\"@ for the current directory.)
currentDir :: FilePath
currentDir = "."

-- ------------------------------------------------------------
-- * Finding the description file
-- ------------------------------------------------------------

buildInfoExt  :: String
buildInfoExt = "buildinfo"

-- |Package description file (/pkgname/@.cabal@)
defaultPackageDesc :: Verbosity -> IO FilePath
defaultPackageDesc verbosity
    = getCurrentDirectory >>= findPackageDesc verbosity

-- |Find a package description file in the given directory.  Looks for
-- @.cabal@ files.
findPackageDesc :: Verbosity   -- ^Verbosity
                -> FilePath    -- ^Where to look
                -> IO FilePath -- ^<pkgname>.cabal
findPackageDesc _verbosity dir
 = do files <- getDirectoryContents dir
      case filter ((==".cabal") . takeExtension) files of
        []          -> noDesc
        [cabalFile] -> return cabalFile
        multiple    -> multiDesc multiple

  where
    noDesc :: IO a
    noDesc = die $ "No cabal file found.\n"
                ++ "Please create a package description file <pkgname>.cabal"

    multiDesc :: [String] -> IO a
    multiDesc l = die $ "Multiple cabal files found.\n"
                    ++ "Please use only one of: "
                    ++ show l

-- |Optional auxiliary package information file (/pkgname/@.buildinfo@)
defaultHookedPackageDesc :: IO (Maybe FilePath)
defaultHookedPackageDesc = getCurrentDirectory >>= findHookedPackageDesc

-- |Find auxiliary package information in the given directory.
-- Looks for @.buildinfo@ files.
findHookedPackageDesc
    :: FilePath			-- ^Directory to search
    -> IO (Maybe FilePath)	-- ^/dir/@\/@/pkgname/@.buildinfo@, if present
findHookedPackageDesc dir = do
    ns <- getDirectoryContents dir
    case [dir </>  n |
		n <- ns, takeExtension n == '.':buildInfoExt] of
	[] -> return Nothing
	[f] -> return (Just f)
	_ -> die ("Multiple files with extension " ++ buildInfoExt)

-- ------------------------------------------------------------
-- * Unicode stuff
-- ------------------------------------------------------------

-- This is a modification of the UTF8 code from gtk2hs

fromUTF8 :: String -> String
fromUTF8 []     = []
fromUTF8 (c:cs)
  | c <= '\x7F' = c : fromUTF8 cs
  | c <= '\xBF' = replacementChar : fromUTF8 cs
  | c <= '\xDF' = twoBytes   c cs
  | c <= '\xEF' = threeBytes c cs
  | otherwise   = replacementChar : fromUTF8 cs
  where
    twoBytes c1 (c2:cs') = chr (((ord c1 .&. 0x1F) `shiftL` 6) .|.
                                 (ord c2 .&. 0x3F)) : fromUTF8 cs'
    twoBytes _  _        = replacementChar : []

    threeBytes c1 (c2:c3:cs') = chr (((ord c1 .&. 0x0F) `shiftL` 12) .|.
                                     ((ord c2 .&. 0x3F) `shiftL`  6) .|.
                                      (ord c3 .&. 0x3F)) : fromUTF8 cs'
    threeBytes _  _           = replacementChar : []

    replacementChar = '\xfffd'

toUTF8 :: String -> String
toUTF8 []        = []
toUTF8 (c:cs)
  | c <= '\x07F' = c
                 : toUTF8 cs
  | c <= '\x7FF' = chr (0xC0 .|. (w `shiftR` 6))
                 : chr (0x80 .|. (w .&. 0x3F))
                 : toUTF8 cs
  | otherwise    = chr (0xE0 .|.  (w `shiftR` 12))
                 : chr (0x80 .|. ((w `shiftR` 6)  .&. 0x3F))
                 : chr (0x80 .|.  (w .&. 0x3F))
                 : toUTF8 cs
  where w = ord c

-- | Reads a UTF8 encoded text file as a Unicode String
--
-- Reads lazily using ordinary 'readFile'.
--
readTextFile :: FilePath -> IO String
readTextFile = fmap fromUTF8 . readFile

-- | Writes a Unicode String as a UTF8 encoded text file.
--
-- Uses 'writeFileAtomic', so provides the same guarantees.
--
writeTextFile :: FilePath -> String -> IO ()
writeTextFile path = writeFileAtomic path . toUTF8

-- ------------------------------------------------------------
-- * Common utils
-- ------------------------------------------------------------

equating :: Eq a => (b -> a) -> b -> b -> Bool
equating p x y = p x == p y

comparing :: Ord a => (b -> a) -> b -> b -> Ordering
comparing p x y = p x `compare` p y

isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)

intercalate :: [a] -> [[a]] -> [a]
intercalate sep = concat . intersperse sep

lowercase :: String -> String
lowercase = map Char.toLower
