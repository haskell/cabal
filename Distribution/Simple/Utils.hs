{-# OPTIONS -cpp -fffi #-}
-- OPTIONS required for ghc-6.4.x compat, and must appear first
{-# LANGUAGE CPP, ForeignFunctionInterface #-}
{-# OPTIONS_GHC -cpp -fffi #-}
{-# OPTIONS_NHC98 -cpp #-}
{-# OPTIONS_JHC -fcpp -fffi #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Utils
-- Copyright   :  Isaac Jones, Simon Marlow 2003-2004
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
        cabalBootstrapping,

        -- * logging and errors
        die,
        dieWithLocation,
        warn, notice, setupMessage, info, debug,
        chattyTry,

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

        -- * finding files
        findFile,
        findFileWithExtension,
        findFileWithExtension',

        -- * simple file globbing
        matchFileGlob,
        matchDirFileGlob,

        -- * temp files and dirs
        withTempFile,
        withTempDirectory,

        -- * .cabal and .buildinfo files
        defaultPackageDesc,
        findPackageDesc,
        defaultHookedPackageDesc,
        findHookedPackageDesc,

        -- * reading and writing files safely
        withFileContents,
        writeFileAtomic,
        rewriteFile,

        -- * Unicode
        fromUTF8,
        toUTF8,
        readUTF8File,
        withUTF8FileContents,
        writeUTF8File,

        -- * generic utils
        equating,
        comparing,
        isInfixOf,
        intercalate,
        lowercase,
        wrapText,
        wrapLine,
  ) where

import Control.Monad
    ( when, unless, filterM )
import Data.List
    ( nub, unfoldr, isPrefixOf, tails, intersperse )
import Data.Char as Char
    ( toLower, chr, ord )
import Data.Bits
    ( Bits((.|.), (.&.), shiftL, shiftR) )

import System.Directory
    ( getDirectoryContents, doesDirectoryExist, doesFileExist, removeFile )
import System.Environment
    ( getProgName )
import System.Cmd
    ( rawSystem )
import System.Exit
    ( exitWith, ExitCode(..) )
import System.FilePath
    ( normalise, (</>), (<.>), takeDirectory, splitFileName
    , splitExtension, splitExtensions )
import System.Directory
    ( copyFile, createDirectoryIfMissing, renameFile, removeDirectoryRecursive )
import System.IO
    ( Handle, openFile, openBinaryFile, IOMode(ReadMode), hSetBinaryMode
    , hGetContents, stderr, stdout, hPutStr, hFlush, hClose )
import System.IO.Error as IO.Error
    ( try, isDoesNotExistError )
import qualified Control.Exception as Exception

import Distribution.Text
    ( display )
import Distribution.Package
    ( PackageIdentifier )
import Distribution.ModuleName (ModuleName)
import qualified Distribution.ModuleName as ModuleName
import Distribution.Version
    (Version(..))

import Control.Exception (evaluate)

#ifdef __GLASGOW_HASKELL__
import Control.Concurrent (forkIO)
import System.Process (runInteractiveProcess, waitForProcess)
#else
import System.Cmd (system)
import System.Directory (getTemporaryDirectory)
#endif

import Distribution.Compat.TempFile (openTempFile,
                                     openNewBinaryFile)
import Distribution.Compat.Exception (catchIO, onException)
#if mingw32_HOST_OS || mingw32_TARGET_OS
import Distribution.Compat.Exception (throwIOIO)
#endif
import Distribution.Verbosity

-- We only get our own version number when we're building with ourselves
cabalVersion :: Version
#ifdef CABAL_VERSION
cabalVersion = Version [CABAL_VERSION] []
#else
cabalVersion = error "Cabal was not bootstrapped correctly"
#endif

cabalBootstrapping :: Bool
#ifdef CABAL_VERSION
cabalBootstrapping = False
#else
cabalBootstrapping = True
#endif

-- ------------------------------------------------------------------------------- Utils for setup

dieWithLocation :: FilePath -> Maybe Int -> String -> IO a
dieWithLocation filename lineno msg =
  die $ normalise filename
     ++ maybe "" (\n -> ":" ++ show n) lineno
     ++ ": " ++ msg

die :: String -> IO a
die msg = do
  hFlush stdout
  pname <- getProgName
  hPutStr stderr (wrapText (pname ++ ": " ++ msg))
  exitWith (ExitFailure 1)

-- | Non fatal conditions that may be indicative of an error or problem.
--
-- We display these at the 'normal' verbosity level.
--
warn :: Verbosity -> String -> IO ()
warn verbosity msg =
  when (verbosity >= normal) $ do
    hFlush stdout
    hPutStr stderr (wrapText ("Warning: " ++ msg))

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
    putStr (wrapText msg)

setupMessage :: Verbosity -> String -> PackageIdentifier -> IO ()
setupMessage verbosity msg pkgid =
    notice verbosity (msg ++ ' ': display pkgid ++ "...")

-- | More detail on the operation of some action.
--
-- We display these messages when the verbosity level is 'verbose'
--
info :: Verbosity -> String -> IO ()
info verbosity msg =
  when (verbosity >= verbose) $
    putStr (wrapText msg)

-- | Detailed internal debugging information
--
-- We display these messages when the verbosity level is 'deafening'
--
debug :: Verbosity -> String -> IO ()
debug verbosity msg =
  when (verbosity >= deafening) $ do
    putStr (wrapText msg)
    hFlush stdout

-- | Perform an IO action, catching any IO exceptions and printing an error
--   if one occurs.
chattyTry :: String  -- ^ a description of the action we were attempting
          -> IO ()   -- ^ the action itself
          -> IO ()
chattyTry desc action =
  catchIO action $ \exception ->
    putStrLn $ "Error while " ++ desc ++ ": " ++ show exception

-- -----------------------------------------------------------------------------
-- Helper functions

-- | Wraps text to the default line width. Existing newlines are preserved.
wrapText :: String -> String
wrapText = unlines
         . concatMap (map unwords
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
  exitcode <- rawSystem path args
  unless (exitcode == ExitSuccess) $ do
    debug verbosity $ path ++ " returned " ++ show exitcode
    exitWith exitcode

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

      -- We want to process the output as text.
      hSetBinaryMode outh False

      -- fork off a thread to pull on (and discard) the stderr
      -- so if the process writes to stderr we do not block.
      -- NB. do the hGetContents synchronously, otherwise the outer
      -- bracket can exit before this thread has run, and hGetContents
      -- will fail.
      err <- hGetContents errh
      forkIO $ do evaluate (length err); return ()

      -- wait for all the output
      output <- hGetContents outh
      evaluate (length output)

      -- wait for the program to terminate
      exitcode <- waitForProcess pid
      unless (exitcode == ExitSuccess) $
        debug verbosity $ path ++ " returned " ++ show exitcode
                       ++ if null err then "" else
                          " with error message:\n" ++ err

      return (output, exitcode)
#else
  tmpDir <- getTemporaryDirectory
  withTempFile tmpDir ".cmd.stdout" $ \tmpName tmpHandle -> do
    hClose tmpHandle
    let quote name = "'" ++ name ++ "'"
    exitcode <- system $ unwords (map quote (path:args)) ++ " >" ++ quote tmpName
    unless (exitcode == ExitSuccess) $
      debug verbosity $ path ++ " returned " ++ show exitcode
    withFileContents tmpName $ \output ->
      length output `seq` return (output, exitcode)
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

data FileGlob
   -- | No glob at all, just an ordinary file
   = NoGlob FilePath

   -- | dir prefix and extension, like @\"foo\/bar\/\*.baz\"@ corresponds to
   --    @FileGlob \"foo\/bar\" \".baz\"@
   | FileGlob FilePath String

parseFileGlob :: FilePath -> Maybe FileGlob
parseFileGlob filepath = case splitExtensions filepath of
  (filepath', ext) -> case splitFileName filepath' of
    (dir, "*") | '*' `elem` dir
              || '*' `elem` ext
              || null ext            -> Nothing
               | null dir            -> Just (FileGlob "." ext)
               | otherwise           -> Just (FileGlob dir ext)
    _          | '*' `elem` filepath -> Nothing
               | otherwise           -> Just (NoGlob filepath)

matchFileGlob :: FilePath -> IO [FilePath]
matchFileGlob = matchDirFileGlob "."

matchDirFileGlob :: FilePath -> FilePath -> IO [FilePath]
matchDirFileGlob dir filepath = case parseFileGlob filepath of
  Nothing -> die $ "invalid filepath '" ++ filepath
                ++ "'. Wildcards '*' are only allowed in place of the file"
                ++ " name, not in the directory name or file extension."
                ++ " If a wildcard is used it must be with an file extension."
  Just (NoGlob filepath') -> return [filepath']
  Just (FileGlob dir' ext) -> do
    files <- getDirectoryContents (dir </> dir')
    case   [ dir' </> file
           | file <- files
           , let (name, ext') = splitExtensions file
           , not (null name) && ext' == ext ] of
      []      -> die $ "filepath wildcard '" ++ filepath
                    ++ "' does not match any files."
      matches -> return matches

-- |Copy the source files into the right directory.  Looks in the
-- build prefix for files that look like the input modules, based on
-- the input search suffixes.  It copies the files into the target
-- directory.

smartCopySources :: Verbosity -- ^verbosity
            -> [FilePath] -- ^build prefix (location of objects)
            -> FilePath -- ^Target directory
            -> [ModuleName] -- ^Modules
            -> [String] -- ^search suffixes
            -> IO ()
smartCopySources verbosity srcDirs targetDir sources searchSuffixes
    = mapM moduleToFPErr sources >>= copyFiles verbosity targetDir

    where moduleToFPErr m
              = findFileWithExtension' searchSuffixes srcDirs (ModuleName.toFilePath m)
            >>= maybe notFound return
            where notFound = die $ "Error: Could not find module: " ++ display m
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

-- | Use a temporary directory.
--
-- Use this exact given dir which must not already exist.
--
withTempDirectory :: Verbosity -> FilePath -> IO a -> IO a
withTempDirectory verbosity tmpDir =
  Exception.bracket_
    (createDirectoryIfMissingVerbose verbosity True tmpDir)
    (removeDirectoryRecursive tmpDir)

-- | Gets the contents of a file, but guarantee that it gets closed.
--
-- The file is read lazily but if it is not fully consumed by the action then
-- the remaining input is truncated and the file is closed.
--
withFileContents :: FilePath -> (String -> IO a) -> IO a
withFileContents name action =
  Exception.bracket (openFile name ReadMode) hClose
                    (\hnd -> hGetContents hnd >>= action)

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
  (tmpFile, tmpHandle) <- openNewBinaryFile targetDir template
  do  hPutStr tmpHandle content
      hClose tmpHandle
#if mingw32_HOST_OS || mingw32_TARGET_OS
      renameFile tmpFile targetFile
        -- If the targetFile exists then renameFile will fail
        `catchIO` \err -> do
          exists <- doesFileExist targetFile
          if exists
            then do removeFile targetFile
                    -- Big fat hairy race condition
                    renameFile tmpFile targetFile
                    -- If the removeFile succeeds and the renameFile fails
                    -- then we've lost the atomic property.
            else throwIOIO err
#else
      renameFile tmpFile targetFile
#endif
   `onException` do hClose tmpHandle
                    removeFile tmpFile
  where
    template = targetName <.> "tmp"
    targetDir | null targetDir_ = currentDir
              | otherwise       = targetDir_
    --TODO: remove this when takeDirectory/splitFileName is fixed
    --      to always return a valid dir
    (targetDir_,targetName) = splitFileName targetFile

-- | Write a file but only if it would have new content. If we would be writing
-- the same as the existing content then leave the file as is so that we do not
-- update the file's modification time.
--
rewriteFile :: FilePath -> String -> IO ()
rewriteFile path newContent =
  flip catch mightNotExist $ do
    existingContent <- readFile path
    evaluate (length existingContent)
    unless (existingContent == newContent) $
      writeFileAtomic path newContent
  where
    mightNotExist e | isDoesNotExistError e = writeFileAtomic path newContent
                    | otherwise             = ioError e

-- | The path name that represents the current directory.
-- In Unix, it's @\".\"@, but this is system-specific.
-- (E.g. AmigaOS uses the empty string @\"\"@ for the current directory.)
currentDir :: FilePath
currentDir = "."

-- ------------------------------------------------------------
-- * Finding the description file
-- ------------------------------------------------------------

-- |Package description file (/pkgname/@.cabal@)
defaultPackageDesc :: Verbosity -> IO FilePath
defaultPackageDesc _verbosity = findPackageDesc currentDir

-- |Find a package description file in the given directory.  Looks for
-- @.cabal@ files.
findPackageDesc :: FilePath    -- ^Where to look
                -> IO FilePath -- ^<pkgname>.cabal
findPackageDesc dir
 = do files <- getDirectoryContents dir
      -- to make sure we do not mistake a ~/.cabal/ dir for a <pkgname>.cabal
      -- file we filter to exclude dirs and null base file names:
      cabalFiles <- filterM doesFileExist
                       [ dir </> file
                       | file <- files
                       , let (name, ext) = splitExtension file
                       , not (null name) && ext == ".cabal" ]
      case cabalFiles of
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
defaultHookedPackageDesc = findHookedPackageDesc currentDir

-- |Find auxiliary package information in the given directory.
-- Looks for @.buildinfo@ files.
findHookedPackageDesc
    :: FilePath                 -- ^Directory to search
    -> IO (Maybe FilePath)      -- ^/dir/@\/@/pkgname/@.buildinfo@, if present
findHookedPackageDesc dir = do
    files <- getDirectoryContents dir
    buildInfoFiles <- filterM doesFileExist
                        [ dir </> file
                        | file <- files
                        , let (name, ext) = splitExtension file
                        , not (null name) && ext == buildInfoExt ]
    case buildInfoFiles of
        [] -> return Nothing
        [f] -> return (Just f)
        _ -> die ("Multiple files with extension " ++ buildInfoExt)

buildInfoExt  :: String
buildInfoExt = ".buildinfo"

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

-- | Reads a UTF8 encoded text file as a Unicode String
--
-- Reads lazily using ordinary 'readFile'.
--
readUTF8File :: FilePath -> IO String
readUTF8File f = fmap fromUTF8 . hGetContents =<< openBinaryFile f ReadMode

-- | Reads a UTF8 encoded text file as a Unicode String
--
-- Same behaviour as 'withFileContents'.
--
withUTF8FileContents :: FilePath -> (String -> IO a) -> IO a
withUTF8FileContents name action =
  Exception.bracket (openBinaryFile name ReadMode) hClose
                    (\hnd -> hGetContents hnd >>= action . fromUTF8)

-- | Writes a Unicode String as a UTF8 encoded text file.
--
-- Uses 'writeFileAtomic', so provides the same guarantees.
--
writeUTF8File :: FilePath -> String -> IO ()
writeUTF8File path = writeFileAtomic path . toUTF8

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
