{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK hide #-}

module Distribution.Compat.CopyFile
  ( copyFile
  , copyFileChanged
  , filesEqual
  , copyOrdinaryFile
  , copyExecutableFile
  , setFileOrdinary
  , setFileExecutable
  , setDirOrdinary
  ) where

import Distribution.Compat.Prelude
import Prelude ()

#ifndef mingw32_HOST_OS
import Distribution.Compat.Internal.TempFile

import Control.Exception
         ( bracketOnError )
import qualified Data.ByteString.Lazy as BSL
import Data.Bits
         ( (.|.) )
import System.IO.Error
         ( ioeSetLocation )
import System.Directory
         ( doesFileExist, renameFile, removeFile )
import System.FilePath
         ( takeDirectory )
import System.IO
         ( IOMode(ReadMode), hClose, hGetBuf, hPutBuf, hFileSize
         , withBinaryFile )
import Foreign
         ( allocaBytes )

import System.Posix.Types
         ( FileMode )
import System.Posix.Files
         ( getFileStatus, fileMode, setFileMode )

#else /* else mingw32_HOST_OS */

import qualified Data.ByteString.Lazy as BSL
import System.IO.Error
  ( ioeSetLocation )
import System.Directory
  ( doesFileExist )
import System.FilePath
  ( addTrailingPathSeparator
  , hasTrailingPathSeparator
  , isPathSeparator
  , isRelative
  , joinDrive
  , joinPath
  , pathSeparator
  , pathSeparators
  , splitDirectories
  , splitDrive
  )
import System.IO
  ( IOMode(ReadMode), hFileSize
  , withBinaryFile )

import qualified System.Win32.File as Win32 ( copyFile )
#endif /* mingw32_HOST_OS */

copyOrdinaryFile, copyExecutableFile :: FilePath -> FilePath -> IO ()
copyOrdinaryFile src dest = copyFile src dest >> setFileOrdinary dest
copyExecutableFile src dest = copyFile src dest >> setFileExecutable dest

setFileOrdinary, setFileExecutable, setDirOrdinary :: FilePath -> IO ()
#ifndef mingw32_HOST_OS
-- When running with a restrictive UMASK such as 0077 we still want to
-- install files and directories that are accessible to other users.
setFileOrdinary   path = addFileMode path 0o644 -- file perms -rw-r--r--
setFileExecutable path = addFileMode path 0o755 -- file perms -rwxr-xr-x

addFileMode :: FilePath -> FileMode -> IO ()
addFileMode name m = do
  o <- fileMode <$> getFileStatus name
  setFileMode name (m .|. o)
#else
setFileOrdinary   _ = return ()
setFileExecutable _ = return ()
#endif
-- This happens to be true on Unix and currently on Windows too:
setDirOrdinary = setFileExecutable

-- | Copies a file to a new destination.
-- Often you should use `copyFileChanged` instead.
copyFile :: FilePath -> FilePath -> IO ()
copyFile fromFPath toFPath =
  copy
    `catchIO` (\ioe -> throwIO (ioeSetLocation ioe "copyFile"))
  where
#ifndef mingw32_HOST_OS
      copy = withBinaryFile fromFPath ReadMode $ \hFrom ->
             bracketOnError openTmp cleanTmp $ \(tmpFPath, hTmp) ->
             do allocaBytes bufferSize $ copyContents hFrom hTmp
                hClose hTmp
                renameFile tmpFPath toFPath
      openTmp = openBinaryTempFile (takeDirectory toFPath) ".copyFile.tmp"
      cleanTmp (tmpFPath, hTmp) = do
        hClose hTmp          `catchIO` \_ -> return ()
        removeFile tmpFPath  `catchIO` \_ -> return ()
      bufferSize = 4096

      copyContents hFrom hTo buffer = do
              count <- hGetBuf hFrom buffer bufferSize
              when (count > 0) $ do
                      hPutBuf hTo buffer count
                      copyContents hFrom hTo buffer
#else
      copy = Win32.copyFile (toExtendedLengthPath fromFPath)
                            (toExtendedLengthPath toFPath)
                            False

-- NOTE: Shamelessly lifted from System.Directory.Internal.Windows

-- | Add the @"\\\\?\\"@ prefix if necessary or possible.  The path remains
-- unchanged if the prefix is not added.  This function can sometimes be used
-- to bypass the @MAX_PATH@ length restriction in Windows API calls.
--
-- See Note [Path normalization].
toExtendedLengthPath :: FilePath -> FilePath
toExtendedLengthPath path
  | isRelative path = path
  | otherwise =
      case normalisedPath of
        '\\' : '?'  : '?' : '\\' : _ -> normalisedPath
        '\\' : '\\' : '?' : '\\' : _ -> normalisedPath
        '\\' : '\\' : '.' : '\\' : _ -> normalisedPath
        '\\' : subpath@('\\' : _)    -> "\\\\?\\UNC" <> subpath
        _                            -> "\\\\?\\" <> normalisedPath
    where normalisedPath = simplifyWindows path

-- | Similar to 'normalise' but:
--
-- * empty paths stay empty,
-- * parent dirs (@..@) are expanded, and
-- * paths starting with @\\\\?\\@ are preserved.
--
-- The goal is to preserve the meaning of paths better than 'normalise'.
--
-- Note [Path normalization]
-- 'normalise' doesn't simplify path names but will convert / into \\
-- this would normally not be a problem as once the path hits the RTS we would
-- have simplified the path then.  However since we're calling the WIn32 API
-- directly we have to do the simplification before the call.  Without this the
-- path Z:// would become Z:\\\\ and when converted to a device path the path
-- becomes \\?\Z:\\\\ which is an invalid path.
--
-- This is not a bug in normalise as it explicitly states that it won't simplify
-- a FilePath.
simplifyWindows :: FilePath -> FilePath
simplifyWindows "" = ""
simplifyWindows path =
  case drive' of
    "\\\\?\\" -> drive' <> subpath
    _ -> simplifiedPath
  where
    simplifiedPath = joinDrive drive' subpath'
    (drive, subpath) = splitDrive path
    drive' = upperDrive (normaliseTrailingSep (normalisePathSeps drive))
    subpath' = appendSep . avoidEmpty . prependSep . joinPath .
               stripPardirs . expandDots . skipSeps .
               splitDirectories $ subpath

    upperDrive d = case d of
      c : ':' : s | isAlpha c && all isPathSeparator s -> toUpper c : ':' : s
      _ -> d
    skipSeps = filter (not . (`elem` (pure <$> pathSeparators)))
    stripPardirs | pathIsAbsolute || subpathIsAbsolute = dropWhile (== "..")
                 | otherwise = id
    prependSep | subpathIsAbsolute = (pathSeparator :)
               | otherwise = id
    avoidEmpty | not pathIsAbsolute
                 && (null drive || hasTrailingPathSep) -- prefer "C:" over "C:."
                 = emptyToCurDir
               | otherwise = id
    appendSep p | hasTrailingPathSep
                  && not (pathIsAbsolute && null p)
                  = addTrailingPathSeparator p
                | otherwise = p
    pathIsAbsolute = not (isRelative path)
    subpathIsAbsolute = any isPathSeparator (take 1 subpath)
    hasTrailingPathSep = hasTrailingPathSeparator subpath

-- | Given a list of path segments, expand @.@ and @..@.  The path segments
-- must not contain path separators.
expandDots :: [FilePath] -> [FilePath]
expandDots = reverse . go []
  where
    go ys' xs' =
      case xs' of
        [] -> ys'
        x : xs ->
          case x of
            "." -> go ys' xs
            ".." ->
              case ys' of
                [] -> go (x : ys') xs
                ".." : _ -> go (x : ys') xs
                _ : ys -> go ys xs
            _ -> go (x : ys') xs

-- | Convert to the right kind of slashes.
normalisePathSeps :: FilePath -> FilePath
normalisePathSeps p = (\ c -> if isPathSeparator c then pathSeparator else c) <$> p

-- | Remove redundant trailing slashes and pick the right kind of slash.
normaliseTrailingSep :: FilePath -> FilePath
normaliseTrailingSep path = do
  let path' = reverse path
  let (sep, path'') = span isPathSeparator path'
  let addSep = if null sep then id else (pathSeparator :)
  reverse (addSep path'')

-- | Convert empty paths to the current directory, otherwise leave it
-- unchanged.
emptyToCurDir :: FilePath -> FilePath
emptyToCurDir ""   = "."
emptyToCurDir path = path
#endif /* mingw32_HOST_OS */

-- | Like `copyFile`, but does not touch the target if source and destination
-- are already byte-identical. This is recommended as it is useful for
-- time-stamp based recompilation avoidance.
copyFileChanged :: FilePath -> FilePath -> IO ()
copyFileChanged src dest = do
  equal <- filesEqual src dest
  unless equal $ copyFile src dest

-- | Checks if two files are byte-identical.
-- Returns False if either of the files do not exist or if files
-- are of different size.
filesEqual :: FilePath -> FilePath -> IO Bool
filesEqual f1 f2 = do
  ex1 <- doesFileExist f1
  ex2 <- doesFileExist f2
  if not (ex1 && ex2)
    then return False
    else withBinaryFile f1 ReadMode $ \h1 ->
      withBinaryFile f2 ReadMode $ \h2 -> do
        s1 <- hFileSize h1
        s2 <- hFileSize h2
        if s1 /= s2
          then return False
          else do
            c1 <- BSL.hGetContents h1
            c2 <- BSL.hGetContents h2
            return $! c1 == c2
