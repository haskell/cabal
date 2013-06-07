{-# LANGUAGE ForeignFunctionInterface, CPP #-}

module Distribution.Client.Utils ( MergeResult(..)
                                 , mergeBy, duplicates, duplicatesBy
                                 , moreRecentFile, inDir, numberOfProcessors
                                 , removeExistingFile
                                 , makeAbsoluteToCwd, filePathToByteString
                                 , byteStringToFilePath, tryCanonicalizePath
                                 , canonicalizePathNoThrow )
       where

import Distribution.Compat.Exception ( catchIO )
import qualified Data.ByteString.Lazy as BS
import Control.Monad
         ( when )
import Data.Bits
         ( (.|.), shiftL, shiftR )
import Data.Char
         ( ord, chr )
import Data.List
         ( sortBy, groupBy )
import Data.Word
         ( Word8, Word32)
import Foreign.C.Types ( CInt(..) )
import qualified Control.Exception as Exception
         ( finally )
import System.Directory
         ( canonicalizePath, doesFileExist, getModificationTime
         , getCurrentDirectory, removeFile, setCurrentDirectory )
import System.FilePath
         ( (</>), isAbsolute )
import System.IO.Unsafe ( unsafePerformIO )

#if defined(mingw32_HOST_OS)
import Control.Monad (liftM2, unless)
import System.Directory (doesDirectoryExist)
#endif

-- | Generic merging utility. For sorted input lists this is a full outer join.
--
-- * The result list never contains @(Nothing, Nothing)@.
--
mergeBy :: (a -> b -> Ordering) -> [a] -> [b] -> [MergeResult a b]
mergeBy cmp = merge
  where
    merge []     ys     = [ OnlyInRight y | y <- ys]
    merge xs     []     = [ OnlyInLeft  x | x <- xs]
    merge (x:xs) (y:ys) =
      case x `cmp` y of
        GT -> OnlyInRight   y : merge (x:xs) ys
        EQ -> InBoth      x y : merge xs     ys
        LT -> OnlyInLeft  x   : merge xs  (y:ys)

data MergeResult a b = OnlyInLeft a | InBoth a b | OnlyInRight b

duplicates :: Ord a => [a] -> [[a]]
duplicates = duplicatesBy compare

duplicatesBy :: (a -> a -> Ordering) -> [a] -> [[a]]
duplicatesBy cmp = filter moreThanOne . groupBy eq . sortBy cmp
  where
    eq a b = case cmp a b of
               EQ -> True
               _  -> False
    moreThanOne (_:_:_) = True
    moreThanOne _       = False

-- | Compare the modification times of two files to see if the first is newer
-- than the second. The first file must exist but the second need not.
-- The expected use case is when the second file is generated using the first.
-- In this use case, if the result is True then the second file is out of date.
--
moreRecentFile :: FilePath -> FilePath -> IO Bool
moreRecentFile a b = do
  exists <- doesFileExist b
  if not exists
    then return True
    else do tb <- getModificationTime b
            ta <- getModificationTime a
            return (ta > tb)

-- | Like 'removeFile', but does not throw an exception when the file does not
-- exist.
removeExistingFile :: FilePath -> IO ()
removeExistingFile path = do
  exists <- doesFileExist path
  when exists $
    removeFile path

-- | Executes the action in the specified directory.
inDir :: Maybe FilePath -> IO a -> IO a
inDir Nothing m = m
inDir (Just d) m = do
  old <- getCurrentDirectory
  setCurrentDirectory d
  m `Exception.finally` setCurrentDirectory old

foreign import ccall "getNumberOfProcessors" c_getNumberOfProcessors :: IO CInt

-- The number of processors is not going to change during the duration of the
-- program, so unsafePerformIO is safe here.
numberOfProcessors :: Int
numberOfProcessors = fromEnum $ unsafePerformIO c_getNumberOfProcessors

-- | Given a relative path, make it absolute relative to the current
-- directory. Absolute paths are returned unmodified.
makeAbsoluteToCwd :: FilePath -> IO FilePath
makeAbsoluteToCwd path | isAbsolute path = return path
                       | otherwise       = do cwd <- getCurrentDirectory
                                              return $! cwd </> path

-- | Convert a 'FilePath' to a lazy 'ByteString'. Each 'Char' is
-- encoded as a little-endian 'Word32'.
filePathToByteString :: FilePath -> BS.ByteString
filePathToByteString p =
  BS.pack $ foldr conv [] codepts
  where
    codepts :: [Word32]
    codepts = map (fromIntegral . ord) p

    conv :: Word32 -> [Word8] -> [Word8]
    conv w32 rest = b0:b1:b2:b3:rest
      where
        b0 = fromIntegral $ w32
        b1 = fromIntegral $ w32 `shiftR` 8
        b2 = fromIntegral $ w32 `shiftR` 16
        b3 = fromIntegral $ w32 `shiftR` 24

-- | Reverse operation to 'filePathToByteString'.
byteStringToFilePath :: BS.ByteString -> FilePath
byteStringToFilePath bs | bslen `mod` 4 /= 0 = unexpected
                        | otherwise = go 0
  where
    unexpected = "Distribution.Client.Utils.byteStringToFilePath: unexpected"
    bslen = BS.length bs

    go i | i == bslen = []
         | otherwise = (chr . fromIntegral $ w32) : go (i+4)
      where
        w32 :: Word32
        w32 = b0 .|. (b1 `shiftL` 8) .|. (b2 `shiftL` 16) .|. (b3 `shiftL` 24)
        b0 = fromIntegral $ BS.index bs i
        b1 = fromIntegral $ BS.index bs (i + 1)
        b2 = fromIntegral $ BS.index bs (i + 2)
        b3 = fromIntegral $ BS.index bs (i + 3)

-- | Workaround for the inconsistent behaviour of 'canonicalizePath'. It throws
-- an error if the path refers to a non-existent file on *nix, but not on
-- Windows.
tryCanonicalizePath :: FilePath -> IO FilePath
tryCanonicalizePath path = do
  ret <- canonicalizePath path
#if defined(mingw32_HOST_OS)
  exists <- liftM2 (||) (doesFileExist ret) (doesDirectoryExist ret)
  unless exists $
    error $ ret ++ ": canonicalizePath: does not exist "
                ++ "(No such file or directory)"
#endif
  return ret

-- | A non-throwing wrapper for 'canonicalizePath'. If 'canonicalizePath' throws
-- an exception, returns the path argument unmodified.
canonicalizePathNoThrow :: FilePath -> IO FilePath
canonicalizePathNoThrow path = do
  canonicalizePath path `catchIO` (\_ -> return path)
