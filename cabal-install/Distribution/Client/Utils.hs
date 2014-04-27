{-# LANGUAGE ForeignFunctionInterface, CPP #-}

module Distribution.Client.Utils ( MergeResult(..)
                                 , mergeBy, duplicates, duplicatesBy
                                 , inDir, determineNumJobs, numberOfProcessors
                                 , removeExistingFile
                                 , makeAbsoluteToCwd, filePathToByteString
                                 , byteStringToFilePath, tryCanonicalizePath
                                 , canonicalizePathNoThrow
                                 , moreRecentFile, existsAndIsMoreRecentThan
                                 , tryFindAddSourcePackageDesc
                                 , tryFindPackageDesc
                                 , relaxEncodingErrors)
       where

import Distribution.Compat.Exception   ( catchIO )
import Distribution.Client.Compat.Time ( getModTime )
import Distribution.Simple.Setup       ( Flag(..) )
import Distribution.Simple.Utils       ( die, findPackageDesc )
import qualified Data.ByteString.Lazy as BS
import Control.Monad
         ( when )
import Data.Bits
         ( (.|.), shiftL, shiftR )
import Data.Char
         ( ord, chr )
import Data.List
         ( isPrefixOf, sortBy, groupBy )
import Data.Word
         ( Word8, Word32)
import Foreign.C.Types ( CInt(..) )
import qualified Control.Exception as Exception
         ( finally )
import System.Directory
         ( canonicalizePath, doesFileExist, getCurrentDirectory
         , removeFile, setCurrentDirectory )
import System.FilePath
         ( (</>), isAbsolute )
import System.IO
         ( Handle
#if MIN_VERSION_base(4,4,0)
         , hGetEncoding, hSetEncoding
#endif
         )
import System.IO.Unsafe ( unsafePerformIO )

#if MIN_VERSION_base(4,4,0)
import GHC.IO.Encoding
         ( recover, TextEncoding(TextEncoding) )
import GHC.IO.Encoding.Failure
         ( recoverEncode, CodingFailureMode(TransliterateCodingFailure) )
#endif

#if defined(mingw32_HOST_OS)
import Prelude hiding (ioError)
import Control.Monad (liftM2, unless)
import System.Directory (doesDirectoryExist)
import System.IO.Error (ioError, mkIOError, doesNotExistErrorType)
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

-- | Determine the number of jobs to use given the value of the '-j' flag.
determineNumJobs :: Flag (Maybe Int) -> Int
determineNumJobs numJobsFlag =
  case numJobsFlag of
    NoFlag        -> 1
    Flag Nothing  -> numberOfProcessors
    Flag (Just n) -> n

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
    ioError $ mkIOError doesNotExistErrorType "canonicalizePath"
                        Nothing (Just ret)
#endif
  return ret

-- | A non-throwing wrapper for 'canonicalizePath'. If 'canonicalizePath' throws
-- an exception, returns the path argument unmodified.
canonicalizePathNoThrow :: FilePath -> IO FilePath
canonicalizePathNoThrow path = do
  canonicalizePath path `catchIO` (\_ -> return path)

--------------------
-- Modification time

-- | Like Distribution.Simple.Utils.moreRecentFile, but uses getModTime instead
-- of getModificationTime for higher precision. We can't merge the two because
-- Distribution.Client.Time uses MIN_VERSION macros.
moreRecentFile :: FilePath -> FilePath -> IO Bool
moreRecentFile a b = do
  exists <- doesFileExist b
  if not exists
    then return True
    else do tb <- getModTime b
            ta <- getModTime a
            return (ta > tb)

-- | Like 'moreRecentFile', but also checks that the first file exists.
existsAndIsMoreRecentThan :: FilePath -> FilePath -> IO Bool
existsAndIsMoreRecentThan a b = do
  exists <- doesFileExist a
  if not exists
    then return False
    else a `moreRecentFile` b

-- | Sets the handler for encoding errors to one that transliterates invalid
-- characters into one present in the encoding (i.e., \'?\').
-- This is opposed to the default behavior, which is to throw an exception on
-- error. This function will ignore file handles that have a Unicode encoding
-- set. It's a no-op for versions of `base` less than 4.4.
relaxEncodingErrors :: Handle -> IO ()
relaxEncodingErrors handle = do
#if MIN_VERSION_base(4,4,0)
  maybeEncoding <- hGetEncoding handle
  case maybeEncoding of
    Just (TextEncoding name decoder encoder) | not ("UTF" `isPrefixOf` name) ->
      let relax x = x { recover = recoverEncode TransliterateCodingFailure }
      in hSetEncoding handle (TextEncoding name decoder (fmap relax encoder))
    _ ->
#endif
      return ()

-- |Like 'tryFindPackageDesc', but with error specific to add-source deps.
tryFindAddSourcePackageDesc :: FilePath -> String -> IO FilePath
tryFindAddSourcePackageDesc depPath err = tryFindPackageDesc depPath $
    err ++ "\n" ++ "Failed to read cabal file of add-source dependency: "
    ++ depPath

-- |Try to find a @.cabal@ file, in directory @depPath@. Fails if one cannot be
-- found, with @err@ prefixing the error message. This function simply allows
-- us to give a more descriptive error than that provided by @findPackageDesc@.
tryFindPackageDesc :: FilePath -> String -> IO FilePath
tryFindPackageDesc depPath err = do
    errOrCabalFile <- findPackageDesc depPath
    case errOrCabalFile of
        Right file -> return file
        Left _ -> die err
