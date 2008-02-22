-- | Simplistic TAR archive reading. Only gets the file names and file contents.
module Hackage.Tar (TarHeader(..), TarFileType(..),
                                         readTarArchive, extractTarArchive, 
                                         extractTarGzFile, gunzip) where

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BS.Char8
import Data.ByteString.Lazy (ByteString)
import Data.Bits ((.&.))
import Data.Char (ord)
import Data.Int (Int8, Int64)
import Data.List (unfoldr)
import Data.Maybe (catMaybes)
import Numeric (readOct)
import System.Directory (Permissions(..), setPermissions, createDirectoryIfMissing, copyFile)
import System.FilePath ((</>), isValid, isAbsolute)
import System.Posix.Types (FileMode)

-- GNU gzip
import Codec.Compression.GZip (decompress)

-- Or use Ian's gunzip:
-- import Codec.Compression.GZip.GUnZip (gunzip)

gunzip :: ByteString -> ByteString
gunzip = decompress

data TarHeader = TarHeader {
                    tarFileName   :: FilePath,
                    tarFileMode   :: FileMode,
                    tarFileType   :: TarFileType,
                    tarLinkTarget :: FilePath
                   } deriving Show

data TarFileType = TarNormalFile
                 | TarHardLink
                 | TarSymbolicLink
                 | TarDirectory
                 | TarOther Char
                  deriving (Eq,Show)

readTarArchive :: ByteString -> [(TarHeader,ByteString)]
readTarArchive = catMaybes . unfoldr getTarEntry

extractTarArchive :: Maybe FilePath -> [(TarHeader,ByteString)] -> IO ()
extractTarArchive mdir = mapM_ (uncurry (extractEntry mdir))

extractTarGzFile :: Maybe FilePath -- ^ Destination directory
               -> FilePath -- ^ Tarball
               -> IO ()
extractTarGzFile mdir file = 
    BS.readFile file >>= extractTarArchive mdir . readTarArchive .  decompress {- gunzip -}

--
-- * Extracting
--

extractEntry :: Maybe FilePath -> TarHeader -> ByteString -> IO ()
extractEntry mdir hdr cnt
    = do path <- relativizePath mdir (tarFileName hdr)
         let setPerms   = setPermissions path (fileModeToPermissions (tarFileMode hdr))
             copyLinked = relativizePath mdir (tarLinkTarget hdr) >>= copyFile path
         case tarFileType hdr of
           TarNormalFile   -> BS.writeFile path cnt >> setPerms
           TarHardLink     -> copyLinked >> setPerms
           TarSymbolicLink -> copyLinked --FIXME: what if the other file has not
                                         --been unpacked yet? Perhaps collect all
                                         --links and do them at the end.
           TarDirectory    -> createDirectoryIfMissing False path >> setPerms
           TarOther _      -> return () -- FIXME: warning?

relativizePath :: Monad m => Maybe FilePath -> FilePath -> m FilePath
relativizePath mdir file
    | isAbsolute file    = fail $ "Absolute file name in TAR archive: " ++ show file
    | not (isValid file) = fail $ "Invalid file name in TAR archive: " ++ show file
    | otherwise          = return $ maybe file (</> file) mdir

fileModeToPermissions :: FileMode -> Permissions
fileModeToPermissions m = 
    Permissions {
                 readable   = m .&. ownerReadMode    /= 0,
                 writable   = m .&. ownerWriteMode   /= 0,
                 executable = m .&. ownerExecuteMode /= 0,
                 searchable = m .&. ownerExecuteMode /= 0
                }

ownerReadMode    :: FileMode
ownerReadMode    = 0o000400

ownerWriteMode   :: FileMode
ownerWriteMode   = 0o000200

ownerExecuteMode :: FileMode
ownerExecuteMode = 0o000100

--
-- * Reading
--

getTarEntry :: ByteString -> Maybe (Maybe (TarHeader,ByteString), ByteString)
getTarEntry bs | endBlock = Nothing
               | BS.length hdr < 512 = error "Truncated TAR archive."
               | not (checkChkSum hdr chkSum) = error "TAR checksum error."
               | otherwise = Just (Just (info, cnt), bs''')

   where (hdr,bs') = BS.splitAt 512 bs

         endBlock  = getByte 0 hdr == '\0'

         fileSuffix = getString   0 100 hdr
         mode       = getOct    100   8 hdr
         chkSum     = getOct    148   8 hdr
         typ        = getByte   156     hdr
         size       = getOct    124  12 hdr
         linkTarget = getString 157 100 hdr
         filePrefix = getString 345 155 hdr

         padding    = (512 - size) `mod` 512
         (cnt,bs'') = BS.splitAt size bs'
         bs'''      = BS.drop padding bs''

         fileType   = case typ of
                        '\0'-> TarNormalFile
                        '0' -> TarNormalFile
                        '1' -> TarHardLink
                        '2' -> TarSymbolicLink
                        '5' -> TarDirectory
                        c   -> TarOther c
                        
         path       = filePrefix ++ fileSuffix
         info       = TarHeader { tarFileName   = path, 
                                  tarFileMode   = mode,
                                  tarFileType   = fileType,
                                  tarLinkTarget = linkTarget }

checkChkSum :: ByteString -> Int -> Bool
checkChkSum hdr s = s == chkSum hdr' || s == signedChkSum hdr'
  where 
    -- replace the checksum with spaces
    hdr' = BS.concat [BS.take 148 hdr, BS.Char8.replicate 8 ' ', BS.drop 156 hdr]
    -- tar.info says that Sun tar is buggy and 
    -- calculates the checksum using signed chars
    chkSum = BS.Char8.foldl' (\x y -> x + ord y) 0
    signedChkSum = BS.Char8.foldl' (\x y -> x + (ordSigned y)) 0

ordSigned :: Char -> Int
ordSigned c = fromIntegral (fromIntegral (ord c) :: Int8)

-- * TAR format primitive input

getOct :: Integral a => Int64 -> Int64 -> ByteString -> a
getOct off len = parseOct . getString off len
  where parseOct "" = 0
        parseOct s = case readOct s of
                       [(x,_)] -> x
                       _       -> error $ "Number format error: " ++ show s

getBytes :: Int64 -> Int64 -> ByteString -> ByteString
getBytes off len = BS.take len . BS.drop off

getByte :: Int64 -> ByteString -> Char
getByte off bs = BS.Char8.index bs off

getString :: Int64 -> Int64 -> ByteString -> String
getString off len = BS.Char8.unpack . BS.Char8.takeWhile (/='\0') . getBytes off len
