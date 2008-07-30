-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Check
-- Copyright   :  (c) 2007 Bjorn Bringert, 2008 Andrea Vezzosi
-- License     :  BSD-like
--
-- Maintainer  :  duncan@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Simplistic TAR archive reading and writing
--
-- Only handles the file names and file contents, ignores other file metadata.
--
-----------------------------------------------------------------------------
module Distribution.Client.Tar (
    TarHeader(..),
    TarFileType(..),
    readTarArchive,
    extractTarGzFile,
    createTarGzFile
  ) where

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BS.Char8
import Data.ByteString.Lazy (ByteString)
import Data.Bits ((.&.),(.|.))
import Data.Char (ord)
import Data.Int (Int8, Int64)
import Data.List (unfoldr,partition,foldl')
import Data.Maybe (catMaybes)
import Numeric (readOct,showOct)
import System.Directory
         ( getDirectoryContents, doesFileExist, doesDirectoryExist
         , getModificationTime,  createDirectoryIfMissing, copyFile
         , Permissions(..), setPermissions, getPermissions )
import System.Time (ClockTime(..))
import System.FilePath as FilePath
         ( (</>), isValid, isAbsolute, splitFileName, splitDirectories
         , makeRelative )
import qualified System.FilePath.Posix as FilePath.Posix
         ( joinPath, pathSeparator )
import System.Posix.Types (FileMode)
import System.IO
         ( Handle, IOMode(ReadMode), openBinaryFile, hFileSize, hClose )
import System.IO.Unsafe (unsafeInterleaveIO)
import Control.Monad (liftM,when)
import Distribution.Simple.Utils (die)

-- GNU gzip
import qualified Codec.Compression.GZip as GZip
         ( decompress, compress )

-- Or use Ian's gunzip:
-- import Codec.Compression.GZip.GUnZip (gunzip)

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

extractTarArchive :: FilePath -> [(TarHeader,ByteString)] -> IO ()
extractTarArchive dir tar = extract files >> extract links
    where
    extract = mapM_ (uncurry (extractEntry dir))
    -- TODO: does this cause a memory leak?
    (files, links) = partition (not . isLink . tarFileType . fst) tar
    isLink TarHardLink     = True
    isLink TarSymbolicLink = True
    isLink _               = False

extractTarGzFile :: FilePath -- ^ Destination directory
                 -> FilePath -- ^ Tarball
                 -> IO ()
extractTarGzFile dir file =
  extractTarArchive dir . readTarArchive . GZip.decompress =<< BS.readFile file

--
-- * Extracting
--

extractEntry :: FilePath -> TarHeader -> ByteString -> IO ()
extractEntry dir hdr cnt
    = do path <- relativizePath dir (tarFileName hdr)
         let setPerms   = setPermissions path (fileModeToPermissions (tarFileMode hdr))
             copyLinked =
                let (base, _) = splitFileName path
                    sourceName = base </> tarLinkTarget hdr
                in copyFile sourceName path
         case tarFileType hdr of
           TarNormalFile   -> BS.writeFile path cnt >> setPerms
           TarHardLink     -> copyLinked >> setPerms
           TarSymbolicLink -> copyLinked
           TarDirectory    -> createDirectoryIfMissing False path >> setPerms
           TarOther _      -> return () -- FIXME: warning?

relativizePath :: Monad m => FilePath -> FilePath -> m FilePath
relativizePath dir file
    | isAbsolute file    = fail $ "Absolute file name in TAR archive: " ++ show file
    | not (isValid file) = fail $ "Invalid file name in TAR archive: " ++ show file
    | otherwise          = return $ dir </> file

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

--
-- * Writing
--

-- | Creates a tar gzipped archive, the paths in the archive will be relative
-- to the base directory.
--
createTarGzFile :: FilePath        -- ^ Full Tarball path
                -> FilePath        -- ^ Base directory
                -> FilePath        -- ^ Directory or file to package, relative to the base dir
                -> IO ()
createTarGzFile tarFile baseDir sourceDir = do
      (entries,hs) <- fmap unzip
                    . mapM (unsafeInterleaveIO
                          . (\path -> createTarEntry path
                                      $ makeRelative baseDir path))
                  =<< recurseDirectories [baseDir </> sourceDir]
      BS.writeFile tarFile . GZip.compress . entries2Archive $ entries
      mapM_ hClose (catMaybes hs) -- TODO: the handles are explicitly closed because of a bug in bytestring-0.9.0.1,
                                  -- once we depend on a later version we can avoid this hack.

recurseDirectories :: [FilePath] -> IO [FilePath]
recurseDirectories = 
    liftM concat . mapM (\p -> liftM (p:) $ unsafeInterleaveIO $ descendants p)
  where 
    descendants path =
        do d <- doesDirectoryExist path
           if d then do cs <- getDirectoryContents path
                        let cs' = [path</>c | c <- cs, includeDir c]
                        ds <- recurseDirectories cs'
                        return ds
                else return []
     where includeDir "." = False
           includeDir ".." = False
           includeDir _ = True

data TarEntry = TarEntry { entryHdr :: TarHeader, entrySize :: Integer, entryModTime :: EpochTime, entryCnt :: ByteString }

-- | Creates an uncompressed archive
entries2Archive :: [TarEntry] -> ByteString
entries2Archive es = BS.concat $ (map putTarEntry es) ++ [BS.replicate (512*2) 0]

-- TODO: It needs to return the handle only because of the hack in createTarGzFile
createTarEntry :: FilePath -- ^ path to find the file
               -> FilePath -- ^ path to use for the TarHeader
               -> IO (TarEntry,Maybe Handle)
createTarEntry path relpath =
    do ftype <- getFileType path
       let tarpath = nativePathToTarPath ftype relpath
       when (null tarpath || length tarpath > 255) $
            die $ "Path too long: " ++ show tarpath
       mode <- getFileMode ftype path
       let hdr = TarHeader {
                   tarFileName = tarpath,
                   tarFileMode = mode,
                   tarFileType = ftype,
                   tarLinkTarget = ""
                 }
       (sz,cnt,mh) <- case ftype of
                TarNormalFile -> do h <- openBinaryFile path ReadMode
                                    sz <- hFileSize h
                                    cnt <- BS.hGetContents h
                                    return (sz,cnt,Just h)
                _             -> return (0,BS.empty,Nothing)
       time <- getModTime path
       return $ (TarEntry hdr sz time cnt,mh)

getFileType :: FilePath -> IO TarFileType
getFileType path = do b <- doesFileExist path
                      if b then return TarNormalFile
                        else do b' <- doesDirectoryExist path
                                if b' then return TarDirectory
                                   else fail $ "tar: Not directory nor regular file: " ++ path

-- We can't be precise because of portability, so we default to rw-r--r-- for normal files
-- and rwxr-xr-x for directories and executables.
getFileMode :: TarFileType -> FilePath -> IO FileMode
getFileMode ftype path = do
  perms <- getPermissions path
  let x = if executable perms || ftype == TarDirectory then 0o000111 else 0
  return $ 0o000644 .|. x

type EpochTime = Int
          
getModTime :: FilePath -> IO EpochTime
getModTime path = 
    do (TOD s _) <- getModificationTime path
       return (fromIntegral s)

putTarEntry :: TarEntry -> ByteString
putTarEntry TarEntry{entryHdr=hdr,entrySize=size,entryModTime=time,entryCnt=cnt} = 
  BS.concat
    [putTarHeader hdr size time
    ,cnt
    ,BS.replicate ((- fromIntegral size) `mod` 512) 0
    ]


putTarHeader :: TarHeader -> Integer -> EpochTime -> BS.ByteString
putTarHeader hdr filesize modTime = 
    let block = concat $ (putHeaderNoChkSum hdr filesize modTime)
        chkSum = foldl' (\x y -> x + ord y) 0 block
    in BS.Char8.pack $ take 148 block ++
       putOct 8 chkSum ++
       drop 156 block

putHeaderNoChkSum :: TarHeader -> Integer -> EpochTime -> [String]
putHeaderNoChkSum hdr filesize modTime =
    let (filePrefix, fileSuffix) = splitLongPath (tarFileName hdr) in
    [   putString  100 $ fileSuffix
     ,  putOct       8 $ tarFileMode hdr
     ,  putOct       8 $ zero --tarOwnerID hdr
     ,  putOct       8 $ zero --tarGroupID hdr
     ,  putOct      12 $ filesize --tarFileSize hdr
     ,  putOct      12 $ modTime --epochTimeToSecs $ tarModTime hdr
     ,  fill         8 $ ' ' -- dummy checksum
     ,  putTarFileType $ tarFileType hdr
     ,  putString  100 $ tarLinkTarget hdr -- FIXME: take suffix split at / if too long
     ,  putString    6 $ "ustar"
     ,  putString    2 $ "00" -- no nul byte
     ,  putString   32 $ "" --tarOwnerName hdr
     ,  putString   32 $ "" --tarGroupName hdr
     ,  putOct       8 $ zero --tarDeviceMajor hdr
     ,  putOct       8 $ zero --tarDeviceMinor hdr
     ,  putString  155 $ filePrefix
     ,  fill        12 $ '\NUL'
    ]
    where zero :: Int
          zero = 0

putTarFileType :: TarFileType -> String
putTarFileType t = 
    putChar8 $ case t of
                 TarNormalFile      -> '0'
                 TarHardLink        -> '1'
                 TarSymbolicLink    -> '2'
                 TarDirectory       -> '5'
                 TarOther c         -> c

-- | Convert a native path to a unix\/posix style path
-- and for directories add a trailing @\/@.
--
nativePathToTarPath :: TarFileType -> FilePath -> FilePath
nativePathToTarPath ftype = addTrailingSep ftype
                          . FilePath.Posix.joinPath
                          . FilePath.splitDirectories
  where 
    addTrailingSep TarDirectory path = path ++ [FilePath.Posix.pathSeparator]
    addTrailingSep _            path = path

-- | Takes a sanitized path, i.e. converted to Posix form
splitLongPath :: FilePath -> (String,String)
splitLongPath path =
    let (x,y) = splitAt (length path - 101) path 
              -- 101 since we will always move a separator to the prefix  
     in if null x 
         then if null y then err "Empty path." else ("", y)
         else case break (==FilePath.Posix.pathSeparator) y of
              --TODO: convert this to use FilePath.Posix.splitPath
                (_,"")    -> err "Can't split path." 
                (_,_:"")  -> err "Can't split path." 
                (y1,s:y2) | length p > 155 || length y2 > 100 -> err "Can't split path."
                          | otherwise -> (p,y2)
                      where p = x ++ y1 ++ [s]
  where err e = error $ show path ++ ": " ++ e

-- * TAR format primitive output

putString :: Int -> String -> String
putString n s = take n s ++
                   fill (n - length s) '\NUL'

putOct :: (Integral a) => Int -> a -> String
putOct n x = let o = take (n-1) $ showOct x "" in
                fill (n - length o - 1) '0' ++
                o ++
                putChar8 '\NUL'

putChar8 :: Char -> String
putChar8 c = [c]

fill :: Int -> Char -> String
fill n c = replicate n c
