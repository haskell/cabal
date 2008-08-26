-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Tar
-- Copyright   :  (c) 2007 Bjorn Bringert,
--                    2008 Andrea Vezzosi,
--                    2008 Duncan Coutts
-- License     :  BSD-like
--
-- Maintainer  :  duncan@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- TAR archive reading and writing
--
-----------------------------------------------------------------------------
module Distribution.Client.Tar (
  -- * High level all in one operations on files
  createTarGzFile,
  extractTarGzFile,

  -- * Reading and writing the tar format
  read,
  write,

  -- * Packing and unpacking files to\/from a tar archive
  pack,
  unpack,

  -- * Tar archive 'Entry'
  Entry(..), fileName,
  ExtendedHeader(..),
  FileType(..),
  UserId,
  GroupId,
  EpochTime,
  DevMajor,
  DevMinor,
  FileSize,

  -- ** Constructing entries
  emptyEntry,
  simpleFileEntry,
  simpleDirectoryEntry,

  -- ** 'TarPath's
  TarPath,
  toTarPath,
  fromTarPath,

  -- * Sequence of 'Entry' records with failures
  Entries(..),
  foldEntries,
  unfoldEntries,
  mapEntries,

  -- * Sanity checking tar contents
  checkEntryNames
  ) where

import Data.Char   (ord)
import Data.Int    (Int64)
import Data.List   (foldl')
import Data.Monoid (Monoid(..))
import Numeric     (readOct, showOct)

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BS.Char8
import Data.ByteString.Lazy (ByteString)
import qualified Codec.Compression.GZip as GZip

import System.FilePath
         ( (</>) )
import qualified System.FilePath as FilePath.Native
         ( (</>), joinPath, splitDirectories, takeDirectory
         , isAbsolute, isValid, makeRelative )
import qualified System.FilePath.Posix as FilePath.Posix
         ( joinPath, pathSeparator, splitPath, splitDirectories )
import System.Directory
         ( getDirectoryContents, doesDirectoryExist
         , getModificationTime,  createDirectoryIfMissing, copyFile
         , Permissions(..), getPermissions )
import System.Posix.Types
         ( FileMode )
import System.Time
         ( ClockTime(..) )
import System.IO
         ( IOMode(ReadMode), openBinaryFile, hFileSize )
import System.IO.Unsafe (unsafeInterleaveIO)

import Distribution.Client.Utils
         ( writeFileAtomic )

import Prelude hiding (read)

--
-- * High level operations
--

createTarGzFile :: FilePath  -- ^ Full Tarball path
                -> FilePath  -- ^ Base directory
                -> FilePath  -- ^ Directory to archive, relative to base dir
                -> IO ()
createTarGzFile tar base dir =
  writeFileAtomic tar . GZip.compress . write =<< pack base dir

extractTarGzFile :: FilePath -- ^ Destination directory
                 -> FilePath -- ^ Tarball
                 -> IO ()
extractTarGzFile dir tar =
  unpack dir . checkEntryNames . read . GZip.decompress =<< BS.readFile tar

--
-- * Entry type
--

type UserId    = Int
type GroupId   = Int
type EpochTime = Int -- ^ The number of seconds since the UNIX epoch
type DevMajor  = Int
type DevMinor  = Int
type FileSize  = Int64

-- | TAR archive entry
data Entry = Entry {

    -- | Path of the file or directory. The path separator should be @/@ for
    -- portable TAR archives.
    filePath :: TarPath,

    -- | UNIX file mode.
    fileMode :: FileMode,

    -- | Numeric owner user id. Should be set to @0@ if unknown.
    ownerId :: UserId,

    -- | Numeric owner group id. Should be set to @0@ if unknown.
    groupId :: GroupId,

    -- | File size in bytes. Should be 0 for entries other than normal files.
    fileSize :: FileSize,

    -- | Last modification time.
    modTime :: EpochTime,

    -- | Type of this entry.
    fileType :: FileType,

    -- | If the entry is a hard link or a symbolic link, this is the path of
    -- the link target. For all other entry types this should be @\"\"@.
    linkTarget :: FilePath,

    -- | The remaining meta-data is in the V7, ustar/posix or gnu formats
    -- For V7 there is no extended info at all and for posix/ustar the
    -- information is the same though the kind affects the way the information
    -- is encoded.
    headerExt :: ExtendedHeader,

    -- | Entry contents. For entries other than normal
    -- files, this should be an empty string.
    fileContent :: ByteString
  }

fileName :: Entry -> FilePath
fileName = fromTarPath . filePath

data ExtendedHeader
   = V7
   | USTAR {
    -- | The owner user name. Should be set to @\"\"@ if unknown.
    ownerName :: String,

    -- | The owner group name. Should be set to @\"\"@ if unknown.
    groupName :: String,

    -- | For character and block device entries, this is the
    -- major number of the device. For all other entry types, it
    -- should be set to @0@.
    deviceMajor :: DevMajor,

    -- | For character and block device entries, this is the
    -- minor number of the device. For all other entry types, it
    -- should be set to @0@.
    deviceMinor :: DevMinor
   }
   | GNU {
    -- | The owner user name. Should be set to @\"\"@ if unknown.
    ownerName :: String,

    -- | The owner group name. Should be set to @\"\"@ if unknown.
    groupName :: String,

    -- | For character and block device entries, this is the
    -- major number of the device. For all other entry types, it
    -- should be set to @0@.
    deviceMajor :: DevMajor,

    -- | For character and block device entries, this is the
    -- minor number of the device. For all other entry types, it
    -- should be set to @0@.
    deviceMinor :: DevMinor
   }

-- | TAR archive entry types.
data FileType = NormalFile
              | HardLink
              | SymbolicLink
              | CharacterDevice
              | BlockDevice
              | Directory
              | FIFO
              | ExtendedHeader
              | GlobalHeader
              | Custom Char   -- 'A' .. 'Z'
              | Reserved Char -- other / reserved / unknown
  deriving (Eq, Show)

toFileTypeCode :: FileType -> Char
toFileTypeCode NormalFile      = '0'
toFileTypeCode HardLink        = '1'
toFileTypeCode SymbolicLink    = '2'
toFileTypeCode CharacterDevice = '3'
toFileTypeCode BlockDevice     = '4'
toFileTypeCode Directory       = '5'
toFileTypeCode FIFO            = '6'
toFileTypeCode ExtendedHeader  = 'x'
toFileTypeCode GlobalHeader    = 'g'
toFileTypeCode (Custom   c)    = c
toFileTypeCode (Reserved c)    = c

fromFileTypeCode :: Char -> FileType
fromFileTypeCode '0'  = NormalFile
fromFileTypeCode '\0' = NormalFile
fromFileTypeCode '1'  = HardLink
fromFileTypeCode '2'  = SymbolicLink
fromFileTypeCode '3'  = CharacterDevice
fromFileTypeCode '4'  = BlockDevice
fromFileTypeCode '5'  = Directory
fromFileTypeCode '6'  = FIFO
fromFileTypeCode '7'  = NormalFile
fromFileTypeCode 'x'  = ExtendedHeader
fromFileTypeCode 'g'  = GlobalHeader
fromFileTypeCode  c   | c >= 'A' && c <= 'Z'
                      = Custom c
fromFileTypeCode  c   = Reserved c

emptyEntry :: FileType -> TarPath -> Entry
emptyEntry ftype tarpath = Entry {
    filePath = tarpath,
    fileMode = case ftype of
                 Directory -> 0o0755  -- rwxr-xr-x for directories
                 _         -> 0o0644, -- rw-r--r-- for normal files
    ownerId  = 0,
    groupId  = 0,
    fileSize = 0,
    modTime  = 0,
    fileType = ftype,
    linkTarget = "",
    headerExt  = USTAR {
      ownerName = "",
      groupName = "",
      deviceMajor = 0,
      deviceMinor = 0
    },
    fileContent = BS.empty
  }

simpleFileEntry :: TarPath -> ByteString -> Entry
simpleFileEntry name content = (emptyEntry NormalFile name) {
    fileSize = BS.length content,
    fileContent = content
  }

simpleDirectoryEntry :: TarPath -> Entry
simpleDirectoryEntry name = emptyEntry Directory name

--
-- * Tar paths
--

-- | The classic tar format allowed just 100 charcters for the file name. The
-- USTAR format extended this with an extra 155 characters, however it uses a
-- complex method of splitting the name between the two sections.
--
-- Instead of just putting any overflow into the extended area, it uses the
-- extended area as a prefix. The agrevating insane bit however is that the
-- prefix (if any) must only contain a directory prefix. That is the split
-- between the two areas must be on a directory separator boundary. So there is
-- no simple calculation to work out if a file name is too long. Instead we
-- have to try to find a valid split that makes the name fit in the two areas.
--
-- The rationale presumably was to make it a bit more compatible with tar
-- programs that only understand the classic format. A classic tar would be
-- able to extract the file name and possibly some dir prefix, but not the
-- full dir prefix. So the files would end up in the wrong place, but that's
-- probably better than ending up with the wrong names too.
--
-- So it's understandable but rather annoying.
--
-- * Tar paths use posix format (ie @\'/\'@ directory separators), irrespective
--   of the local path conventions.
--
-- * The directory separator between the prefix and name is /not/ stored.
--
data TarPath = TarPath FilePath -- path name, 100 characters max.
                       FilePath -- path prefix, 155 characters max.

-- | Convert a 'TarPath' to a native 'FilePath'.
--
-- The native 'FilePath' will use the native directory separator but it is not
-- otherwise checked for validity or sanity. In particular:
--
-- * The tar path may be invalid as a native path, eg the filename @\"nul\"@ is
--   not valid on Windows.
-- * The tar path may be an absolute path or may contain @\"..\"@ components.
--   For security reasons this should not usually be allowed, but it is your
--   responsibility to check for these conditions.
--
fromTarPath :: TarPath -> FilePath
fromTarPath (TarPath name prefix) =
  FilePath.Native.joinPath $ FilePath.Posix.splitDirectories prefix
                          ++ FilePath.Posix.splitDirectories name

-- | Convert a native 'FilePath' to a 'TarPath'. The 'FileType' is needed
-- because for directories a 'TarPath' uses a trailing @\/@.
--
-- The conversion may fail if the 'FilePath' is too long. See 'TarPath' for a
-- description of the problem with splitting long 'FilePath's.
--
toTarPath :: FileType -> FilePath -> Either String TarPath
toTarPath ftype = splitLongPath
                . addTrailingSep ftype
                . FilePath.Posix.joinPath
                . FilePath.Native.splitDirectories
  where
    addTrailingSep Directory path = path ++ [FilePath.Posix.pathSeparator]
    addTrailingSep _         path = path

-- | Takes a sanitized path, split on directory separators and tries to pack it
-- into the 155 + 100 tar file name format.
--
-- The stragey is this: take the name-directory components in reverse order
-- and try to fit as many components into the 100 long name area as possible.
-- If all the remaining components fit in the 155 name area then we win.
--
splitLongPath :: FilePath -> Either String TarPath
splitLongPath path =
  case packName nameMax (reverse (FilePath.Posix.splitPath path)) of
    Left err                 -> Left err
    Right (name, [])         -> Right (TarPath name "")
    Right (name, first:rest) -> case packName prefixMax remainder of
      Left err               -> Left err
      Right (_     , (_:_))  -> Left "File name too long (cannot split)"
      Right (prefix, [])     -> Right (TarPath name prefix)
      where
        -- drop the '/' between the name and prefix:
        remainder = init first : rest

  where
    nameMax, prefixMax :: Int
    nameMax   = 100
    prefixMax = 155

    packName _      []     = Left "File name empty"
    packName maxLen (c:cs)
      | n > maxLen         = Left "File name too long"
      | otherwise          = Right (packName' maxLen n [c] cs)
      where n = length c

    packName' maxLen n ok (c:cs)
      | n' <= maxLen             = packName' maxLen n' (c:ok) cs
                                     where n' = n + length c
    packName' _      _ ok    cs  = (FilePath.Posix.joinPath ok, cs)

--
-- * Entries type
--

-- | A tar archive is a sequence of entries.
data Entries = Next Entry Entries
             | Done
             | Fail String

unfoldEntries :: (a -> Either String (Maybe (Entry, a))) -> a -> Entries
unfoldEntries f = unfold
  where
    unfold x = case f x of
      Left err             -> Fail err
      Right Nothing        -> Done
      Right (Just (e, x')) -> Next e (unfold x')

foldEntries :: (Entry -> a -> a) -> a -> (String -> a) -> Entries -> a
foldEntries next done fail' = fold
  where
    fold (Next e es) = next e (fold es)
    fold Done        = done
    fold (Fail err)  = fail' err

mapEntries :: (Entry -> Either String Entry) -> Entries -> Entries
mapEntries f =
  foldEntries (\entry rest -> either Fail (flip Next rest) (f entry)) Done Fail

--
-- * Checking
--

checkEntryNames :: Entries -> Entries
checkEntryNames =
  mapEntries (\entry -> maybe (Right entry) Left (checkEntryName entry))

checkEntryName :: Entry -> Maybe String
checkEntryName entry = case fileType entry of
    HardLink     -> check (fileName entry) `mappend` check (linkTarget entry)
    SymbolicLink -> check (fileName entry) `mappend` check (linkTarget entry)
    _            -> check (fileName entry)

  where
    check name
      | FilePath.Native.isAbsolute name
      = Just $ "Absolute file name in tar archive: " ++ show name
      | not (FilePath.Native.isValid name)
      = Just $ "Invalid file name in tar archive: " ++ show name
      | any (=="..") (FilePath.Native.splitDirectories name)
      = Just $ "Invalid file name in tar archive: " ++ show name
      | otherwise = Nothing

--
-- * Reading
--

read :: ByteString -> Entries
read = unfoldEntries getEntry

getEntry :: ByteString -> Either String (Maybe (Entry, ByteString))
getEntry bs
  | BS.length header < 512 = Left "Truncated TAR archive"
  | endBlock = Right Nothing --FIXME: force last two blocks to close fds!
  | not (correctChecksum header chksum)  = Left "TAR checksum error"
  | magic /= "ustar\NUL00"
 && magic /= "ustar  \NUL" = Left $ "TAR entry not ustar format: " ++ show magic
  | otherwise = Right (Just (entry, bs'''))
  where
   (header,bs')  = BS.splitAt 512 bs

   endBlock   = getByte 0 header == '\0'

   name       = getString   0 100 header
   mode       = getOct    100   8 header
   uid        = getOct    108   8 header
   gid        = getOct    116   8 header
   size       = getOct    124  12 header
   mtime      = getOct    136  12 header
   chksum     = getOct    148   8 header
   typecode   = getByte   156     header
   linkname   = getString 157 100 header
   magic      = getChars  257   8 header
   uname      = getString 265  32 header
   gname      = getString 297  32 header
   devmajor   = getOct    329   8 header
   devminor   = getOct    337   8 header
   prefix     = getString 345 155 header
--   trailing   = getBytes  500  12 header --TODO: check all \0's

   padding    = (512 - size) `mod` 512
   (cnt,bs'') = BS.splitAt size bs'
   bs'''      = BS.drop padding bs''

   entry      = Entry {
     filePath    = TarPath name prefix,
     fileMode    = mode,
     ownerId     = uid,
     groupId     = gid,
     fileSize    = size,
     modTime     = mtime,
     fileType    = fromFileTypeCode typecode,
     linkTarget  = linkname,
     headerExt   = case magic of
       "\0\0\0\0\0\0\0\0" -> V7
       "ustar\NUL00" -> USTAR {
         ownerName   = uname,
         groupName   = gname,
         deviceMajor = devmajor,
         deviceMinor = devminor
       }
       "ustar  \NUL" -> GNU {
         ownerName   = uname,
         groupName   = gname,
         deviceMajor = devmajor,
         deviceMinor = devminor
       }
       _ -> V7, --FIXME: fail instead
     fileContent = cnt
   }

correctChecksum :: ByteString -> Int -> Bool
correctChecksum header checksum = checksum == checksum'
  where
    -- sum of all 512 bytes in the header block,
    -- treating each byte as an 8-bit unsigned value
    checksum' = BS.Char8.foldl' (\x y -> x + ord y) 0 header'
    -- treating the 8 bytes of chksum as blank characters.
    header'   = BS.concat [BS.take 148 header,
                           BS.Char8.replicate 8 ' ',
                           BS.drop 156 header]

-- * TAR format primitive input

getOct :: Integral a => Int64 -> Int64 -> ByteString -> a
getOct off len = parseOct
               . BS.Char8.unpack
               . BS.Char8.takeWhile (\c -> c /= '\NUL' && c /= ' ')
               . getBytes off len
  where
    parseOct "" = 0
    parseOct s = case readOct s of
                   [(x,[])] -> x
                   _        -> error $ "Number format error: " ++ show s

getBytes :: Int64 -> Int64 -> ByteString -> ByteString
getBytes off len = BS.take len . BS.drop off

getByte :: Int64 -> ByteString -> Char
getByte off bs = BS.Char8.index bs off

getChars :: Int64 -> Int64 -> ByteString -> String
getChars off len = BS.Char8.unpack . getBytes off len

getString :: Int64 -> Int64 -> ByteString -> String
getString off len = BS.Char8.unpack . BS.Char8.takeWhile (/='\0') . getBytes off len

--
-- * Writing
--

-- | Creates an uncompressed archive
write :: [Entry] -> ByteString
write es = BS.concat $ map putEntry es ++ [BS.replicate (512*2) 0]

putEntry :: Entry -> ByteString
putEntry entry = BS.concat [ header, content, padding ]
  where
    header  = putHeader entry
    content = fileContent entry
    padding = BS.replicate paddingSize 0
    paddingSize = fromIntegral $ negate (fileSize entry) `mod` 512

putHeader :: Entry -> BS.ByteString
putHeader entry =
     BS.Char8.pack $ take 148 block
  ++ putOct 7 checksum
  ++ ' ' : drop 156 block
  where
    block    = putHeaderNoChkSum entry
    checksum = foldl' (\x y -> x + ord y) 0 block

putHeaderNoChkSum :: Entry -> String
putHeaderNoChkSum entry = concat
    [ putString  100 $ name
    , putOct       8 $ fileMode entry
    , putOct       8 $ ownerId entry
    , putOct       8 $ groupId entry
    , putOct      12 $ fileSize entry
    , putOct      12 $ modTime entry
    , fill         8 $ ' ' -- dummy checksum
    , putChar8       $ toFileTypeCode (fileType entry)
    , putString  100 $ linkTarget entry
    ] ++
  case headerExt entry of
  V7    ->
      fill 255 '\NUL'
  ext@USTAR {}-> concat
    [ putString    8 $ "ustar\NUL00"
    , putString   32 $ ownerName ext
    , putString   32 $ groupName ext
    , putOct       8 $ deviceMajor ext
    , putOct       8 $ deviceMinor ext
    , putString  155 $ prefix
    , fill        12 $ '\NUL'
    ]
  ext@GNU {} -> concat
    [ putString    8 $ "ustar  \NUL"
    , putString   32 $ ownerName ext
    , putString   32 $ groupName ext
    , putGnuDev    8 $ deviceMajor ext
    , putGnuDev    8 $ deviceMinor ext
    , putString  155 $ prefix
    , fill        12 $ '\NUL'
    ]
  where
    TarPath name prefix = filePath entry
    putGnuDev w n = case fileType entry of
      CharacterDevice -> putOct w n
      BlockDevice     -> putOct w n
      _               -> replicate w '\NUL'


-- * TAR format primitive output

type FieldWidth = Int

putString :: FieldWidth -> String -> String
putString n s = take n s ++ fill (n - length s) '\NUL'

putOct :: Integral a => FieldWidth -> a -> String
putOct n x =
  let octStr = take (n-1) $ showOct x ""
   in fill (n - length octStr - 1) '0'
   ++ octStr
   ++ putChar8 '\NUL'

putChar8 :: Char -> String
putChar8 c = [c]

fill :: FieldWidth -> Char -> String
fill n c = replicate n c

--
-- * Unpacking
--

unpack :: FilePath -> Entries -> IO ()
unpack dir entries = extractLinks =<< extractFiles [] entries
  where
    extractFiles _     (Fail err)            = Prelude.fail err
    extractFiles links Done                  = return links
    extractFiles links (Next entry entries') = case fileType entry of
      NormalFile   -> BS.writeFile (dir </> fileName entry) (fileContent entry)
                   >> extractFiles links entries'
      HardLink     -> saveLink
      SymbolicLink -> saveLink
      Directory    -> createDirectoryIfMissing False (dir </> fileName entry)
                   >> extractFiles links entries'
      _            -> extractFiles links entries' -- FIXME: warning?
      where
        saveLink    = seq (length name)
                    $ seq (length name)
                    $ extractFiles (link:links) entries'
          where
            name    = fileName entry
            target  = linkTarget entry
            link    = (name, target)

    extractLinks = mapM_ $ \(name, target) ->
      let path      = dir </> name
       in copyFile (FilePath.Native.takeDirectory path </> target) path

--
-- * Packing
--

-- | Creates a tar archive from a directory of files, the paths in the archive
-- will be relative to the given base directory.
--
pack :: FilePath        -- ^ Base directory
     -> FilePath        -- ^ Directory or file to package, relative to the base dir
     -> IO [Entry]
pack baseDir sourceDir =
      mapM (unsafeInterleaveIO . uncurry (createFileEntry baseDir))
  =<< recurseDirectories [baseDir </> sourceDir]

recurseDirectories :: [FilePath] -> IO [(FileType, FilePath)]
recurseDirectories []         = return []
recurseDirectories (dir:dirs) = unsafeInterleaveIO $ do
  (files, dirs') <- collect [] [] =<< getDirectoryContents dir

  files' <- recurseDirectories (dirs' ++ dirs)
  return ((Directory, dir) : map ((,) NormalFile) files ++ files')

  where
    collect files dirs' []              = return (reverse files, reverse dirs')
    collect files dirs' (entry:entries) | ignore entry
                                        = collect files dirs' entries
    collect files dirs' (entry:entries) = do
      let dirEntry = dir </> entry
      isDirectory <- doesDirectoryExist dirEntry
      if isDirectory
        then collect files (dirEntry:dirs') entries
        else collect (dirEntry:files) dirs' entries

    ignore ['.']      = True
    ignore ['.', '.'] = True
    ignore _          = False

createFileEntry :: FilePath -- ^ path to find the file
                -> FileType
                -> FilePath -- ^ path to use for the tar Entry
                -> IO Entry
createFileEntry baseDir ftype absPath = do
  let relPath = FilePath.Native.makeRelative baseDir absPath
  tarpath <- either Prelude.fail return (toTarPath ftype relPath)
  mtime   <- getModTime absPath

  case ftype of
    NormalFile -> do
      file    <- openBinaryFile absPath ReadMode
      mode    <- getFileMode absPath
      size    <- hFileSize file
      content <- BS.hGetContents file
      return (emptyEntry NormalFile tarpath) {
        fileMode    = mode,
        modTime     = mtime,
        fileSize    = fromIntegral size,
        fileContent = content
      }
    _ ->
      return (emptyEntry Directory tarpath) {
        modTime     = mtime
      }

-- | We can't be precise because of portability, so we default to rw-r--r-- for
-- normal filesand rwxr-xr-x for executables.
getFileMode :: FilePath -> IO FileMode
getFileMode path = do
  perms <- getPermissions path
  if executable perms
    then return 0o0755
    else return 0o0644

getModTime :: FilePath -> IO EpochTime
getModTime path =
    do (TOD s _) <- getModificationTime path
       return (fromIntegral s)
