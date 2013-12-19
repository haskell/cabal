{-# OPTIONS_GHC -fno-warn-unused-imports #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Tar
-- Copyright   :  (c) 2007 Bjorn Bringert,
--                    2008 Andrea Vezzosi,
--                    2008-2009 Duncan Coutts
-- License     :  BSD3
--
-- Maintainer  :  duncan@community.haskell.org
-- Portability :  portable
--
-- Reading, writing and manipulating \"@.tar@\" archive files.
--
-----------------------------------------------------------------------------
module Distribution.Client.Tar (
  -- * High level \"all in one\" operations
  createTarGzFile,
  extractTarGzFile,

  -- * Converting between internal and external representation
  read,
  write,
  writeEntries,

  -- * Packing and unpacking files to\/from internal representation
  pack,
  unpack,

  -- * Tar entry and associated types
  Entry(..),
  entryPath,
  EntryContent(..),
  Ownership(..),
  FileSize,
  Permissions,
  EpochTime,
  DevMajor,
  DevMinor,
  TypeCode,
  Format(..),
  buildTreeRefTypeCode,
  buildTreeSnapshotTypeCode,
  isBuildTreeRefTypeCode,
  entrySizeInBlocks,
  entrySizeInBytes,

  -- * Constructing simple entry values
  simpleEntry,
  fileEntry,
  directoryEntry,

  -- * TarPath type
  TarPath,
  toTarPath,
  fromTarPath,

  -- ** Sequences of tar entries
  Entries(..),
  foldrEntries,
  foldlEntries,
  unfoldrEntries,
  mapEntries,
  filterEntries,
  entriesIndex,

  ) where

import Data.Char     (ord)
import Data.Int      (Int64)
import Data.Bits     (Bits, shiftL, testBit)
import Data.List     (foldl')
import Numeric       (readOct, showOct)
import Control.Applicative (Applicative(..))
import Control.Monad (MonadPlus(mplus), when, ap, liftM)
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BS.Char8
import Data.ByteString.Lazy (ByteString)
import qualified Codec.Compression.GZip as GZip
import qualified Distribution.Client.GZipUtils as GZipUtils

import System.FilePath
         ( (</>) )
import qualified System.FilePath         as FilePath.Native
import qualified System.FilePath.Windows as FilePath.Windows
import qualified System.FilePath.Posix   as FilePath.Posix
import System.Directory
         ( getDirectoryContents, doesDirectoryExist
         , getPermissions, createDirectoryIfMissing, copyFile )
import qualified System.Directory as Permissions
         ( Permissions(executable) )
import Distribution.Client.Compat.FilePerms
         ( setFileExecutable )
import System.Posix.Types
         ( FileMode )
import Distribution.Client.Compat.Time
         ( EpochTime, getModTime )
import System.IO
         ( IOMode(ReadMode), openBinaryFile, hFileSize )
import System.IO.Unsafe (unsafeInterleaveIO)

import Prelude hiding (read)


--
-- * High level operations
--

createTarGzFile :: FilePath  -- ^ Full Tarball path
                -> FilePath  -- ^ Base directory
                -> FilePath  -- ^ Directory to archive, relative to base dir
                -> IO ()
createTarGzFile tar base dir =
  BS.writeFile tar . GZip.compress . write =<< pack base [dir]

extractTarGzFile :: FilePath -- ^ Destination directory
                 -> FilePath -- ^ Expected subdir (to check for tarbombs)
                 -> FilePath -- ^ Tarball
                -> IO ()
extractTarGzFile dir expected tar =
  unpack dir . checkTarbomb expected . read
  . GZipUtils.maybeDecompress =<< BS.readFile tar

--
-- * Entry type
--

type FileSize  = Int64
type DevMajor  = Int
type DevMinor  = Int
type TypeCode  = Char
type Permissions = FileMode

-- | Tar archive entry.
--
data Entry = Entry {

    -- | The path of the file or directory within the archive. This is in a
    -- tar-specific form. Use 'entryPath' to get a native 'FilePath'.
    entryTarPath :: !TarPath,

    -- | The real content of the entry. For 'NormalFile' this includes the
    -- file data. An entry usually contains a 'NormalFile' or a 'Directory'.
    entryContent :: !EntryContent,

    -- | File permissions (Unix style file mode).
    entryPermissions :: !Permissions,

    -- | The user and group to which this file belongs.
    entryOwnership :: !Ownership,

    -- | The time the file was last modified.
    entryTime :: !EpochTime,

    -- | The tar format the archive is using.
    entryFormat :: !Format
  }

-- | Type code for the local build tree reference entry type. We don't use the
-- symbolic link entry type because it allows only 100 ASCII characters for the
-- path.
buildTreeRefTypeCode :: TypeCode
buildTreeRefTypeCode = 'C'

-- | Type code for the local build tree snapshot entry type.
buildTreeSnapshotTypeCode :: TypeCode
buildTreeSnapshotTypeCode = 'S'

-- | Is this a type code for a build tree reference?
isBuildTreeRefTypeCode :: TypeCode -> Bool
isBuildTreeRefTypeCode typeCode
  | (typeCode == buildTreeRefTypeCode
     || typeCode == buildTreeSnapshotTypeCode) = True
  | otherwise                                  = False

-- | Native 'FilePath' of the file or directory within the archive.
--
entryPath :: Entry -> FilePath
entryPath = fromTarPath . entryTarPath

-- | Return the size of an entry in bytes.
entrySizeInBytes :: Entry -> FileSize
entrySizeInBytes = (*512) . fromIntegral . entrySizeInBlocks

-- | Return the number of blocks in an entry.
entrySizeInBlocks :: Entry -> Int
entrySizeInBlocks entry = 1 + case entryContent entry of
  NormalFile     _   size -> bytesToBlocks size
  OtherEntryType _ _ size -> bytesToBlocks size
  _                       -> 0
  where
    bytesToBlocks s = 1 + ((fromIntegral s - 1) `div` 512)

-- | The content of a tar archive entry, which depends on the type of entry.
--
-- Portable archives should contain only 'NormalFile' and 'Directory'.
--
data EntryContent = NormalFile      ByteString !FileSize
                  | Directory
                  | SymbolicLink    !LinkTarget
                  | HardLink        !LinkTarget
                  | CharacterDevice !DevMajor !DevMinor
                  | BlockDevice     !DevMajor !DevMinor
                  | NamedPipe
                  | OtherEntryType  !TypeCode ByteString !FileSize

data Ownership = Ownership {
    -- | The owner user name. Should be set to @\"\"@ if unknown.
    ownerName :: String,

    -- | The owner group name. Should be set to @\"\"@ if unknown.
    groupName :: String,

    -- | Numeric owner user id. Should be set to @0@ if unknown.
    ownerId :: !Int,

    -- | Numeric owner group id. Should be set to @0@ if unknown.
    groupId :: !Int
  }

-- | There have been a number of extensions to the tar file format over the
-- years. They all share the basic entry fields and put more meta-data in
-- different extended headers.
--
data Format =

     -- | This is the classic Unix V7 tar format. It does not support owner and
     -- group names, just numeric Ids. It also does not support device numbers.
     V7Format

     -- | The \"USTAR\" format is an extension of the classic V7 format. It was
     -- later standardised by POSIX. It has some restructions but is the most
     -- portable format.
     --
   | UstarFormat

     -- | The GNU tar implementation also extends the classic V7 format, though
     -- in a slightly different way from the USTAR format. In general for new
     -- archives the standard USTAR/POSIX should be used.
     --
   | GnuFormat
  deriving Eq

-- | @rw-r--r--@ for normal files
ordinaryFilePermissions :: Permissions
ordinaryFilePermissions   = 0o0644

-- | @rwxr-xr-x@ for executable files
executableFilePermissions :: Permissions
executableFilePermissions = 0o0755

-- | @rwxr-xr-x@ for directories
directoryPermissions :: Permissions
directoryPermissions  = 0o0755

isExecutable :: Permissions -> Bool
isExecutable p = testBit p 0 || testBit p 6 -- user or other exectuable

-- | An 'Entry' with all default values except for the file name and type. It
-- uses the portable USTAR/POSIX format (see 'UstarHeader').
--
-- You can use this as a basis and override specific fields, eg:
--
-- > (emptyEntry name HardLink) { linkTarget = target }
--
simpleEntry :: TarPath -> EntryContent -> Entry
simpleEntry tarpath content = Entry {
    entryTarPath     = tarpath,
    entryContent     = content,
    entryPermissions = case content of
                         Directory -> directoryPermissions
                         _         -> ordinaryFilePermissions,
    entryOwnership   = Ownership "" "" 0 0,
    entryTime        = 0,
    entryFormat      = UstarFormat
  }

-- | A tar 'Entry' for a file.
--
-- Entry  fields such as file permissions and ownership have default values.
--
-- You can use this as a basis and override specific fields. For example if you
-- need an executable file you could use:
--
-- > (fileEntry name content) { fileMode = executableFileMode }
--
fileEntry :: TarPath -> ByteString -> Entry
fileEntry name fileContent =
  simpleEntry name (NormalFile fileContent (BS.length fileContent))

-- | A tar 'Entry' for a directory.
--
-- Entry fields such as file permissions and ownership have default values.
--
directoryEntry :: TarPath -> Entry
directoryEntry name = simpleEntry name Directory

--
-- * Tar paths
--

-- | The classic tar format allowed just 100 characters for the file name. The
-- USTAR format extended this with an extra 155 characters, however it uses a
-- complex method of splitting the name between the two sections.
--
-- Instead of just putting any overflow into the extended area, it uses the
-- extended area as a prefix. The aggravating insane bit however is that the
-- prefix (if any) must only contain a directory prefix. That is the split
-- between the two areas must be on a directory separator boundary. So there is
-- no simple calculation to work out if a file name is too long. Instead we
-- have to try to find a valid split that makes the name fit in the two areas.
--
-- The rationale presumably was to make it a bit more compatible with old tar
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
  deriving (Eq, Ord)

-- | Convert a 'TarPath' to a native 'FilePath'.
--
-- The native 'FilePath' will use the native directory separator but it is not
-- otherwise checked for validity or sanity. In particular:
--
-- * The tar path may be invalid as a native path, eg the filename @\"nul\"@ is
--   not valid on Windows.
--
-- * The tar path may be an absolute path or may contain @\"..\"@ components.
--   For security reasons this should not usually be allowed, but it is your
--   responsibility to check for these conditions (eg using 'checkSecurity').
--
fromTarPath :: TarPath -> FilePath
fromTarPath (TarPath name prefix) = adjustDirectory $
  FilePath.Native.joinPath $ FilePath.Posix.splitDirectories prefix
                          ++ FilePath.Posix.splitDirectories name
  where
    adjustDirectory | FilePath.Posix.hasTrailingPathSeparator name
                    = FilePath.Native.addTrailingPathSeparator
                    | otherwise = id

-- | Convert a native 'FilePath' to a 'TarPath'.
--
-- The conversion may fail if the 'FilePath' is too long. See 'TarPath' for a
-- description of the problem with splitting long 'FilePath's.
--
toTarPath :: Bool -- ^ Is the path for a directory? This is needed because for
                  -- directories a 'TarPath' must always use a trailing @\/@.
          -> FilePath -> Either String TarPath
toTarPath isDir = splitLongPath
                . addTrailingSep
                . FilePath.Posix.joinPath
                . FilePath.Native.splitDirectories
  where
    addTrailingSep | isDir     = FilePath.Posix.addTrailingPathSeparator
                   | otherwise = id

-- | Take a sanitized path, split on directory separators and try to pack it
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
      Right (_     , _ : _)  -> Left "File name too long (cannot split)"
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

-- | The tar format allows just 100 ASCII characters for the 'SymbolicLink' and
-- 'HardLink' entry types.
--
newtype LinkTarget = LinkTarget FilePath
  deriving (Eq, Ord)

-- | Convert a tar 'LinkTarget' to a native 'FilePath'.
--
fromLinkTarget :: LinkTarget -> FilePath
fromLinkTarget (LinkTarget path) = adjustDirectory $
  FilePath.Native.joinPath $ FilePath.Posix.splitDirectories path
  where
    adjustDirectory | FilePath.Posix.hasTrailingPathSeparator path
                    = FilePath.Native.addTrailingPathSeparator
                    | otherwise = id

--
-- * Entries type
--

-- | A tar archive is a sequence of entries.
data Entries = Next Entry Entries
             | Done
             | Fail String

unfoldrEntries :: (a -> Either String (Maybe (Entry, a))) -> a -> Entries
unfoldrEntries f = unfold
  where
    unfold x = case f x of
      Left err             -> Fail err
      Right Nothing        -> Done
      Right (Just (e, x')) -> Next e (unfold x')

foldrEntries :: (Entry -> a -> a) -> a -> (String -> a) -> Entries -> a
foldrEntries next done fail' = fold
  where
    fold (Next e es) = next e (fold es)
    fold Done        = done
    fold (Fail err)  = fail' err

foldlEntries :: (a -> Entry -> a) -> a -> Entries -> Either String a
foldlEntries f = fold
  where
    fold a (Next e es) = (fold $! f a e) es
    fold a Done        = Right a
    fold _ (Fail err)  = Left err

mapEntries :: (Entry -> Entry) -> Entries -> Entries
mapEntries f = foldrEntries (Next . f) Done Fail

filterEntries :: (Entry -> Bool) -> Entries -> Entries
filterEntries p =
  foldrEntries
    (\entry rest -> if p entry
                      then Next entry rest
                      else rest)
    Done Fail

checkEntries :: (Entry -> Maybe String) -> Entries -> Entries
checkEntries checkEntry =
  foldrEntries
    (\entry rest -> case checkEntry entry of
                      Nothing  -> Next entry rest
                      Just err -> Fail err)
    Done Fail

entriesIndex :: Entries -> Either String (Map.Map TarPath Entry)
entriesIndex = foldlEntries (\m e -> Map.insert (entryTarPath e) e m) Map.empty

--
-- * Checking
--

-- | This function checks a sequence of tar entries for file name security
-- problems. It checks that:
--
-- * file paths are not absolute
--
-- * file paths do not contain any path components that are \"@..@\"
--
-- * file names are valid
--
-- These checks are from the perspective of the current OS. That means we check
-- for \"@C:\blah@\" files on Windows and \"\/blah\" files on unix. For archive
-- entry types 'HardLink' and 'SymbolicLink' the same checks are done for the
-- link target. A failure in any entry terminates the sequence of entries with
-- an error.
--
checkSecurity :: Entries -> Entries
checkSecurity = checkEntries checkEntrySecurity

checkTarbomb :: FilePath -> Entries -> Entries
checkTarbomb expectedTopDir = checkEntries (checkEntryTarbomb expectedTopDir)

checkEntrySecurity :: Entry -> Maybe String
checkEntrySecurity entry = case entryContent entry of
    HardLink     link -> check (entryPath entry)
                 `mplus` check (fromLinkTarget link)
    SymbolicLink link -> check (entryPath entry)
                 `mplus` check (fromLinkTarget link)
    _                 -> check (entryPath entry)

  where
    check name
      | not (FilePath.Native.isRelative name)
      = Just $ "Absolute file name in tar archive: " ++ show name

      | not (FilePath.Native.isValid name)
      = Just $ "Invalid file name in tar archive: " ++ show name

      | ".." `elem` FilePath.Native.splitDirectories name
      = Just $ "Invalid file name in tar archive: " ++ show name

      | otherwise = Nothing

checkEntryTarbomb :: FilePath -> Entry -> Maybe String
checkEntryTarbomb _ entry | nonFilesystemEntry = Nothing
  where
    -- Ignore some special entries we will not unpack anyway
    nonFilesystemEntry =
      case entryContent entry of
        OtherEntryType 'g' _ _ -> True --PAX global header
        OtherEntryType 'x' _ _ -> True --PAX individual header
        _                      -> False

checkEntryTarbomb expectedTopDir entry =
  case FilePath.Native.splitDirectories (entryPath entry) of
    (topDir:_) | topDir == expectedTopDir -> Nothing
    s -> Just $ "File in tar archive is not in the expected directory. "
             ++ "Expected: " ++ show expectedTopDir
             ++ " but got the following hierarchy: "
             ++ show s


--
-- * Reading
--

read :: ByteString -> Entries
read = unfoldrEntries getEntry

getEntry :: ByteString -> Either String (Maybe (Entry, ByteString))
getEntry bs
  | BS.length header < 512 = Left "truncated tar archive"

  -- Tar files end with at least two blocks of all '0'. Checking this serves
  -- two purposes. It checks the format but also forces the tail of the data
  -- which is necessary to close the file if it came from a lazily read file.
  | BS.head bs == 0 = case BS.splitAt 1024 bs of
      (end, trailing)
        | BS.length end /= 1024        -> Left "short tar trailer"
        | not (BS.all (== 0) end)      -> Left "bad tar trailer"
        | not (BS.all (== 0) trailing) -> Left "tar file has trailing junk"
        | otherwise                    -> Right Nothing

  | otherwise  = partial $ do

  case (chksum_, format_) of
    (Ok chksum, _   ) | correctChecksum header chksum -> return ()
    (Ok _,      Ok _) -> fail "tar checksum error"
    _                 -> fail "data is not in tar format"

  -- These fields are partial, have to check them
  format   <- format_;   mode     <- mode_;
  uid      <- uid_;      gid      <- gid_;
  size     <- size_;     mtime    <- mtime_;
  devmajor <- devmajor_; devminor <- devminor_;

  let content = BS.take size (BS.drop 512 bs)
      padding = (512 - size) `mod` 512
      bs'     = BS.drop (512 + size + padding) bs

      entry = Entry {
        entryTarPath     = TarPath name prefix,
        entryContent     = case typecode of
                   '\0' -> NormalFile      content size
                   '0'  -> NormalFile      content size
                   '1'  -> HardLink        (LinkTarget linkname)
                   '2'  -> SymbolicLink    (LinkTarget linkname)
                   '3'  -> CharacterDevice devmajor devminor
                   '4'  -> BlockDevice     devmajor devminor
                   '5'  -> Directory
                   '6'  -> NamedPipe
                   '7'  -> NormalFile      content size
                   _    -> OtherEntryType  typecode content size,
        entryPermissions = mode,
        entryOwnership   = Ownership uname gname uid gid,
        entryTime        = mtime,
        entryFormat      = format
    }

  return (Just (entry, bs'))

  where
   header = BS.take 512 bs

   name       = getString   0 100 header
   mode_      = getOct    100   8 header
   uid_       = getOct    108   8 header
   gid_       = getOct    116   8 header
   size_      = getOct    124  12 header
   mtime_     = getOct    136  12 header
   chksum_    = getOct    148   8 header
   typecode   = getByte   156     header
   linkname   = getString 157 100 header
   magic      = getChars  257   8 header
   uname      = getString 265  32 header
   gname      = getString 297  32 header
   devmajor_  = getOct    329   8 header
   devminor_  = getOct    337   8 header
   prefix     = getString 345 155 header
-- trailing   = getBytes  500  12 header

   format_ = case magic of
    "\0\0\0\0\0\0\0\0" -> return V7Format
    "ustar\NUL00"      -> return UstarFormat
    "ustar  \NUL"      -> return GnuFormat
    _                  -> fail "tar entry not in a recognised format"

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

getOct :: (Integral a, Bits a) => Int64 -> Int64 -> ByteString -> Partial a
getOct off len header
  | BS.head bytes == 128 = parseBinInt (BS.unpack (BS.tail bytes))
  | null octstr          = return 0
  | otherwise            = case readOct octstr of
               [(x,[])] -> return x
               _        -> fail "tar header is malformed (bad numeric encoding)"
  where
    bytes  = getBytes off len header
    octstr = BS.Char8.unpack
           . BS.Char8.takeWhile (\c -> c /= '\NUL' && c /= ' ')
           . BS.Char8.dropWhile (== ' ')
           $ bytes

    -- Some tar programs switch into a binary format when they try to represent
    -- field values that will not fit in the required width when using the text
    -- octal format. In particular, the UID/GID fields can only hold up to 2^21
    -- while in the binary format can hold up to 2^32. The binary format uses
    -- '\128' as the header which leaves 7 bytes. Only the last 4 are used.
    parseBinInt [0, 0, 0, byte3, byte2, byte1, byte0] =
      return $! shiftL (fromIntegral byte3) 24
              + shiftL (fromIntegral byte2) 16
              + shiftL (fromIntegral byte1) 8
              + shiftL (fromIntegral byte0) 0
    parseBinInt _ = fail "tar header uses non-standard number encoding"

getBytes :: Int64 -> Int64 -> ByteString -> ByteString
getBytes off len = BS.take len . BS.drop off

getByte :: Int64 -> ByteString -> Char
getByte off bs = BS.Char8.index bs off

getChars :: Int64 -> Int64 -> ByteString -> String
getChars off len = BS.Char8.unpack . getBytes off len

getString :: Int64 -> Int64 -> ByteString -> String
getString off len = BS.Char8.unpack . BS.Char8.takeWhile (/='\0')
                    . getBytes off len

data Partial a = Error String | Ok a

partial :: Partial a -> Either String a
partial (Error msg) = Left msg
partial (Ok x)      = Right x

instance Functor Partial where
    fmap          = liftM

instance Applicative Partial where
    pure          = return
    (<*>)         = ap

instance Monad Partial where
    return        = Ok
    Error m >>= _ = Error m
    Ok    x >>= k = k x
    fail          = Error

--
-- * Writing
--

-- | Create the external representation of a tar archive by serialising a list
-- of tar entries.
--
-- * The conversion is done lazily.
--
write :: [Entry] -> ByteString
write es = BS.concat $ map putEntry es ++ [BS.replicate (512*2) 0]

-- | Same as 'write', but for 'Entries'.
writeEntries :: Entries -> ByteString
writeEntries entries = BS.concat $ foldrEntries (\e res -> putEntry e : res)
                       [BS.replicate (512*2) 0] error entries

putEntry :: Entry -> ByteString
putEntry entry = case entryContent entry of
  NormalFile       content size -> BS.concat [ header, content, padding size ]
  OtherEntryType _ content size -> BS.concat [ header, content, padding size ]
  _                             -> header
  where
    header       = putHeader entry
    padding size = BS.replicate paddingSize 0
      where paddingSize = fromIntegral (negate size `mod` 512)

putHeader :: Entry -> ByteString
putHeader entry =
     BS.concat [ BS.take 148 block
               , BS.Char8.pack $ putOct 7 checksum
               , BS.Char8.singleton ' '
               , BS.drop 156 block ]
  where
    -- putHeaderNoChkSum returns a String, so we convert it to the final
    -- representation before calculating the checksum.
    block    = BS.Char8.pack . putHeaderNoChkSum $ entry
    checksum = BS.Char8.foldl' (\x y -> x + ord y) 0 block

putHeaderNoChkSum :: Entry -> String
putHeaderNoChkSum Entry {
    entryTarPath     = TarPath name prefix,
    entryContent     = content,
    entryPermissions = permissions,
    entryOwnership   = ownership,
    entryTime        = modTime,
    entryFormat      = format
  } =

  concat
    [ putString  100 $ name
    , putOct       8 $ permissions
    , putOct       8 $ ownerId ownership
    , putOct       8 $ groupId ownership
    , putOct      12 $ contentSize
    , putOct      12 $ modTime
    , fill         8 $ ' ' -- dummy checksum
    , putChar8       $ typeCode
    , putString  100 $ linkTarget
    ] ++
  case format of
  V7Format    ->
      fill 255 '\NUL'
  UstarFormat -> concat
    [ putString    8 $ "ustar\NUL00"
    , putString   32 $ ownerName ownership
    , putString   32 $ groupName ownership
    , putOct       8 $ deviceMajor
    , putOct       8 $ deviceMinor
    , putString  155 $ prefix
    , fill        12 $ '\NUL'
    ]
  GnuFormat -> concat
    [ putString    8 $ "ustar  \NUL"
    , putString   32 $ ownerName ownership
    , putString   32 $ groupName ownership
    , putGnuDev    8 $ deviceMajor
    , putGnuDev    8 $ deviceMinor
    , putString  155 $ prefix
    , fill        12 $ '\NUL'
    ]
  where
    (typeCode, contentSize, linkTarget,
     deviceMajor, deviceMinor) = case content of
       NormalFile      _ size            -> ('0' , size, [],   0,     0)
       Directory                         -> ('5' , 0,    [],   0,     0)
       SymbolicLink    (LinkTarget link) -> ('2' , 0,    link, 0,     0)
       HardLink        (LinkTarget link) -> ('1' , 0,    link, 0,     0)
       CharacterDevice major minor       -> ('3' , 0,    [],   major, minor)
       BlockDevice     major minor       -> ('4' , 0,    [],   major, minor)
       NamedPipe                         -> ('6' , 0,    [],   0,     0)
       OtherEntryType  code _ size       -> (code, size, [],   0,     0)

    putGnuDev w n = case content of
      CharacterDevice _ _ -> putOct w n
      BlockDevice     _ _ -> putOct w n
      _                   -> replicate w '\NUL'

-- * TAR format primitive output

type FieldWidth = Int

putString :: FieldWidth -> String -> String
putString n s = take n s ++ fill (n - length s) '\NUL'

--TODO: check integer widths, eg for large file sizes
putOct :: (Show a, Integral a) => FieldWidth -> a -> String
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
unpack baseDir entries = unpackEntries [] (checkSecurity entries)
                     >>= emulateLinks

  where
    -- We're relying here on 'checkSecurity' to make sure we're not scribbling
    -- files all over the place.

    unpackEntries _     (Fail err)      = fail err
    unpackEntries links Done            = return links
    unpackEntries links (Next entry es) = case entryContent entry of
      NormalFile file _ -> extractFile entry path file
                        >> unpackEntries links es
      Directory         -> extractDir path
                        >> unpackEntries links es
      HardLink     link -> (unpackEntries $! saveLink path link links) es
      SymbolicLink link -> (unpackEntries $! saveLink path link links) es
      _                 -> unpackEntries links es --ignore other file types
      where
        path = entryPath entry

    extractFile entry path content = do
      -- Note that tar archives do not make sure each directory is created
      -- before files they contain, indeed we may have to create several
      -- levels of directory.
      createDirectoryIfMissing True absDir
      BS.writeFile absPath content
      when (isExecutable (entryPermissions entry))
           (setFileExecutable absPath)
      where
        absDir  = baseDir </> FilePath.Native.takeDirectory path
        absPath = baseDir </> path

    extractDir path = createDirectoryIfMissing True (baseDir </> path)

    saveLink path link links = seq (length path)
                             $ seq (length link')
                             $ (path, link'):links
      where link' = fromLinkTarget link

    emulateLinks = mapM_ $ \(relPath, relLinkTarget) ->
      let absPath   = baseDir </> relPath
          absTarget = FilePath.Native.takeDirectory absPath </> relLinkTarget
       in copyFile absTarget absPath

--
-- * Packing
--

pack :: FilePath   -- ^ Base directory
     -> [FilePath] -- ^ Files and directories to pack, relative to the base dir
     -> IO [Entry]
pack baseDir paths0 = preparePaths baseDir paths0 >>= packPaths baseDir

preparePaths :: FilePath -> [FilePath] -> IO [FilePath]
preparePaths baseDir paths =
  fmap concat $ interleave
    [ do isDir  <- doesDirectoryExist (baseDir </> path)
         if isDir
           then do entries <- getDirectoryContentsRecursive (baseDir </> path)
                   return (FilePath.Native.addTrailingPathSeparator path
                         : map (path </>) entries)
           else return [path]
    | path <- paths ]

packPaths :: FilePath -> [FilePath] -> IO [Entry]
packPaths baseDir paths =
  interleave
    [ do tarpath <- either fail return (toTarPath isDir relpath)
         if isDir then packDirectoryEntry filepath tarpath
                  else packFileEntry      filepath tarpath
    | relpath <- paths
    , let isDir    = FilePath.Native.hasTrailingPathSeparator filepath
          filepath = baseDir </> relpath ]

interleave :: [IO a] -> IO [a]
interleave = unsafeInterleaveIO . go
  where
    go []     = return []
    go (x:xs) = do
      x'  <- x
      xs' <- interleave xs
      return (x':xs')

packFileEntry :: FilePath -- ^ Full path to find the file on the local disk
              -> TarPath  -- ^ Path to use for the tar Entry in the archive
              -> IO Entry
packFileEntry filepath tarpath = do
  mtime   <- getModTime filepath
  perms   <- getPermissions filepath
  file    <- openBinaryFile filepath ReadMode
  size    <- hFileSize file
  content <- BS.hGetContents file
  return (simpleEntry tarpath (NormalFile content (fromIntegral size))) {
    entryPermissions = if Permissions.executable perms
                         then executableFilePermissions
                         else ordinaryFilePermissions,
    entryTime = mtime
  }

packDirectoryEntry :: FilePath -- ^ Full path to find the file on the local disk
                   -> TarPath  -- ^ Path to use for the tar Entry in the archive
                   -> IO Entry
packDirectoryEntry filepath tarpath = do
  mtime   <- getModTime filepath
  return (directoryEntry tarpath) {
    entryTime = mtime
  }

getDirectoryContentsRecursive :: FilePath -> IO [FilePath]
getDirectoryContentsRecursive dir0 =
  fmap tail (recurseDirectories dir0 [""])

recurseDirectories :: FilePath -> [FilePath] -> IO [FilePath]
recurseDirectories _    []         = return []
recurseDirectories base (dir:dirs) = unsafeInterleaveIO $ do
  (files, dirs') <- collect [] [] =<< getDirectoryContents (base </> dir)

  files' <- recurseDirectories base (dirs' ++ dirs)
  return (dir : files ++ files')

  where
    collect files dirs' []              = return (reverse files, reverse dirs')
    collect files dirs' (entry:entries) | ignore entry
                                        = collect files dirs' entries
    collect files dirs' (entry:entries) = do
      let dirEntry  = dir </> entry
          dirEntry' = FilePath.Native.addTrailingPathSeparator dirEntry
      isDirectory <- doesDirectoryExist (base </> dirEntry)
      if isDirectory
        then collect files (dirEntry':dirs') entries
        else collect (dirEntry:files) dirs' entries

    ignore ['.']      = True
    ignore ['.', '.'] = True
    ignore _          = False
