{-# LANGUAGE ForeignFunctionInterface, ScopedTypeVariables, CPP #-}

module Distribution.Client.Utils
  ( MergeResult(..)
  , mergeBy, duplicates, duplicatesBy
  , readMaybe
  , inDir, withEnv, withEnvOverrides
  , logDirChange, withExtraPathEnv
  , determineNumJobs, numberOfProcessors
  , removeExistingFile
  , withTempFileName
  , makeAbsoluteToCwd
  , makeRelativeToCwd, makeRelativeToDir
  , makeRelativeCanonical
  , filePathToByteString
  , byteStringToFilePath, tryCanonicalizePath
  , canonicalizePathNoThrow
  , moreRecentFile, existsAndIsMoreRecentThan
  , tryFindAddSourcePackageDesc
  , tryFindPackageDesc
  , findOpenProgramLocation
  , relaxEncodingErrors
  , ProgressPhase (..)
  , progressMessage
  , pvpize
  , incVersion
  , getCurrentYear
  , listFilesRecursive
  , listFilesInside
  , safeRead
  ) where

import Prelude ()
import Distribution.Client.Compat.Prelude

import Distribution.Compat.Environment
import Distribution.Compat.Time        ( getModTime )
import Distribution.Simple.Setup       ( Flag(..) )
import Distribution.Version
import Distribution.Simple.Utils       ( die', findPackageDesc, noticeNoWrap )
import Distribution.System             ( Platform (..), OS(..))
import qualified Data.ByteString.Lazy as BS
import Data.Bits
         ( (.|.), shiftL, shiftR )
import System.FilePath
import Control.Monad
         ( zipWithM_ )
import Data.List
         ( groupBy )
import Foreign.C.Types ( CInt(..) )
import qualified Control.Exception as Exception
         ( finally )
import qualified Control.Exception.Safe as Safe
         ( bracket )
import System.Directory
         ( canonicalizePath, doesFileExist, findExecutable, getCurrentDirectory
         , removeFile, setCurrentDirectory, getDirectoryContents, doesDirectoryExist )
import System.IO
         ( Handle, hClose, openTempFile
         , hGetEncoding, hSetEncoding
         )
import System.IO.Unsafe ( unsafePerformIO )

import GHC.IO.Encoding
         ( recover, TextEncoding(TextEncoding) )
import GHC.IO.Encoding.Failure
         ( recoverEncode, CodingFailureMode(TransliterateCodingFailure) )
import Data.Time.Clock.POSIX (getCurrentTime)
import Data.Time.LocalTime (getCurrentTimeZone, localDay)
import Data.Time (utcToLocalTime)
import Data.Time.Calendar (toGregorian)
#if defined(mingw32_HOST_OS) || MIN_VERSION_directory(1,2,3)
import qualified System.Directory as Dir
import qualified System.IO.Error as IOError
#endif


-- | Generic merging utility. For sorted input lists this is a full outer join.
--
mergeBy :: forall a b. (a -> b -> Ordering) -> [a] -> [b] -> [MergeResult a b]
mergeBy cmp = merge
  where
    merge               :: [a] -> [b] -> [MergeResult a b]
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

duplicatesBy :: forall a. (a -> a -> Ordering) -> [a] -> [[a]]
duplicatesBy cmp = filter moreThanOne . groupBy eq . sortBy cmp
  where
    eq :: a -> a -> Bool
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

-- | A variant of 'withTempFile' that only gives us the file name, and while
-- it will clean up the file afterwards, it's lenient if the file is
-- moved\/deleted.
--
withTempFileName :: FilePath
                 -> String
                 -> (FilePath -> IO a) -> IO a
withTempFileName tmpDir template action =
  Safe.bracket
    (openTempFile tmpDir template)
    (\(name, _) -> removeExistingFile name)
    (\(name, h) -> hClose h >> action name)

-- | Executes the action in the specified directory.
--
-- Warning: This operation is NOT thread-safe, because current
-- working directory is a process-global concept.
inDir :: Maybe FilePath -> IO a -> IO a
inDir Nothing m = m
inDir (Just d) m = do
  old <- getCurrentDirectory
  setCurrentDirectory d
  m `Exception.finally` setCurrentDirectory old

-- | Executes the action with an environment variable set to some
-- value.
--
-- Warning: This operation is NOT thread-safe, because current
-- environment is a process-global concept.
withEnv :: String -> String -> IO a -> IO a
withEnv k v m = do
  mb_old <- lookupEnv k
  setEnv k v
  m `Exception.finally` (case mb_old of
    Nothing -> unsetEnv k
    Just old -> setEnv k old)

-- | Executes the action with a list of environment variables and
-- corresponding overrides, where
--
-- * @'Just' v@ means \"set the environment variable's value to @v@\".
-- * 'Nothing' means \"unset the environment variable\".
--
-- Warning: This operation is NOT thread-safe, because current
-- environment is a process-global concept.
withEnvOverrides :: [(String, Maybe FilePath)] -> IO a -> IO a
withEnvOverrides overrides m = do
  mb_olds <- traverse lookupEnv envVars
  traverse_ (uncurry update) overrides
  m `Exception.finally` zipWithM_ update envVars mb_olds
   where
    envVars :: [String]
    envVars = map fst overrides

    update :: String -> Maybe FilePath -> IO ()
    update var Nothing    = unsetEnv var
    update var (Just val) = setEnv var val

-- | Executes the action, increasing the PATH environment
-- in some way
--
-- Warning: This operation is NOT thread-safe, because the
-- environment variables are a process-global concept.
withExtraPathEnv :: [FilePath] -> IO a -> IO a
withExtraPathEnv paths m = do
  oldPathSplit <- getSearchPath
  let newPath :: String
      newPath = mungePath $ intercalate [searchPathSeparator] (paths ++ oldPathSplit)
      oldPath :: String
      oldPath = mungePath $ intercalate [searchPathSeparator] oldPathSplit
      -- TODO: This is a horrible hack to work around the fact that
      -- setEnv can't take empty values as an argument
      mungePath p | p == ""   = "/dev/null"
                  | otherwise = p
  setEnv "PATH" newPath
  m `Exception.finally` setEnv "PATH" oldPath

-- | Log directory change in 'make' compatible syntax
logDirChange :: (String -> IO ()) -> Maybe FilePath -> IO a -> IO a
logDirChange _ Nothing m = m
logDirChange l (Just d) m = do
  l $ "cabal: Entering directory '" ++ d ++ "'\n"
  m `Exception.finally`
    (l $ "cabal: Leaving directory '" ++ d ++ "'\n")

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

-- | Given a path (relative or absolute), make it relative to the current
-- directory, including using @../..@ if necessary.
makeRelativeToCwd :: FilePath -> IO FilePath
makeRelativeToCwd path =
    makeRelativeCanonical <$> canonicalizePath path <*> getCurrentDirectory

-- | Given a path (relative or absolute), make it relative to the given
-- directory, including using @../..@ if necessary.
makeRelativeToDir :: FilePath -> FilePath -> IO FilePath
makeRelativeToDir path dir =
    makeRelativeCanonical <$> canonicalizePath path <*> canonicalizePath dir

-- | Given a canonical absolute path and canonical absolute dir, make the path
-- relative to the directory, including using @../..@ if necessary. Returns
-- the original absolute path if it is not on the same drive as the given dir.
makeRelativeCanonical :: FilePath -> FilePath -> FilePath
makeRelativeCanonical path dir
  | takeDrive path /= takeDrive dir = path
  | otherwise                       = go (splitPath path) (splitPath dir)
  where
    go (p:ps) (d:ds) | p' == d' = go ps ds
      where (p', d') = (dropTrailingPathSeparator p, dropTrailingPathSeparator d)
    go    []     []             = "./"
    go    ps     ds             = joinPath (replicate (length ds) ".." ++ ps)

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

-- | Workaround for the inconsistent behaviour of 'canonicalizePath'. Always
-- throws an error if the path refers to a non-existent file.
tryCanonicalizePath :: FilePath -> IO FilePath
tryCanonicalizePath path = do
  ret <- canonicalizePath path
#if defined(mingw32_HOST_OS) || MIN_VERSION_directory(1,2,3)
  exists <- liftM2 (||) (doesFileExist ret) (Dir.doesDirectoryExist ret)
  unless exists $
    IOError.ioError $ IOError.mkIOError IOError.doesNotExistErrorType "canonicalizePath"
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
  maybeEncoding <- hGetEncoding handle
  case maybeEncoding of
    Just (TextEncoding name decoder encoder) | not ("UTF" `isPrefixOf` name) ->
      let relax x = x { recover = recoverEncode TransliterateCodingFailure }
      in hSetEncoding handle (TextEncoding name decoder (fmap relax encoder))
    _ ->
      return ()

-- |Like 'tryFindPackageDesc', but with error specific to add-source deps.
tryFindAddSourcePackageDesc :: Verbosity -> FilePath -> String -> IO FilePath
tryFindAddSourcePackageDesc verbosity depPath err = tryFindPackageDesc verbosity depPath $
    err ++ "\n" ++ "Failed to read cabal file of add-source dependency: "
    ++ depPath

-- |Try to find a @.cabal@ file, in directory @depPath@. Fails if one cannot be
-- found, with @err@ prefixing the error message. This function simply allows
-- us to give a more descriptive error than that provided by @findPackageDesc@.
tryFindPackageDesc :: Verbosity -> FilePath -> String -> IO FilePath
tryFindPackageDesc verbosity depPath err = do
    errOrCabalFile <- findPackageDesc depPath
    case errOrCabalFile of
        Right file -> return file
        Left _ -> die' verbosity err

findOpenProgramLocation :: Platform -> IO (Either String FilePath)
findOpenProgramLocation (Platform _ os) =
  let
    locate name = do
      exe <- findExecutable name
      case exe of
        Just s -> pure (Right s)
        Nothing -> pure (Left ("Couldn't find file-opener program `" <> name <> "`"))
    xdg = locate "xdg-open"
  in case os of
    Windows -> pure (Right "start")
    OSX -> locate "open"
    Linux -> xdg
    FreeBSD -> xdg
    OpenBSD -> xdg
    NetBSD -> xdg
    DragonFly -> xdg
    _ -> pure (Left ("Couldn't determine file-opener program for " <> show os))


-- | Phase of building a dependency. Represents current status of package
-- dependency processing. See #4040 for details.
data ProgressPhase
    = ProgressDownloading
    | ProgressDownloaded
    | ProgressStarting
    | ProgressBuilding
    | ProgressHaddock
    | ProgressInstalling
    | ProgressCompleted

progressMessage :: Verbosity -> ProgressPhase -> String -> IO ()
progressMessage verbosity phase subject = do
    noticeNoWrap verbosity $ phaseStr ++ subject ++ "\n"
  where
    phaseStr = case phase of
        ProgressDownloading -> "Downloading  "
        ProgressDownloaded  -> "Downloaded   "
        ProgressStarting    -> "Starting     "
        ProgressBuilding    -> "Building     "
        ProgressHaddock     -> "Haddock      "
        ProgressInstalling  -> "Installing   "
        ProgressCompleted   -> "Completed    "


-- | Given a version, return an API-compatible (according to PVP) version range.
--
-- If the boolean argument denotes whether to use a desugared
-- representation (if 'True') or the new-style @^>=@-form (if
-- 'False').
--
-- Example: @pvpize True (mkVersion [0,4,1])@ produces the version range @>= 0.4 && < 0.5@ (which is the
-- same as @0.4.*@).
pvpize :: Bool -> Version -> VersionRange
pvpize False  v = majorBoundVersion v
pvpize True   v = orLaterVersion v'
           `intersectVersionRanges`
           earlierVersion (incVersion 1 v')
  where v' = alterVersion (take 2) v

-- | Increment the nth version component (counting from 0).
incVersion :: Int -> Version -> Version
incVersion n = alterVersion (incVersion' n)
  where
    incVersion' 0 []     = [1]
    incVersion' 0 (v:_)  = [v+1]
    incVersion' m []     = replicate m 0 ++ [1]
    incVersion' m (v:vs) = v : incVersion' (m-1) vs

-- | Returns the current calendar year.
getCurrentYear :: IO Integer
getCurrentYear = do
  u <- getCurrentTime
  z <- getCurrentTimeZone
  let l = utcToLocalTime z u
      (y, _, _) = toGregorian $ localDay l
  return y

-- | From System.Directory.Extra
--   https://hackage.haskell.org/package/extra-1.7.9
listFilesInside :: (FilePath -> IO Bool) -> FilePath -> IO [FilePath]
listFilesInside test dir = ifM (notM $ test $ dropTrailingPathSeparator dir) (pure []) $ do
    (dirs,files) <- partitionM doesDirectoryExist =<< listContents dir
    rest <- concatMapM (listFilesInside test) dirs
    pure $ files ++ rest

-- | From System.Directory.Extra
--   https://hackage.haskell.org/package/extra-1.7.9
listFilesRecursive :: FilePath -> IO [FilePath]
listFilesRecursive = listFilesInside (const $ pure True)

-- | From System.Directory.Extra
--   https://hackage.haskell.org/package/extra-1.7.9
listContents :: FilePath -> IO [FilePath]
listContents dir = do
    xs <- getDirectoryContents dir
    pure $ sort [dir </> x | x <- xs, not $ all (== '.') x]

-- | From Control.Monad.Extra
--   https://hackage.haskell.org/package/extra-1.7.9
ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM b t f = do b' <- b; if b' then t else f

-- | From Control.Monad.Extra
--   https://hackage.haskell.org/package/extra-1.7.9
concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
{-# INLINE concatMapM #-}
concatMapM op = foldr f (pure [])
    where f x xs = do x' <- op x; if null x' then xs else do xs' <- xs; pure $ x' ++ xs'

-- | From Control.Monad.Extra
--   https://hackage.haskell.org/package/extra-1.7.9
partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM _ [] = pure ([], [])
partitionM f (x:xs) = do
    res <- f x
    (as,bs) <- partitionM f xs
    pure ([x | res]++as, [x | not res]++bs)

-- | From Control.Monad.Extra
--   https://hackage.haskell.org/package/extra-1.7.9
notM :: Functor m => m Bool -> m Bool
notM = fmap not

safeRead :: Read a => String -> Maybe a
safeRead s
  | [(x, "")] <- reads s = Just x
  | otherwise = Nothing
