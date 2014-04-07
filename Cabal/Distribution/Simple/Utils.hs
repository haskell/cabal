{-# LANGUAGE CPP, ForeignFunctionInterface #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Utils
-- Copyright   :  Isaac Jones, Simon Marlow 2003-2004
-- License     :  BSD3
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

module Distribution.Simple.Utils (
        cabalVersion,

        -- * logging and errors
        die,
        dieWithLocation,
        topHandler, topHandlerWith,
        warn, notice, setupMessage, info, debug,
        debugNoWrap, chattyTry,

        -- * running programs
        rawSystemExit,
        rawSystemExitCode,
        rawSystemExitWithEnv,
        rawSystemStdout,
        rawSystemStdInOut,
        rawSystemIOWithEnv,
        maybeExit,
        xargs,
        findProgramLocation,
        findProgramVersion,

        -- * copying files
        smartCopySources,
        createDirectoryIfMissingVerbose,
        copyFileVerbose,
        copyDirectoryRecursiveVerbose,
        copyFiles,
        copyFileTo,

        -- * installing files
        installOrdinaryFile,
        installExecutableFile,
        installMaybeExecutableFile,
        installOrdinaryFiles,
        installExecutableFiles,
        installMaybeExecutableFiles,
        installDirectoryContents,

        -- * File permissions
        doesExecutableExist,
        setFileOrdinary,
        setFileExecutable,

        -- * file names
        currentDir,

        -- * finding files
        findFile,
        findFirstFile,
        findFileWithExtension,
        findFileWithExtension',
        findModuleFile,
        findModuleFiles,
        getDirectoryContentsRecursive,

        -- * environment variables
        isInSearchPath,

        -- * simple file globbing
        matchFileGlob,
        matchDirFileGlob,
        parseFileGlob,
        FileGlob(..),

        -- * modification time
        moreRecentFile,
        existsAndIsMoreRecentThan,

        -- * temp files and dirs
        TempFileOptions(..), defaultTempFileOptions,
        withTempFile, withTempFileEx,
        withTempDirectory, withTempDirectoryEx,

        -- * .cabal and .buildinfo files
        defaultPackageDesc,
        findPackageDesc,
        tryFindPackageDesc,
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
        normaliseLineEndings,

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
    ( join, when, unless, filterM )
import Control.Concurrent.MVar
    ( newEmptyMVar, putMVar, takeMVar )
import Data.List
  ( nub, unfoldr, isPrefixOf, tails, intercalate )
import Data.Char as Char
    ( toLower, chr, ord )
import Data.Bits
    ( Bits((.|.), (.&.), shiftL, shiftR) )
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BS.Char8

import System.Directory
    ( Permissions(executable), getDirectoryContents, getPermissions
    , doesDirectoryExist, doesFileExist, removeFile, findExecutable
    , getModificationTime )
import System.Environment
    ( getProgName )
import System.Exit
    ( exitWith, ExitCode(..) )
import System.FilePath
    ( normalise, (</>), (<.>)
    , getSearchPath, takeDirectory, splitFileName
    , splitExtension, splitExtensions, splitDirectories )
import System.Directory
    ( createDirectory, renameFile, removeDirectoryRecursive )
import System.IO
    ( Handle, openFile, openBinaryFile, openBinaryTempFile
    , IOMode(ReadMode), hSetBinaryMode
    , hGetContents, stdin, stderr, stdout, hPutStr, hFlush, hClose )
import System.IO.Error as IO.Error
    ( isDoesNotExistError, isAlreadyExistsError
    , ioeSetFileName, ioeGetFileName, ioeGetErrorString )
import System.IO.Error
    ( ioeSetLocation, ioeGetLocation )
import System.IO.Unsafe
    ( unsafeInterleaveIO )
import qualified Control.Exception as Exception

import Distribution.Text
    ( display, simpleParse )
import Distribution.Package
    ( PackageIdentifier )
import Distribution.ModuleName (ModuleName)
import qualified Distribution.ModuleName as ModuleName
import Distribution.Version
    (Version(..))

import Control.Exception (IOException, evaluate, throwIO)
import System.Process (rawSystem)
import qualified System.Process as Process (CreateProcess(..))

import Control.Concurrent (forkIO)
import System.Process (runInteractiveProcess, waitForProcess, proc,
                       StdStream(..))
#if __GLASGOW_HASKELL__ >= 702
import System.Process (showCommandForUser)
#endif

#ifndef mingw32_HOST_OS
import System.Posix.Signals (installHandler, sigINT, sigQUIT, Handler(..))
import System.Process.Internals (defaultSignal, runGenProcess_)
#else
import System.Process (createProcess)
#endif

import Distribution.Compat.CopyFile
         ( copyFile, copyOrdinaryFile, copyExecutableFile
         , setFileOrdinary, setFileExecutable, setDirOrdinary )
import Distribution.Compat.TempFile
         ( openTempFile, createTempDirectory )
import Distribution.Compat.Exception
         ( tryIO, catchIO, catchExit )
import Distribution.Verbosity

#ifdef VERSION_base
import qualified Paths_Cabal (version)
#endif

-- We only get our own version number when we're building with ourselves
cabalVersion :: Version
#if defined(VERSION_base)
cabalVersion = Paths_Cabal.version
#elif defined(CABAL_VERSION)
cabalVersion = Version [CABAL_VERSION] []
#else
cabalVersion = Version [1,9999] []  --used when bootstrapping
#endif

-- ----------------------------------------------------------------------------
-- Exception and logging utils

dieWithLocation :: FilePath -> Maybe Int -> String -> IO a
dieWithLocation filename lineno msg =
  ioError . setLocation lineno
          . flip ioeSetFileName (normalise filename)
          $ userError msg
  where
    setLocation Nothing  err = err
    setLocation (Just n) err = ioeSetLocation err (show n)

die :: String -> IO a
die msg = ioError (userError msg)

topHandlerWith :: (Exception.IOException -> IO a) -> IO a -> IO a
topHandlerWith cont prog = catchIO prog handle
  where
    handle ioe = do
      hFlush stdout
      pname <- getProgName
      hPutStr stderr (mesage pname)
      cont ioe
      where
        mesage pname = wrapText (pname ++ ": " ++ file ++ detail)
        file         = case ioeGetFileName ioe of
                         Nothing   -> ""
                         Just path -> path ++ location ++ ": "
        location     = case ioeGetLocation ioe of
                         l@(n:_) | n >= '0' && n <= '9' -> ':' : l
                         _                              -> ""
        detail       = ioeGetErrorString ioe

topHandler :: IO a -> IO a
topHandler prog = topHandlerWith (const $ exitWith (ExitFailure 1)) prog

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

-- | A variant of 'debug' that doesn't perform the automatic line
-- wrapping. Produces better output in some cases.
debugNoWrap :: Verbosity -> String -> IO ()
debugNoWrap verbosity msg =
  when (verbosity >= deafening) $ do
    putStrLn msg
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
         . map (intercalate "\n"
              . map unwords
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
 | verbosity >= verbose   =
#if __GLASGOW_HASKELL__ >= 702
                            putStrLn $ showCommandForUser path args
#else
                            putStrLn $ unwords (path : args)
#endif
 | otherwise              = return ()

printRawCommandAndArgsAndEnv :: Verbosity
                             -> FilePath
                             -> [String]
                             -> [(String, String)]
                             -> IO ()
printRawCommandAndArgsAndEnv verbosity path args env
 | verbosity >= deafening = do putStrLn ("Environment: " ++ show env)
                               print (path, args)
 | verbosity >= verbose   = putStrLn $ unwords (path : args)
 | otherwise              = return ()


-- This is taken directly from the process package.
-- The reason we need it is that runProcess doesn't handle ^C in the same
-- way that rawSystem handles it, but rawSystem doesn't allow us to pass
-- an environment.
syncProcess :: String -> Process.CreateProcess -> IO ExitCode
#if mingw32_HOST_OS
syncProcess _fun c = do
  (_,_,_,p) <- createProcess c
  waitForProcess p
#else
syncProcess fun c = do
  -- The POSIX version of system needs to do some manipulation of signal
  -- handlers.  Since we're going to be synchronously waiting for the child,
  -- we want to ignore ^C in the parent, but handle it the default way
  -- in the child (using SIG_DFL isn't really correct, it should be the
  -- original signal handler, but the GHC RTS will have already set up
  -- its own handler and we don't want to use that).
  r <- Exception.bracket (installHandlers) (restoreHandlers) $
       (\_ -> do (_,_,_,p) <- runGenProcess_ fun c
                              (Just defaultSignal) (Just defaultSignal)
                 waitForProcess p)
  return r
    where
      installHandlers = do
        old_int  <- installHandler sigINT  Ignore Nothing
        old_quit <- installHandler sigQUIT Ignore Nothing
        return (old_int, old_quit)
      restoreHandlers (old_int, old_quit) = do
        _ <- installHandler sigINT  old_int Nothing
        _ <- installHandler sigQUIT old_quit Nothing
        return ()
#endif  /* mingw32_HOST_OS */

-- Exit with the same exitcode if the subcommand fails
rawSystemExit :: Verbosity -> FilePath -> [String] -> IO ()
rawSystemExit verbosity path args = do
  printRawCommandAndArgs verbosity path args
  hFlush stdout
  exitcode <- rawSystem path args
  unless (exitcode == ExitSuccess) $ do
    debug verbosity $ path ++ " returned " ++ show exitcode
    exitWith exitcode

rawSystemExitCode :: Verbosity -> FilePath -> [String] -> IO ExitCode
rawSystemExitCode verbosity path args = do
  printRawCommandAndArgs verbosity path args
  hFlush stdout
  exitcode <- rawSystem path args
  unless (exitcode == ExitSuccess) $ do
    debug verbosity $ path ++ " returned " ++ show exitcode
  return exitcode

rawSystemExitWithEnv :: Verbosity
                     -> FilePath
                     -> [String]
                     -> [(String, String)]
                     -> IO ()
rawSystemExitWithEnv verbosity path args env = do
    printRawCommandAndArgsAndEnv verbosity path args env
    hFlush stdout
    exitcode <- syncProcess "rawSystemExitWithEnv" (proc path args)
                { Process.env = Just env }
    unless (exitcode == ExitSuccess) $ do
        debug verbosity $ path ++ " returned " ++ show exitcode
        exitWith exitcode

-- Closes the passed in handles before returning.
rawSystemIOWithEnv :: Verbosity
                   -> FilePath
                   -> [String]
                   -> Maybe FilePath           -- ^ New working dir or inherit
                   -> Maybe [(String, String)] -- ^ New environment or inherit
                   -> Maybe Handle  -- ^ stdin
                   -> Maybe Handle  -- ^ stdout
                   -> Maybe Handle  -- ^ stderr
                   -> IO ExitCode
rawSystemIOWithEnv verbosity path args mcwd menv inp out err = do
    maybe (printRawCommandAndArgs       verbosity path args)
          (printRawCommandAndArgsAndEnv verbosity path args) menv
    hFlush stdout
    exitcode <- syncProcess "rawSystemIOWithEnv" (proc path args)
                { Process.cwd     = mcwd
                , Process.env     = menv
                , Process.std_in  = mbToStd inp
                , Process.std_out = mbToStd out
                , Process.std_err = mbToStd err }
                `Exception.finally` (mapM_ maybeClose [inp, out, err])
    unless (exitcode == ExitSuccess) $ do
      debug verbosity $ path ++ " returned " ++ show exitcode
    return exitcode
  where
  -- Also taken from System.Process
  maybeClose :: Maybe Handle -> IO ()
  maybeClose (Just  hdl)
    | hdl /= stdin && hdl /= stdout && hdl /= stderr = hClose hdl
  maybeClose _ = return ()

  mbToStd :: Maybe Handle -> StdStream
  mbToStd Nothing    = Inherit
  mbToStd (Just hdl) = UseHandle hdl

-- | Run a command and return its output.
--
-- The output is assumed to be text in the locale encoding.
--
rawSystemStdout :: Verbosity -> FilePath -> [String] -> IO String
rawSystemStdout verbosity path args = do
  (output, errors, exitCode) <- rawSystemStdInOut verbosity path args
                                                  Nothing Nothing
                                                  Nothing False
  when (exitCode /= ExitSuccess) $
    die errors
  return output

-- | Run a command and return its output, errors and exit status. Optionally
-- also supply some input. Also provides control over whether the binary/text
-- mode of the input and output.
--
rawSystemStdInOut :: Verbosity
                  -> FilePath                 -- ^ Program location
                  -> [String]                 -- ^ Arguments
                  -> Maybe FilePath           -- ^ New working dir or inherit
                  -> Maybe [(String, String)] -- ^ New environment or inherit
                  -> Maybe (String, Bool)     -- ^ input text and binary mode
                  -> Bool                     -- ^ output in binary mode
                  -> IO (String, String, ExitCode) -- ^ output, errors, exit
rawSystemStdInOut verbosity path args mcwd menv input outputBinary = do
  printRawCommandAndArgs verbosity path args

  Exception.bracket
     (runInteractiveProcess path args mcwd menv)
     (\(inh,outh,errh,_) -> hClose inh >> hClose outh >> hClose errh)
    $ \(inh,outh,errh,pid) -> do

      -- output mode depends on what the caller wants
      hSetBinaryMode outh outputBinary
      -- but the errors are always assumed to be text (in the current locale)
      hSetBinaryMode errh False

      -- fork off a couple threads to pull on the stderr and stdout
      -- so if the process writes to stderr we do not block.

      err <- hGetContents errh
      out <- hGetContents outh

      mv <- newEmptyMVar
      let force str = (evaluate (length str) >> return ())
            `Exception.finally` putMVar mv ()
          --TODO: handle exceptions like text decoding.
      _ <- forkIO $ force out
      _ <- forkIO $ force err

      -- push all the input, if any
      case input of
        Nothing -> return ()
        Just (inputStr, inputBinary) -> do
                -- input mode depends on what the caller wants
          hSetBinaryMode inh inputBinary
          hPutStr inh inputStr
          hClose inh
          --TODO: this probably fails if the process refuses to consume
          -- or if it closes stdin (eg if it exits)

      -- wait for both to finish, in either order
      takeMVar mv
      takeMVar mv

      -- wait for the program to terminate
      exitcode <- waitForProcess pid
      unless (exitcode == ExitSuccess) $
        debug verbosity $ path ++ " returned " ++ show exitcode
                       ++ if null err then "" else
                          " with error message:\n" ++ err
                       ++ case input of
                            Nothing       -> ""
                            Just ("",  _) -> ""
                            Just (inp, _) -> "\nstdin input:\n" ++ inp

      return (out, err, exitcode)


-- | Look for a program on the path.
findProgramLocation :: Verbosity -> FilePath -> IO (Maybe FilePath)
findProgramLocation verbosity prog = do
  debug verbosity $ "searching for " ++ prog ++ " in path."
  res <- findExecutable prog
  case res of
      Nothing   -> debug verbosity ("Cannot find " ++ prog ++ " on the path")
      Just path -> debug verbosity ("found " ++ prog ++ " at "++ path)
  return res


-- | Look for a program and try to find it's version number. It can accept
-- either an absolute path or the name of a program binary, in which case we
-- will look for the program on the path.
--
findProgramVersion :: String             -- ^ version args
                   -> (String -> String) -- ^ function to select version
                                         --   number from program output
                   -> Verbosity
                   -> FilePath           -- ^ location
                   -> IO (Maybe Version)
findProgramVersion versionArg selectVersion verbosity path = do
  str <- rawSystemStdout verbosity path [versionArg]
         `catchIO`   (\_ -> return "")
         `catchExit` (\_ -> return "")
  let version :: Maybe Version
      version = simpleParse (selectVersion str)
  case version of
      Nothing -> warn verbosity $ "cannot determine version of " ++ path
                               ++ " :\n" ++ show str
      Just v  -> debug verbosity $ path ++ " is version " ++ display v
  return version


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

----------------
-- Finding files

-- | Find a file by looking in a search path. The file path must match exactly.
--
findFile :: [FilePath]    -- ^search locations
         -> FilePath      -- ^File Name
         -> IO FilePath
findFile searchPath fileName =
  findFirstFile id
    [ path </> fileName
    | path <- nub searchPath]
  >>= maybe (die $ fileName ++ " doesn't exist") return

-- | Find a file by looking in a search path with one of a list of possible
-- file extensions. The file base name should be given and it will be tried
-- with each of the extensions in each element of the search path.
--
findFileWithExtension :: [String]
                      -> [FilePath]
                      -> FilePath
                      -> IO (Maybe FilePath)
findFileWithExtension extensions searchPath baseName =
  findFirstFile id
    [ path </> baseName <.> ext
    | path <- nub searchPath
    , ext <- nub extensions ]

-- | Like 'findFileWithExtension' but returns which element of the search path
-- the file was found in, and the file path relative to that base directory.
--
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

-- | Finds the files corresponding to a list of Haskell module names.
--
-- As 'findModuleFile' but for a list of module names.
--
findModuleFiles :: [FilePath]   -- ^ build prefix (location of objects)
                -> [String]     -- ^ search suffixes
                -> [ModuleName] -- ^ modules
                -> IO [(FilePath, FilePath)]
findModuleFiles searchPath extensions moduleNames =
  mapM (findModuleFile searchPath extensions) moduleNames

-- | Find the file corresponding to a Haskell module name.
--
-- This is similar to 'findFileWithExtension'' but specialised to a module
-- name. The function fails if the file corresponding to the module is missing.
--
findModuleFile :: [FilePath]  -- ^ build prefix (location of objects)
               -> [String]    -- ^ search suffixes
               -> ModuleName  -- ^ module
               -> IO (FilePath, FilePath)
findModuleFile searchPath extensions moduleName =
      maybe notFound return
  =<< findFileWithExtension' extensions searchPath
                             (ModuleName.toFilePath moduleName)
  where
    notFound = die $ "Error: Could not find module: " ++ display moduleName
                  ++ " with any suffix: " ++ show extensions
                  ++ " in the search path: " ++ show searchPath

-- | List all the files in a directory and all subdirectories.
--
-- The order places files in sub-directories after all the files in their
-- parent directories. The list is generated lazily so is not well defined if
-- the source directory structure changes before the list is used.
--
getDirectoryContentsRecursive :: FilePath -> IO [FilePath]
getDirectoryContentsRecursive topdir = recurseDirectories [""]
  where
    recurseDirectories :: [FilePath] -> IO [FilePath]
    recurseDirectories []         = return []
    recurseDirectories (dir:dirs) = unsafeInterleaveIO $ do
      (files, dirs') <- collect [] [] =<< getDirectoryContents (topdir </> dir)
      files' <- recurseDirectories (dirs' ++ dirs)
      return (files ++ files')

      where
        collect files dirs' []              = return (reverse files
                                                     ,reverse dirs')
        collect files dirs' (entry:entries) | ignore entry
                                            = collect files dirs' entries
        collect files dirs' (entry:entries) = do
          let dirEntry = dir </> entry
          isDirectory <- doesDirectoryExist (topdir </> dirEntry)
          if isDirectory
            then collect files (dirEntry:dirs') entries
            else collect (dirEntry:files) dirs' entries

        ignore ['.']      = True
        ignore ['.', '.'] = True
        ignore _          = False

------------------------
-- Environment variables

-- | Is this directory in the system search path?
isInSearchPath :: FilePath -> IO Bool
isInSearchPath path = fmap (elem path) getSearchPath

----------------
-- File globbing

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
  Nothing -> die $ "invalid file glob '" ++ filepath
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

--------------------
-- Modification time

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

-- | Like 'moreRecentFile', but also checks that the first file exists.
existsAndIsMoreRecentThan :: FilePath -> FilePath -> IO Bool
existsAndIsMoreRecentThan a b = do
  exists <- doesFileExist a
  if not exists
    then return False
    else a `moreRecentFile` b

----------------------------------------
-- Copying and installing files and dirs

-- | Same as 'createDirectoryIfMissing' but logs at higher verbosity levels.
--
createDirectoryIfMissingVerbose :: Verbosity
                                -> Bool     -- ^ Create its parents too?
                                -> FilePath
                                -> IO ()
createDirectoryIfMissingVerbose verbosity create_parents path0
  | create_parents = createDirs (parents path0)
  | otherwise      = createDirs (take 1 (parents path0))
  where
    parents = reverse . scanl1 (</>) . splitDirectories . normalise

    createDirs []         = return ()
    createDirs (dir:[])   = createDir dir throwIO
    createDirs (dir:dirs) =
      createDir dir $ \_ -> do
        createDirs dirs
        createDir dir throwIO

    createDir :: FilePath -> (IOException -> IO ()) -> IO ()
    createDir dir notExistHandler = do
      r <- tryIO $ createDirectoryVerbose verbosity dir
      case (r :: Either IOException ()) of
        Right ()                   -> return ()
        Left  e
          | isDoesNotExistError  e -> notExistHandler e
          -- createDirectory (and indeed POSIX mkdir) does not distinguish
          -- between a dir already existing and a file already existing. So we
          -- check for it here. Unfortunately there is a slight race condition
          -- here, but we think it is benign. It could report an exeption in
          -- the case that the dir did exist but another process deletes the
          -- directory and creates a file in its place before we can check
          -- that the directory did indeed exist.
          | isAlreadyExistsError e -> (do
              isDir <- doesDirectoryExist dir
              if isDir then return ()
                       else throwIO e
              ) `catchIO` ((\_ -> return ()) :: IOException -> IO ())
          | otherwise              -> throwIO e

createDirectoryVerbose :: Verbosity -> FilePath -> IO ()
createDirectoryVerbose verbosity dir = do
  info verbosity $ "creating " ++ dir
  createDirectory dir
  setDirOrdinary dir

-- | Copies a file without copying file permissions. The target file is created
-- with default permissions. Any existing target file is replaced.
--
-- At higher verbosity levels it logs an info message.
--
copyFileVerbose :: Verbosity -> FilePath -> FilePath -> IO ()
copyFileVerbose verbosity src dest = do
  info verbosity ("copy " ++ src ++ " to " ++ dest)
  copyFile src dest

-- | Install an ordinary file. This is like a file copy but the permissions
-- are set appropriately for an installed file. On Unix it is \"-rw-r--r--\"
-- while on Windows it uses the default permissions for the target directory.
--
installOrdinaryFile :: Verbosity -> FilePath -> FilePath -> IO ()
installOrdinaryFile verbosity src dest = do
  info verbosity ("Installing " ++ src ++ " to " ++ dest)
  copyOrdinaryFile src dest

-- | Install an executable file. This is like a file copy but the permissions
-- are set appropriately for an installed file. On Unix it is \"-rwxr-xr-x\"
-- while on Windows it uses the default permissions for the target directory.
--
installExecutableFile :: Verbosity -> FilePath -> FilePath -> IO ()
installExecutableFile verbosity src dest = do
  info verbosity ("Installing executable " ++ src ++ " to " ++ dest)
  copyExecutableFile src dest

-- | Install a file that may or not be executable, preserving permissions.
installMaybeExecutableFile :: Verbosity -> FilePath -> FilePath -> IO ()
installMaybeExecutableFile verbosity src dest = do
  perms <- getPermissions src
  if (executable perms) --only checks user x bit
    then installExecutableFile verbosity src dest
    else installOrdinaryFile   verbosity src dest

-- | Given a relative path to a file, copy it to the given directory, preserving
-- the relative path and creating the parent directories if needed.
copyFileTo :: Verbosity -> FilePath -> FilePath -> IO ()
copyFileTo verbosity dir file = do
  let targetFile = dir </> file
  createDirectoryIfMissingVerbose verbosity True (takeDirectory targetFile)
  installOrdinaryFile verbosity file targetFile

-- | Common implementation of 'copyFiles', 'installOrdinaryFiles',
-- 'installExecutableFiles' and 'installMaybeExecutableFiles'.
copyFilesWith :: (Verbosity -> FilePath -> FilePath -> IO ())
              -> Verbosity -> FilePath -> [(FilePath, FilePath)] -> IO ()
copyFilesWith doCopy verbosity targetDir srcFiles = do

  -- Create parent directories for everything
  let dirs = map (targetDir </>) . nub . map (takeDirectory . snd) $ srcFiles
  mapM_ (createDirectoryIfMissingVerbose verbosity True) dirs

  -- Copy all the files
  sequence_ [ let src  = srcBase   </> srcFile
                  dest = targetDir </> srcFile
               in doCopy verbosity src dest
            | (srcBase, srcFile) <- srcFiles ]

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
copyFiles = copyFilesWith copyFileVerbose

-- | This is like 'copyFiles' but uses 'installOrdinaryFile'.
--
installOrdinaryFiles :: Verbosity -> FilePath -> [(FilePath, FilePath)] -> IO ()
installOrdinaryFiles = copyFilesWith installOrdinaryFile

-- | This is like 'copyFiles' but uses 'installExecutableFile'.
--
installExecutableFiles :: Verbosity -> FilePath -> [(FilePath, FilePath)]
                          -> IO ()
installExecutableFiles = copyFilesWith installExecutableFile

-- | This is like 'copyFiles' but uses 'installMaybeExecutableFile'.
--
installMaybeExecutableFiles :: Verbosity -> FilePath -> [(FilePath, FilePath)]
                               -> IO ()
installMaybeExecutableFiles = copyFilesWith installMaybeExecutableFile

-- | This installs all the files in a directory to a target location,
-- preserving the directory layout. All the files are assumed to be ordinary
-- rather than executable files.
--
installDirectoryContents :: Verbosity -> FilePath -> FilePath -> IO ()
installDirectoryContents verbosity srcDir destDir = do
  info verbosity ("copy directory '" ++ srcDir ++ "' to '" ++ destDir ++ "'.")
  srcFiles <- getDirectoryContentsRecursive srcDir
  installOrdinaryFiles verbosity destDir [ (srcDir, f) | f <- srcFiles ]

-------------------
-- File permissions

-- | Like 'doesFileExist', but also checks that the file is executable.
doesExecutableExist :: FilePath -> IO Bool
doesExecutableExist f = do
  exists <- doesFileExist f
  if exists
    then do perms <- getPermissions f
            return (executable perms)
    else return False

---------------------------------
-- Deprecated file copy functions

{-# DEPRECATED smartCopySources
      "Use findModuleFiles and copyFiles or installOrdinaryFiles" #-}
smartCopySources :: Verbosity -> [FilePath] -> FilePath
                 -> [ModuleName] -> [String] -> IO ()
smartCopySources verbosity searchPath targetDir moduleNames extensions =
      findModuleFiles searchPath extensions moduleNames
  >>= copyFiles verbosity targetDir

{-# DEPRECATED copyDirectoryRecursiveVerbose
      "You probably want installDirectoryContents instead" #-}
copyDirectoryRecursiveVerbose :: Verbosity -> FilePath -> FilePath -> IO ()
copyDirectoryRecursiveVerbose verbosity srcDir destDir = do
  info verbosity ("copy directory '" ++ srcDir ++ "' to '" ++ destDir ++ "'.")
  srcFiles <- getDirectoryContentsRecursive srcDir
  copyFiles verbosity destDir [ (srcDir, f) | f <- srcFiles ]

---------------------------
-- Temporary files and dirs

-- | Advanced options for 'withTempFile' and 'withTempDirectory'.
data TempFileOptions = TempFileOptions {
  optKeepTempFiles :: Bool  -- ^ Keep temporary files?
  }

defaultTempFileOptions :: TempFileOptions
defaultTempFileOptions = TempFileOptions { optKeepTempFiles = False }

-- | Use a temporary filename that doesn't already exist.
--
withTempFile :: FilePath    -- ^ Temp dir to create the file in
                -> String   -- ^ File name template. See 'openTempFile'.
                -> (FilePath -> Handle -> IO a) -> IO a
withTempFile tmpDir template action =
  withTempFileEx defaultTempFileOptions tmpDir template action

-- | A version of 'withTempFile' that additionally takes a 'TempFileOptions'
-- argument.
withTempFileEx :: TempFileOptions
                 -> FilePath -- ^ Temp dir to create the file in
                 -> String   -- ^ File name template. See 'openTempFile'.
                 -> (FilePath -> Handle -> IO a) -> IO a
withTempFileEx opts tmpDir template action =
  Exception.bracket
    (openTempFile tmpDir template)
    (\(name, handle) -> do hClose handle
                           unless (optKeepTempFiles opts) $ removeFile name)
    (uncurry action)

-- | Create and use a temporary directory.
--
-- Creates a new temporary directory inside the given directory, making use
-- of the template. The temp directory is deleted after use. For example:
--
-- > withTempDirectory verbosity "src" "sdist." $ \tmpDir -> do ...
--
-- The @tmpDir@ will be a new subdirectory of the given directory, e.g.
-- @src/sdist.342@.
--
withTempDirectory :: Verbosity
                     -> FilePath -> String -> (FilePath -> IO a) -> IO a
withTempDirectory verbosity targetDir template =
  withTempDirectoryEx verbosity defaultTempFileOptions targetDir template

-- | A version of 'withTempDirectory' that additionally takes a
-- 'TempFileOptions' argument.
withTempDirectoryEx :: Verbosity
                       -> TempFileOptions
                       -> FilePath -> String -> (FilePath -> IO a) -> IO a
withTempDirectoryEx _verbosity opts targetDir template =
  Exception.bracket
    (createTempDirectory targetDir template)
    (unless (optKeepTempFiles opts) . removeDirectoryRecursive)

-----------------------------------
-- Safely reading and writing files

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
-- On windows it is not possible to delete a file that is open by a process.
-- This case will give an IO exception but the atomic property is not affected.
--
writeFileAtomic :: FilePath -> BS.ByteString -> IO ()
writeFileAtomic targetPath content = do
  let (targetDir, targetFile) = splitFileName targetPath
  Exception.bracketOnError
    (openBinaryTempFile targetDir $ targetFile <.> "tmp")
    (\(tmpPath, handle) -> hClose handle >> removeFile tmpPath)
    (\(tmpPath, handle) -> do
        BS.hPut handle content
        hClose handle
        renameFile tmpPath targetPath)

-- | Write a file but only if it would have new content. If we would be writing
-- the same as the existing content then leave the file as is so that we do not
-- update the file's modification time.
--
-- NB: the file is assumed to be ASCII-encoded.
rewriteFile :: FilePath -> String -> IO ()
rewriteFile path newContent =
  flip catchIO mightNotExist $ do
    existingContent <- readFile path
    _ <- evaluate (length existingContent)
    unless (existingContent == newContent) $
      writeFileAtomic path (BS.Char8.pack newContent)
  where
    mightNotExist e | isDoesNotExistError e = writeFileAtomic path
                                              (BS.Char8.pack newContent)
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
defaultPackageDesc _verbosity = tryFindPackageDesc currentDir

-- |Find a package description file in the given directory.  Looks for
-- @.cabal@ files.
findPackageDesc :: FilePath                    -- ^Where to look
                -> IO (Either String FilePath) -- ^<pkgname>.cabal
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
        []          -> return (Left  noDesc)
        [cabalFile] -> return (Right cabalFile)
        multiple    -> return (Left  $ multiDesc multiple)

  where
    noDesc :: String
    noDesc = "No cabal file found.\n"
             ++ "Please create a package description file <pkgname>.cabal"

    multiDesc :: [String] -> String
    multiDesc l = "Multiple cabal files found.\n"
                  ++ "Please use only one of: "
                  ++ intercalate ", " l

-- |Like 'findPackageDesc', but calls 'die' in case of error.
tryFindPackageDesc :: FilePath -> IO FilePath
tryFindPackageDesc dir = join . fmap (either die return) $ findPackageDesc dir

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

-- | Ignore a Unicode byte order mark (BOM) at the beginning of the input
--
ignoreBOM :: String -> String
ignoreBOM ('\xFEFF':string) = string
ignoreBOM string            = string

-- | Reads a UTF8 encoded text file as a Unicode String
--
-- Reads lazily using ordinary 'readFile'.
--
readUTF8File :: FilePath -> IO String
readUTF8File f = fmap (ignoreBOM . fromUTF8)
               . hGetContents =<< openBinaryFile f ReadMode

-- | Reads a UTF8 encoded text file as a Unicode String
--
-- Same behaviour as 'withFileContents'.
--
withUTF8FileContents :: FilePath -> (String -> IO a) -> IO a
withUTF8FileContents name action =
  Exception.bracket
    (openBinaryFile name ReadMode)
    hClose
    (\hnd -> hGetContents hnd >>= action . ignoreBOM . fromUTF8)

-- | Writes a Unicode String as a UTF8 encoded text file.
--
-- Uses 'writeFileAtomic', so provides the same guarantees.
--
writeUTF8File :: FilePath -> String -> IO ()
writeUTF8File path = writeFileAtomic path . BS.Char8.pack . toUTF8

-- | Fix different systems silly line ending conventions
normaliseLineEndings :: String -> String
normaliseLineEndings [] = []
normaliseLineEndings ('\r':'\n':s) = '\n' : normaliseLineEndings s -- windows
normaliseLineEndings ('\r':s)      = '\n' : normaliseLineEndings s -- old osx
normaliseLineEndings (  c :s)      =   c  : normaliseLineEndings s

-- ------------------------------------------------------------
-- * Common utils
-- ------------------------------------------------------------

equating :: Eq a => (b -> a) -> b -> b -> Bool
equating p x y = p x == p y

comparing :: Ord a => (b -> a) -> b -> b -> Ordering
comparing p x y = p x `compare` p y

isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)

lowercase :: String -> String
lowercase = map Char.toLower
