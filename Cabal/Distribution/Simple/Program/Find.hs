{-# LANGUAGE CPP, DeriveGeneric #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Program.Find
-- Copyright   :  Duncan Coutts 2013
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- A somewhat extended notion of the normal program search path concept.
--
-- Usually when finding executables we just want to look in the usual places
-- using the OS's usual method for doing so. In Haskell the normal OS-specific
-- method is captured by 'findExecutable'. On all common OSs that makes use of
-- a @PATH@ environment variable, (though on Windows it is not just the @PATH@).
--
-- However it is sometimes useful to be able to look in additional locations
-- without having to change the process-global @PATH@ environment variable.
-- So we need an extension of the usual 'findExecutable' that can look in
-- additional locations, either before, after or instead of the normal OS
-- locations.
--
module Distribution.Simple.Program.Find (
    -- * Program search path
    ProgramSearchPath,
    ProgramSearchPathEntry(..),
    defaultProgramSearchPath,
    findProgramOnSearchPath,
    programSearchPathAsPATHVar,
    getSystemSearchPath,
  ) where

import Distribution.Verbosity
         ( Verbosity )
import Distribution.Simple.Utils
         ( debug, doesExecutableExist )
import Distribution.System
         ( OS(..), buildOS )
#if MIN_VERSION_directory(1,2,1)
import qualified System.Directory as Directory
         ( findExecutable )
#endif
import Distribution.Compat.Environment
         ( getEnvironment )
import System.FilePath as FilePath
         ( (</>), (<.>), splitSearchPath, searchPathSeparator, getSearchPath
         , takeDirectory )
import Data.List
         ( intercalate, nub )
import Distribution.Compat.Binary
import GHC.Generics
#if defined(mingw32_HOST_OS)
import qualified System.Win32
#endif

-- | A search path to use when locating executables. This is analogous
-- to the unix @$PATH@ or win32 @%PATH%@ but with the ability to use
-- the system default method for finding executables ('findExecutable' which
-- on unix is simply looking on the @$PATH@ but on win32 is a bit more
-- complicated).
--
-- The default to use is @[ProgSearchPathDefault]@ but you can add extra dirs
-- either before, after or instead of the default, e.g. here we add an extra
-- dir to search after the usual ones.
--
-- > ['ProgramSearchPathDefault', 'ProgramSearchPathDir' dir]
--
type ProgramSearchPath = [ProgramSearchPathEntry]
data ProgramSearchPathEntry =
         ProgramSearchPathDir FilePath  -- ^ A specific dir
       | ProgramSearchPathDefault       -- ^ The system default
  deriving (Eq, Generic)

instance Binary ProgramSearchPathEntry

defaultProgramSearchPath :: ProgramSearchPath
defaultProgramSearchPath = [ProgramSearchPathDefault]

findProgramOnSearchPath :: Verbosity -> ProgramSearchPath
                        -> FilePath -> IO (Maybe FilePath)
findProgramOnSearchPath verbosity searchpath prog = do
    debug verbosity $ "Searching for " ++ prog ++ " in path."
    res <- tryPathElems [] searchpath
    case res of
      Nothing   -> debug verbosity ("Cannot find " ++ prog ++ " on the path")
      Just (path, _) -> debug verbosity ("Found " ++ prog ++ " at "++ path)
    return (fmap fst res)
  where
    tryPathElems :: [[FilePath]] -> [ProgramSearchPathEntry]
                 -> IO (Maybe (FilePath, [FilePath]))
    tryPathElems _     []       = return Nothing
    tryPathElems tried (pe:pes) = do
      res <- tryPathElem pe
      case res of
        (Nothing,      notfoundat) -> tryPathElems (notfoundat : tried) pes
        (Just foundat, notfoundat) -> return (Just (foundat, alltried))
          where
            alltried = concat (reverse (notfoundat : tried))

    tryPathElem :: ProgramSearchPathEntry -> IO (Maybe FilePath, [FilePath])
    tryPathElem (ProgramSearchPathDir dir) =
        findFirstExe [ dir </> prog <.> ext | ext <- exeExtensions ]

    -- On windows, getSystemSearchPath is not guaranteed 100% correct so we
    -- use findExecutable and then approximate the not-found-at locations.
    tryPathElem ProgramSearchPathDefault | buildOS == Windows = do
      mExe    <- findExecutable prog
      syspath <- getSystemSearchPath
      case mExe of
        Nothing ->
          let notfoundat = [ dir </> prog | dir <- syspath ] in
          return (Nothing, notfoundat)

        Just foundat -> do
          let founddir   = takeDirectory foundat
              notfoundat = [ dir </> prog
                           | dir <- takeWhile (/= founddir) syspath ]
          return (Just foundat, notfoundat)

    -- On other OSs we can just do the simple thing
    tryPathElem ProgramSearchPathDefault = do
      dirs <- getSystemSearchPath
      findFirstExe [ dir </> prog <.> ext | dir <- dirs, ext <- exeExtensions ]

    -- Possible improvement: on Windows, read the list of extensions from
    -- the PATHEXT environment variable. By default PATHEXT is ".com; .exe;
    -- .bat; .cmd".
    exeExtensions = case buildOS of
                      Windows -> ["", "exe"]
                      Ghcjs   -> ["", "exe"]
                      _       -> [""]

    findFirstExe :: [FilePath] -> IO (Maybe FilePath, [FilePath])
    findFirstExe = go []
      where
        go fs' []     = return (Nothing, reverse fs')
        go fs' (f:fs) = do
          isExe <- doesExecutableExist f
          if isExe
            then return (Just f, reverse fs')
            else go (f:fs') fs

-- | Interpret a 'ProgramSearchPath' to construct a new @$PATH@ env var.
-- Note that this is close but not perfect because on Windows the search
-- algorithm looks at more than just the @%PATH%@.
programSearchPathAsPATHVar :: ProgramSearchPath -> IO String
programSearchPathAsPATHVar searchpath = do
    ess <- mapM getEntries searchpath
    return (intercalate [searchPathSeparator] (concat ess))
  where
    getEntries (ProgramSearchPathDir dir) = return [dir]
    getEntries ProgramSearchPathDefault   = do
      env <- getEnvironment
      return (maybe [] splitSearchPath (lookup "PATH" env))

-- | Get the system search path. On Unix systems this is just the @$PATH@ env
-- var, but on windows it's a bit more complicated.
--
getSystemSearchPath :: IO [FilePath]
getSystemSearchPath = fmap nub $ do
#if defined(mingw32_HOST_OS)
    processdir <- liftM takeDirectory (Win32.getModuleFileName Win32.nullHANDLE)
    currentdir <- getCurrentDirectory
    systemdir  <- Win32.getSystemDirectory
    windowsdir <- Win32.getWindowsDirectory
    pathdirs   <- FilePath.getSearchPath
    let path = processdir : currentdir
             : systemdir  : windowsdir
             : pathdirs
    return path
#else
    FilePath.getSearchPath
#endif

findExecutable :: FilePath -> IO (Maybe FilePath)
#if MIN_VERSION_directory(1,2,1)
findExecutable = Directory.findExecutable
#else
findExecutable prog = do
      -- With directory < 1.2.1 'findExecutable' doesn't check that the path
      -- really refers to an executable.
      mExe <- findExecutable prog
      case mExe of
        Just exe -> do
          exeExists <- doesExecutableExist exe
          if exeExists
            then return mExe
            else return Nothing
        _     -> return mExe
#endif

