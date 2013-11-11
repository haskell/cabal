-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Program.Types
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
  ) where

import Distribution.Verbosity
         ( Verbosity )
import Distribution.Simple.Utils
         ( debug, doesExecutableExist )
import Distribution.System
         ( OS(..), buildOS )
import System.Directory
         ( findExecutable )
import Distribution.Compat.Environment
         ( getEnvironment )
import System.FilePath
         ( (</>), (<.>), splitSearchPath, searchPathSeparator )
import Data.List
         ( intercalate )


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

defaultProgramSearchPath :: ProgramSearchPath
defaultProgramSearchPath = [ProgramSearchPathDefault]

findProgramOnSearchPath :: Verbosity -> ProgramSearchPath
                        -> FilePath -> IO (Maybe FilePath)
findProgramOnSearchPath verbosity searchpath prog = do
    debug verbosity $ "Searching for " ++ prog ++ " in path."
    res <- tryPathElems searchpath
    case res of
      Nothing   -> debug verbosity ("Cannot find " ++ prog ++ " on the path")
      Just path -> debug verbosity ("Found " ++ prog ++ " at "++ path)
    return res
  where
    tryPathElems []       = return Nothing
    tryPathElems (pe:pes) = do
      res <- tryPathElem pe
      case res of
        Nothing -> tryPathElems pes
        Just _  -> return res

    tryPathElem (ProgramSearchPathDir dir) =
        findFirstExe [ dir </> prog <.> ext | ext <- extensions ]
      where
        -- Possible improvement: on Windows, read the list of extensions from
        -- the PATHEXT environment variable. By default PATHEXT is ".com; .exe;
        -- .bat; .cmd".
        extensions = case buildOS of
                       Windows -> ["", "exe"]
                       _       -> [""]

    tryPathElem ProgramSearchPathDefault = do
      -- 'findExecutable' doesn't check that the path really refers to an
      -- executable on Windows (at least with GHC < 7.8). See
      -- https://ghc.haskell.org/trac/ghc/ticket/2184
      mExe <- findExecutable prog
      case mExe of
        Just exe -> do
          exeExists <- doesExecutableExist exe
          if exeExists
            then return mExe
            else return Nothing
        _        -> return mExe

    findFirstExe []     = return Nothing
    findFirstExe (f:fs) = do
      isExe <- doesExecutableExist f
      if isExe
        then return (Just f)
        else findFirstExe fs

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
