{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module: Distribution.Simple.FileMonitor.Types
--
-- Types for monitoring files and directories.
module Distribution.Simple.FileMonitor.Types
  ( -- * Globs with respect to a root
    RootedGlob (..)
  , FilePathRoot (..)
  , Glob

    -- * File monitoring
  , MonitorFilePath (..)
  , MonitorKindFile (..)
  , MonitorKindDir (..)

    -- ** Utility constructors of t'MonitorFilePath'
  , monitorFile
  , monitorFileHashed
  , monitorNonExistentFile
  , monitorFileExistence
  , monitorDirectory
  , monitorNonExistentDirectory
  , monitorDirectoryExistence
  , monitorFileOrDirectory
  , monitorFileGlob
  , monitorFileGlobExistence
  , monitorFileSearchPath
  , monitorFileHashedSearchPath
  )
where

import Distribution.Compat.Prelude
import Distribution.Simple.Glob.Internal
  ( Glob (..)
  )

import qualified Distribution.Compat.CharParsing as P
import Distribution.Parsec
import Distribution.Pretty
import qualified Text.PrettyPrint as Disp

--------------------------------------------------------------------------------
-- Rooted globs.
--

-- | A file path specified by globbing, relative
-- to some root directory.
data RootedGlob
  = RootedGlob
      FilePathRoot
      -- ^ what the glob is relative to
      Glob
      -- ^ the glob
  deriving (Eq, Show, Generic)

instance Binary RootedGlob
instance Structured RootedGlob

data FilePathRoot
  = FilePathRelative
  | -- | e.g. @"/"@, @"c:\"@ or result of 'takeDrive'
    FilePathRoot FilePath
  | FilePathHomeDir
  deriving (Eq, Show, Generic)

instance Binary FilePathRoot
instance Structured FilePathRoot

------------------------------------------------------------------------------
-- Types for specifying files to monitor
--

-- | A description of a file (or set of files) to monitor for changes.
--
-- Where file paths are relative they are relative to a common directory
-- (e.g. project root), not necessarily the process current directory.
data MonitorFilePath
  = MonitorFile
      { monitorKindFile :: !MonitorKindFile
      , monitorKindDir :: !MonitorKindDir
      , monitorPath :: !FilePath
      }
  | MonitorFileGlob
      { monitorKindFile :: !MonitorKindFile
      , monitorKindDir :: !MonitorKindDir
      , monitorPathGlob :: !RootedGlob
      }
  deriving (Eq, Show, Generic)

data MonitorKindFile
  = FileExists
  | FileModTime
  | FileHashed
  | FileNotExists
  deriving (Eq, Show, Generic)

data MonitorKindDir
  = DirExists
  | DirModTime
  | DirNotExists
  deriving (Eq, Show, Generic)

instance Binary MonitorFilePath
instance Binary MonitorKindFile
instance Binary MonitorKindDir

instance Structured MonitorFilePath
instance Structured MonitorKindFile
instance Structured MonitorKindDir

-- | Monitor a single file for changes, based on its modification time.
-- The monitored file is considered to have changed if it no longer
-- exists or if its modification time has changed.
monitorFile :: FilePath -> MonitorFilePath
monitorFile = MonitorFile FileModTime DirNotExists

-- | Monitor a single file for changes, based on its modification time
-- and content hash. The monitored file is considered to have changed if
-- it no longer exists or if its modification time and content hash have
-- changed.
monitorFileHashed :: FilePath -> MonitorFilePath
monitorFileHashed = MonitorFile FileHashed DirNotExists

-- | Monitor a single non-existent file for changes. The monitored file
-- is considered to have changed if it exists.
monitorNonExistentFile :: FilePath -> MonitorFilePath
monitorNonExistentFile = MonitorFile FileNotExists DirNotExists

-- | Monitor a single file for existence only. The monitored file is
-- considered to have changed if it no longer exists.
monitorFileExistence :: FilePath -> MonitorFilePath
monitorFileExistence = MonitorFile FileExists DirNotExists

-- | Monitor a single directory for changes, based on its modification
-- time. The monitored directory is considered to have changed if it no
-- longer exists or if its modification time has changed.
monitorDirectory :: FilePath -> MonitorFilePath
monitorDirectory = MonitorFile FileNotExists DirModTime

-- | Monitor a single non-existent directory for changes.  The monitored
-- directory is considered to have changed if it exists.
monitorNonExistentDirectory :: FilePath -> MonitorFilePath
-- Just an alias for monitorNonExistentFile, since you can't
-- tell the difference between a non-existent directory and
-- a non-existent file :)
monitorNonExistentDirectory = monitorNonExistentFile

-- | Monitor a single directory for existence. The monitored directory is
-- considered to have changed only if it no longer exists.
monitorDirectoryExistence :: FilePath -> MonitorFilePath
monitorDirectoryExistence = MonitorFile FileNotExists DirExists

-- | Monitor a single file or directory for changes, based on its modification
-- time. The monitored file is considered to have changed if it no longer
-- exists or if its modification time has changed.
monitorFileOrDirectory :: FilePath -> MonitorFilePath
monitorFileOrDirectory = MonitorFile FileModTime DirModTime

-- | Monitor a set of files (or directories) identified by a file glob.
-- The monitored glob is considered to have changed if the set of files
-- matching the glob changes (i.e. creations or deletions), or for files if the
-- modification time and content hash of any matching file has changed.
monitorFileGlob :: RootedGlob -> MonitorFilePath
monitorFileGlob = MonitorFileGlob FileHashed DirExists

-- | Monitor a set of files (or directories) identified by a file glob for
-- existence only. The monitored glob is considered to have changed if the set
-- of files matching the glob changes (i.e. creations or deletions).
monitorFileGlobExistence :: RootedGlob -> MonitorFilePath
monitorFileGlobExistence = MonitorFileGlob FileExists DirExists

-- | Creates a list of files to monitor when you search for a file which
-- unsuccessfully looked in @notFoundAtPaths@ before finding it at
-- @foundAtPath@.
monitorFileSearchPath :: [FilePath] -> FilePath -> [MonitorFilePath]
monitorFileSearchPath notFoundAtPaths foundAtPath =
  monitorFile foundAtPath
    : map monitorNonExistentFile notFoundAtPaths

-- | Similar to 'monitorFileSearchPath', but also instructs us to
-- monitor the hash of the found file.
monitorFileHashedSearchPath :: [FilePath] -> FilePath -> [MonitorFilePath]
monitorFileHashedSearchPath notFoundAtPaths foundAtPath =
  monitorFileHashed foundAtPath
    : map monitorNonExistentFile notFoundAtPaths

------------------------------------------------------------------------------
-- Parsing & pretty-printing
--

instance Pretty RootedGlob where
  pretty (RootedGlob root pathglob) = pretty root Disp.<> pretty pathglob

instance Parsec RootedGlob where
  parsec = do
    root <- parsec
    case root of
      FilePathRelative -> RootedGlob root <$> parsec
      _ -> RootedGlob root <$> parsec <|> pure (RootedGlob root GlobDirTrailing)

instance Pretty FilePathRoot where
  pretty FilePathRelative = Disp.empty
  pretty (FilePathRoot root) = Disp.text root
  pretty FilePathHomeDir = Disp.char '~' Disp.<> Disp.char '/'

instance Parsec FilePathRoot where
  parsec = root <|> P.try home <|> P.try drive <|> pure FilePathRelative
    where
      root = FilePathRoot "/" <$ P.char '/'
      home = FilePathHomeDir <$ P.string "~/"
      drive = do
        dr <- P.satisfy $ \c -> (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
        _ <- P.char ':'
        _ <- P.char '/' <|> P.char '\\'
        return (FilePathRoot (toUpper dr : ":\\"))
