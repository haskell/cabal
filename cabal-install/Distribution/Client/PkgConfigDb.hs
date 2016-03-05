{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.PkgConfigDb
-- Copyright   :  (c) Iñaki García Etxebarria 2016
-- License     :  BSD-like
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Read the list of packages available to pkg-config.
-----------------------------------------------------------------------------
module Distribution.Client.PkgConfigDb
    ( PkgConfigDb
    , readPkgConfigDb
    , pkgConfigDbFromList
    , pkgConfigPkgIsPresent
    , getPkgConfigDbDirs
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>), (<*>))
#endif

import Control.Exception (IOException, handle)
import Data.Char (isSpace)
import qualified Data.Map as M
import Data.Version (parseVersion)
import Text.ParserCombinators.ReadP (readP_to_S)
import System.FilePath (splitSearchPath)

import Distribution.Package
    ( PackageName(..) )
import Distribution.Verbosity
    ( Verbosity )
import Distribution.Version
    ( Version, VersionRange, withinRange )

import Distribution.Compat.Environment
    ( lookupEnv )
import Distribution.Simple.Program
    ( ProgramConfiguration, pkgConfigProgram, getProgramOutput,
      requireProgram )
import Distribution.Simple.Utils
    ( info )

-- | The list of packages installed in the system visible to
-- @pkg-config@. This is an opaque datatype, to be constructed with
-- `readPkgConfigDb` and queried with `pkgConfigPkgPresent`.
data PkgConfigDb =  PkgConfigDb (M.Map PackageName (Maybe Version))
                 -- ^ If an entry is `Nothing`, this means that the
                 -- package seems to be present, but we don't know the
                 -- exact version (because parsing of the version
                 -- number failed).
                 | NoPkgConfigDb
                 -- ^ For when we could not run pkg-config successfully.
     deriving (Show)

-- | Query pkg-config for the list of installed packages, together
-- with their versions. Return a `PkgConfigDb` encapsulating this
-- information.
readPkgConfigDb :: Verbosity -> ProgramConfiguration -> IO PkgConfigDb
readPkgConfigDb verbosity conf = handle ioErrorHandler $ do
  (pkgConfig, _) <- requireProgram verbosity pkgConfigProgram conf
  pkgList <- lines <$> getProgramOutput verbosity pkgConfig ["--list-all"]
  -- The output of @pkg-config --list-all@ also includes a description
  -- for each package, which we do not need.
  let pkgNames = map (takeWhile (not . isSpace)) pkgList
  pkgVersions <- lines <$> getProgramOutput verbosity pkgConfig
                             ("--modversion" : pkgNames)
  (return . pkgConfigDbFromList . zip pkgNames) pkgVersions
      where
        -- For when pkg-config invocation fails (possibly because of a
        -- too long command line).
        ioErrorHandler :: IOException -> IO PkgConfigDb
        ioErrorHandler e = do
          info verbosity ("Failed to query pkg-config, Cabal will continue"
                          ++ " without solving for pkg-config constraints: "
                          ++ show e)
          return NoPkgConfigDb

-- | Create a `PkgConfigDb` from a list of @(packageName, version)@ pairs.
pkgConfigDbFromList :: [(String, String)] -> PkgConfigDb
pkgConfigDbFromList pairs = (PkgConfigDb . M.fromList . map convert) pairs
    where
      convert :: (String, String) -> (PackageName, Maybe Version)
      convert (n,vs) = (PackageName n,
                        case (reverse . readP_to_S parseVersion) vs of
                          (v, "") : _ -> Just v
                          _           -> Nothing -- Version not (fully)
                                                 -- understood.
                       )

-- | Check whether a given package range is satisfiable in the given
-- @pkg-config@ database.
pkgConfigPkgIsPresent :: PkgConfigDb -> PackageName -> VersionRange -> Bool
pkgConfigPkgIsPresent (PkgConfigDb db) pn vr =
    case M.lookup pn db of
      Nothing       -> False    -- Package not present in the DB.
      Just Nothing  -> True     -- Package present, but version unknown.
      Just (Just v) -> withinRange v vr
-- If we could not read the pkg-config database successfully we allow
-- the check to succeed. The plan found by the solver may fail to be
-- executed later on, but we have no grounds for rejecting the plan at
-- this stage.
pkgConfigPkgIsPresent NoPkgConfigDb _ _ = True


-- | Query pkg-config for the locations of pkg-config's package files. Use this
-- to monitor for changes in the pkg-config DB.
--
getPkgConfigDbDirs :: Verbosity -> ProgramConfiguration -> IO [FilePath]
getPkgConfigDbDirs verbosity conf =
    (++) <$> getEnvPath <*> getDefPath
 where
    -- According to @man pkg-config@:
    --
    -- PKG_CONFIG_PATH
    -- A  colon-separated  (on Windows, semicolon-separated) list of directories
    -- to search for .pc files.  The default directory will always be searched
    -- after searching the path
    --
    getEnvPath = maybe [] parseSearchPath
             <$> lookupEnv "PKG_CONFIG_PATH"

    -- Again according to @man pkg-config@:
    --
    -- pkg-config can be used to query itself for the default search path,
    -- version number and other information, for instance using:
    --
    -- > pkg-config --variable pc_path pkg-config
    --
    getDefPath = handle ioErrorHandler $ do
      (pkgConfig, _) <- requireProgram verbosity pkgConfigProgram conf
      parseSearchPath <$>
        getProgramOutput verbosity pkgConfig
                         ["--variable", "pc_path", "pkg-config"]

    parseSearchPath str =
      case lines str of
        [p] | not (null p) -> splitSearchPath p
        _                  -> []

    ioErrorHandler :: IOException -> IO [FilePath]
    ioErrorHandler _e = return []

