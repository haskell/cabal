{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Solver.Types.PkgConfigDb
-- Copyright   :  (c) Iñaki García Etxebarria 2016
-- License     :  BSD-like
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Read the list of packages available to pkg-config.
-----------------------------------------------------------------------------
module Distribution.Solver.Types.PkgConfigDb
    ( PkgConfigDb (..)
    , readPkgConfigDb
    , pkgConfigDbFromList
    , pkgConfigPkgIsPresent
    , pkgConfigDbPkgVersion
    , getPkgConfigDbDirs
    ) where

import Distribution.Solver.Compat.Prelude
import Prelude ()

import           Control.Exception (handle)
import           Control.Monad     (mapM)
import qualified Data.Map          as M
import           System.FilePath   (splitSearchPath)

import Distribution.Compat.Environment          (lookupEnv)
import Distribution.Package                     (PkgconfigName, mkPkgconfigName)
import Distribution.Parsec
import Distribution.Simple.Program
       (ProgramDb, getProgramOutput, pkgConfigProgram, needProgram, ConfiguredProgram)
import Distribution.Simple.Program.Run          (getProgramInvocationOutputAndErrors, programInvocation)
import Distribution.Simple.Utils                (info)
import Distribution.Types.PkgconfigVersion
import Distribution.Types.PkgconfigVersionRange
import Distribution.Verbosity                   (Verbosity)

-- | The list of packages installed in the system visible to
-- @pkg-config@. This is an opaque datatype, to be constructed with
-- `readPkgConfigDb` and queried with `pkgConfigPkgPresent`.
data PkgConfigDb =  PkgConfigDb (M.Map PkgconfigName (Maybe PkgconfigVersion))
                 -- ^ If an entry is `Nothing`, this means that the
                 -- package seems to be present, but we don't know the
                 -- exact version (because parsing of the version
                 -- number failed).
                 | NoPkgConfigDb
                 -- ^ For when we could not run pkg-config successfully.
     deriving (Show, Generic, Typeable)

instance Binary PkgConfigDb
instance Structured PkgConfigDb

-- | Query pkg-config for the list of installed packages, together
-- with their versions. Return a `PkgConfigDb` encapsulating this
-- information.
readPkgConfigDb :: Verbosity -> ProgramDb -> IO PkgConfigDb
readPkgConfigDb verbosity progdb = handle ioErrorHandler $ do
    mpkgConfig <- needProgram verbosity pkgConfigProgram progdb
    case mpkgConfig of
      Nothing             -> noPkgConfig "Cannot find pkg-config program"
      Just (pkgConfig, _) -> do
        pkgList <- lines <$> getProgramOutput verbosity pkgConfig ["--list-all"]
        -- The output of @pkg-config --list-all@ also includes a description
        -- for each package, which we do not need.
        let pkgNames = map (takeWhile (not . isSpace)) pkgList
        (pkgVersions, _errs, exitCode) <-
                     getProgramInvocationOutputAndErrors verbosity
                       (programInvocation pkgConfig ("--modversion" : pkgNames))
        if exitCode == ExitSuccess && length pkgNames == length pkgList
          then (return . pkgConfigDbFromList . zip pkgNames) (lines pkgVersions)
          else
          -- if there's a single broken pc file the above fails, so we fall back
          -- into calling it individually
          --
          -- Also some implementations of @pkg-config@ do not provide more than
          -- one package version, so if the returned list is shorter than the
          -- requested one, we fall back to querying one by one.
          do
            info verbosity ("call to pkg-config --modversion on all packages failed. Falling back to querying pkg-config individually on each package")
            pkgConfigDbFromList . catMaybes <$> mapM (getIndividualVersion pkgConfig) pkgNames
  where
    -- For when pkg-config invocation fails (possibly because of a
    -- too long command line).
    noPkgConfig extra = do
        info verbosity ("Failed to query pkg-config, Cabal will continue"
                        ++ " without solving for pkg-config constraints: "
                        ++ extra)
        return NoPkgConfigDb

    ioErrorHandler :: IOException -> IO PkgConfigDb
    ioErrorHandler e = noPkgConfig (show e)

    getIndividualVersion :: ConfiguredProgram -> String -> IO (Maybe (String, String))
    getIndividualVersion pkgConfig pkg = do
       (pkgVersion, _errs, exitCode) <-
               getProgramInvocationOutputAndErrors verbosity
                 (programInvocation pkgConfig ["--modversion", pkg])
       return $ case exitCode of
         ExitSuccess -> Just (pkg, pkgVersion)
         _ -> Nothing

-- | Create a `PkgConfigDb` from a list of @(packageName, version)@ pairs.
pkgConfigDbFromList :: [(String, String)] -> PkgConfigDb
pkgConfigDbFromList pairs = (PkgConfigDb . M.fromList . map convert) pairs
    where
      convert :: (String, String) -> (PkgconfigName, Maybe PkgconfigVersion)
      convert (n,vs) = (mkPkgconfigName n, simpleParsec vs)

-- | Check whether a given package range is satisfiable in the given
-- @pkg-config@ database.
pkgConfigPkgIsPresent :: PkgConfigDb -> PkgconfigName -> PkgconfigVersionRange -> Bool
pkgConfigPkgIsPresent (PkgConfigDb db) pn vr =
    case M.lookup pn db of
      Nothing       -> False    -- Package not present in the DB.
      Just Nothing  -> True     -- Package present, but version unknown.
      Just (Just v) -> withinPkgconfigVersionRange v vr
-- If we could not read the pkg-config database successfully we fail.
-- The plan found by the solver can't be executed later, because pkg-config itself
-- is going to be called in the build phase to get the library location for linking
-- so even if there is a library, it would need to be passed manual flags anyway.
pkgConfigPkgIsPresent NoPkgConfigDb _ _ = False



-- | Query the version of a package in the @pkg-config@ database.
-- @Nothing@ indicates the package is not in the database, while
-- @Just Nothing@ indicates that the package is in the database,
-- but its version is not known.
pkgConfigDbPkgVersion :: PkgConfigDb -> PkgconfigName -> Maybe (Maybe PkgconfigVersion)
pkgConfigDbPkgVersion (PkgConfigDb db) pn = M.lookup pn db
-- NB: Since the solver allows solving to succeed if there is
-- NoPkgConfigDb, we should report that we *guess* that there
-- is a matching pkg-config configuration, but that we just
-- don't know about it.
pkgConfigDbPkgVersion NoPkgConfigDb _ = Just Nothing


-- | Query pkg-config for the locations of pkg-config's package files. Use this
-- to monitor for changes in the pkg-config DB.
--
getPkgConfigDbDirs :: Verbosity -> ProgramDb -> IO [FilePath]
getPkgConfigDbDirs verbosity progdb =
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
      mpkgConfig <- needProgram verbosity pkgConfigProgram progdb
      case mpkgConfig of
        Nothing -> return []
        Just (pkgConfig, _) -> parseSearchPath <$>
          getProgramOutput verbosity pkgConfig ["--variable", "pc_path", "pkg-config"]

    parseSearchPath str =
      case lines str of
        [p] | not (null p) -> splitSearchPath p
        _                  -> []

    ioErrorHandler :: IOException -> IO [FilePath]
    ioErrorHandler _e = return []
