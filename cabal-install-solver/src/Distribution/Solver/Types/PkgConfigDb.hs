{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
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
import Prelude (read)

import           Control.Exception (handle, handleJust)
import qualified Data.Map          as M
import           System.FilePath   (splitSearchPath, takeBaseName, (</>))

import Distribution.Compat.Environment          (lookupEnv)
import Distribution.Compat.Directory
import Distribution.Compat.Time                 (getModTime, ModTime)
import Distribution.Package                     (PkgconfigName, mkPkgconfigName)
import Distribution.Parsec
import Distribution.Simple.Program
       (ProgramDb, getProgramOutput, pkgConfigProgram, needProgram, ConfiguredProgram)
import Distribution.Simple.Program.Run          (getProgramInvocationOutputAndErrors, programInvocation)
import Distribution.Simple.Utils                (info)
import Distribution.Types.PkgconfigVersion
import Distribution.Types.PkgconfigVersionRange
import Distribution.Verbosity                   (Verbosity)
import System.IO.Error                          (isDoesNotExistError)

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
        -- TODO use a more sensible data structure
        -- should I just add a `ModTime` field to `PkgConfigDb` and serialise that directly?
        -- TODO don't hardcode path (how to get? does this need to be its own preference?)
        let cacheFile = "/home/gthomas/.local/state/cabal/pkg-config"
            writeCache = writeFile cacheFile . show @[(String, ModTime, String)]
            readCache = handleJust (guard . isDoesNotExistError) (\() -> pure []) $ read @[(String, ModTime, String)] <$> readFile cacheFile
        -- TODO more logging
        pcPaths <- splitOn ":" . dropWhileEnd isSpace
            -- TODO verbose logs imply we already call this elsewhere
            <$> getProgramOutput verbosity pkgConfig ["--variable", "pc_path", "pkg-config"]
        -- ["/usr/lib/pkgconfig", "/usr/share/pkgconfig"]
        ts <- traverse (\(d, f) -> (takeBaseName f,) <$> getModTime (d </> f))
            -- TODO why are there directories here (`personality`)?
            -- note that if we remove them, we get exactly the same list here that we'd get from `pkg-config --list-package-names`
            -- should we sanity-check that, or is it part of the `pkg-config` spec?
            -- =<< filterM (fmap not . doesFileExist)
            =<< foldMap (\p -> map (p,) <$> listDirectory p) pcPaths
        cache <- M.fromList . map (\(p, t, v) -> (p, (t, v))) <$> readCache
        r <- fmap catMaybes $ for ts $ \(p, t) -> do
            case M.lookup p cache of
                Just (storedTime, storedVersion) | t == storedTime -> pure $ Just (p, t, storedVersion)
                -- TODO the `Nothing` case is (on my machine) always for packages with missing dependencies
                -- since this call will always fail for them, for these never get cached, and thus get checked again on every iteration
                -- but maybe that's fine? since we'd hope there aren't many of them
                -- TODO inline `getIndividualVersion` and add better error reporting?
                _ -> fmap ((p, t,) . snd) <$> getIndividualVersion pkgConfig p
        writeCache r
        pure $ pkgConfigDbFromList $ map (\(p, _t, v) -> (p, v)) r
  where
    -- For when pkg-config invocation fails (possibly because of a
    -- too long command line).
    noPkgConfig extra = do
        info verbosity ("Failed to query pkg-config, Cabal will continue"
                        ++ " without solving for pkg-config constraints: "
                        ++ extra)
        return NoPkgConfigDb

    -- TODO vendored from `extra` - what should I do?
    splitOn :: (Eq a) => [a] -> [a] -> [[a]]
    splitOn [] _ = error "splitOn, needle may not be empty"
    splitOn _ [] = [[]]
    splitOn needle haystack = a : if null b then [] else splitOn needle $ drop (length needle) b
      where
        (a, b) = breakOn needle haystack
    breakOn :: Eq a => [a] -> [a] -> ([a], [a])
    breakOn needle haystack | needle `isPrefixOf` haystack = ([], haystack)
    breakOn _ [] = ([], [])
    breakOn needle (x:xs) = first (x:) $ breakOn needle xs

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
