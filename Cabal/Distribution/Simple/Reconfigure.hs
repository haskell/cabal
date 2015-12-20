{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
#if __GLASGOW_HASKELL__ >= 711
{-# LANGUAGE PatternSynonyms #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Reconfigure
-- Copyright   :  Thomas Tuegel 2015
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This implements the /reconfigure/ command and the bookkeeping that goes with
-- it. It saves the configuration flags in a version-independent format and
-- restores them on demand.

module Distribution.Simple.Reconfigure
       ( -- * Exceptions
         ConfigStateFileError(..), ReconfigureError(..)
         -- * Reconfiguration
       , Reconfigure, reconfigure
       , readArgs, writeArgs, setupConfigArgsFile
         -- * Persistent build configuration
       , writePersistBuildConfig, localBuildInfoFile
       , getPersistBuildConfig, tryGetPersistBuildConfig
       , maybeGetPersistBuildConfig
       , getConfigStateFile, tryGetConfigStateFile
       , checkPersistBuildConfigOutdated
       , currentCabalId, currentCompilerId
       ) where

import Distribution.Package
    ( PackageIdentifier(..), PackageName(PackageName), packageId )
import Distribution.Simple.Command ( CommandUI )
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(..))
import Distribution.Simple.Reconfigure.Generic hiding (reconfigure)
import qualified Distribution.Simple.Reconfigure.Generic as Generic
import Distribution.Simple.Setup ( ConfigFlags(..), toFlag )
import Distribution.Simple.UserHooks ( Args )
import Distribution.Simple.Utils ( cabalVersion, moreRecentFile, writeFileAtomic )
import Distribution.Text (display, simpleParse)

import Control.Exception
    ( Exception, evaluate, throw, throwIO, try )
#if __GLASGOW_HASKELL__ >= 711
import Control.Exception ( pattern ErrorCall )
#else
import Control.Exception ( ErrorCall(..) )
#endif
import Control.Monad (liftM, unless)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy.Char8 as BLC8
import Data.Maybe ( fromMaybe )
import Data.Typeable
import Distribution.Compat.Binary (decodeOrFailIO, encode)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ( (</>) )
import qualified System.Info (compilerName, compilerVersion)

-- -----------------------------------------------------------------------------
-- * Exceptions
-- -----------------------------------------------------------------------------

-- | The errors that can be thrown when reading the @setup-config@ file.
data ConfigStateFileError
    = ConfigStateFileNoHeader -- ^ No header found.
    | ConfigStateFileBadHeader -- ^ Incorrect header.
    | ConfigStateFileNoParse -- ^ Cannot parse file contents.
    | ConfigStateFileMissing -- ^ No file!
    | ConfigStateFileBadVersion
        PackageIdentifier
        PackageIdentifier
        (Either ConfigStateFileError LocalBuildInfo)
      -- ^ Mismatched version.
  deriving (Typeable)

instance Show ConfigStateFileError where
    show ConfigStateFileNoHeader =
        "Saved package config file header is missing. "
        ++ "Try re-running the 'configure' command."
    show ConfigStateFileBadHeader =
        "Saved package config file header is corrupt. "
        ++ "Try re-running the 'configure' command."
    show ConfigStateFileNoParse =
        "Saved package config file body is corrupt. "
        ++ "Try re-running the 'configure' command."
    show ConfigStateFileMissing = "Run the 'configure' command first."
    show (ConfigStateFileBadVersion oldCabal oldCompiler _) =
        "You need to re-run the 'configure' command. "
        ++ "The version of Cabal being used has changed (was "
        ++ display oldCabal ++ ", now "
        ++ display currentCabalId ++ ")."
        ++ badCompiler
      where
        badCompiler
          | oldCompiler == currentCompilerId = ""
          | otherwise =
              " Additionally the compiler is different (was "
              ++ display oldCompiler ++ ", now "
              ++ display currentCompilerId
              ++ ") which is probably the cause of the problem."

instance Exception ConfigStateFileError

-- -----------------------------------------------------------------------------
-- * Reconfiguration
-- -----------------------------------------------------------------------------

reconfigure :: Bool  -- ^ reconfigure even if unnecessary
            -> CommandUI ConfigFlags
            -> (ConfigFlags -> Args -> IO LocalBuildInfo)
               -- ^ configure action
            -> Reconfigure ConfigFlags LocalBuildInfo
reconfigure force command action =
  Generic.reconfigure force command setVerbosity getPersistBuildConfig action setupConfigArgsFile
  where
    setVerbosity flags verbosity = flags { configVerbosity = toFlag verbosity }

-- | The path (relative to @--build-dir@) where the arguments to @configure@
-- should be saved.
setupConfigArgsFile :: FilePath -> FilePath
setupConfigArgsFile = (</> "setup-config-args")

-- -----------------------------------------------------------------------------
-- * Persistent build configuration
-- -----------------------------------------------------------------------------

-- | After running configure, output the 'LocalBuildInfo' to the
-- 'localBuildInfoFile'.
writePersistBuildConfig :: FilePath
                        -> LocalBuildInfo -- ^ The 'LocalBuildInfo' to write.
                        -> IO ()
writePersistBuildConfig distPref lbi = do
    createDirectoryIfMissing False distPref
    writeFileAtomic (localBuildInfoFile distPref) $
      BLC8.unlines [showHeader pkgId, encode lbi]
  where
    pkgId = packageId $ localPkgDescr lbi

-- | Read the 'localBuildInfoFile'. Throw an exception if the file is
-- missing, if the file cannot be read, or if the file was created by an older
-- version of Cabal.
getPersistBuildConfig :: FilePath -- ^ The @dist@ directory path.
                      -> IO LocalBuildInfo
getPersistBuildConfig = getConfigStateFile . localBuildInfoFile

-- | Try to read the 'localBuildInfoFile'.
tryGetPersistBuildConfig :: FilePath -- ^ The @dist@ directory path.
                         -> IO (Either ConfigStateFileError LocalBuildInfo)
tryGetPersistBuildConfig = try . getPersistBuildConfig

-- | Try to read the 'localBuildInfoFile'.
maybeGetPersistBuildConfig :: FilePath -- ^ The @dist@ directory path.
                           -> IO (Maybe LocalBuildInfo)
maybeGetPersistBuildConfig =
    liftM (either (const Nothing) Just) . tryGetPersistBuildConfig

-- | Read the 'localBuildInfoFile'.  Throw an exception if the file is
-- missing, if the file cannot be read, or if the file was created by an older
-- version of Cabal.
getConfigStateFile :: FilePath -- ^ The file path of the @setup-config@ file.
                   -> IO LocalBuildInfo
getConfigStateFile filename = do
    exists <- doesFileExist filename
    unless exists $ throwIO ConfigStateFileMissing
    -- Read the config file into a strict ByteString to avoid problems with
    -- lazy I/O, then convert to lazy because the binary package needs that.
    contents <- BS.readFile filename
    let (header, body) = BLC8.span (/='\n') (BLC8.fromChunks [contents])

    headerParseResult <- try $ evaluate $ parseHeader header
    let (cabalId, compId) =
            case headerParseResult of
              Left (ErrorCall _) -> throw ConfigStateFileBadHeader
              Right x -> x

    let getStoredValue = do
          result <- decodeOrFailIO (BLC8.tail body)
          case result of
            Left _ -> throwIO ConfigStateFileNoParse
            Right x -> return x
        deferErrorIfBadVersion act
          | cabalId /= currentCabalId = do
              eResult <- try act
              throwIO $ ConfigStateFileBadVersion cabalId compId eResult
          | otherwise = act
    deferErrorIfBadVersion getStoredValue

-- | Read the 'localBuildInfoFile', returning either an error or the local build info.
tryGetConfigStateFile :: FilePath -- ^ The file path of the @setup-config@ file.
                      -> IO (Either ConfigStateFileError LocalBuildInfo)
tryGetConfigStateFile = try . getConfigStateFile

-- | Identifier of the current Cabal package.
currentCabalId :: PackageIdentifier
currentCabalId = PackageIdentifier (PackageName "Cabal") cabalVersion

-- | Identifier of the current compiler package.
currentCompilerId :: PackageIdentifier
currentCompilerId =
  PackageIdentifier
    (PackageName System.Info.compilerName)
    System.Info.compilerVersion

-- | Parse the @setup-config@ file header, returning the package identifiers
-- for Cabal and the compiler.
parseHeader :: ByteString -- ^ The file contents.
            -> (PackageIdentifier, PackageIdentifier)
parseHeader header = case BLC8.words header of
  [ "Saved", "package", "config", "for"
    , pkgId
    , "written", "by"
    , cabalId
    , "using"
    , compId ] ->
      fromMaybe (throw ConfigStateFileBadHeader) $ do
          _ <- simpleParse (BLC8.unpack pkgId) :: Maybe PackageIdentifier
          cabalId' <- simpleParse (BLC8.unpack cabalId)
          compId' <- simpleParse (BLC8.unpack compId)
          return (cabalId', compId')
  _ -> throw ConfigStateFileNoHeader

-- | Generate the @setup-config@ file header.
showHeader :: PackageIdentifier -- ^ The processed package.
            -> ByteString
showHeader pkgId = BLC8.unwords
    [ "Saved", "package", "config", "for"
    , BLC8.pack $ display pkgId
    , "written", "by"
    , BLC8.pack $ display currentCabalId
    , "using"
    , BLC8.pack $ display currentCompilerId
    ]

-- | Check that localBuildInfoFile is up-to-date with respect to the
-- .cabal file.
checkPersistBuildConfigOutdated :: FilePath -> FilePath -> IO Bool
checkPersistBuildConfigOutdated distPref pkg_descr_file =
  pkg_descr_file `moreRecentFile` (localBuildInfoFile distPref)

-- | Get the path of @dist\/setup-config@.
localBuildInfoFile :: FilePath -- ^ The @dist@ directory path.
                   -> FilePath
localBuildInfoFile distPref = distPref </> "setup-config"
