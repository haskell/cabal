{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

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
       , Reconfigure
       , reconfigure, forceReconfigure
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
import Distribution.Simple.Command
    ( CommandParse(..), CommandUI, commandAddAction, commandsRun
    , commandShowOptions )
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(..))
import Distribution.Simple.Setup
    ( ConfigFlags(..), GlobalFlags(..), defaultDistPref
    , fromFlag, fromFlagOrDefault, globalCommand, toFlag )
import Distribution.Simple.UserHooks (Args)
import Distribution.Simple.Utils
    ( cabalVersion, createDirectoryIfMissingVerbose, info, moreRecentFile
    , notice, writeFileAtomic )
import Distribution.Text (display, simpleParse)
import Distribution.Verbosity (Verbosity, lessVerbose)

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
import Data.Foldable (foldlM)
import Data.List (intercalate)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Typeable
import Distribution.Compat.Binary (decodeOrFailIO, encode)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>), takeDirectory)
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

data ReconfigureError
    = ReconfigureErrorHelp Args
    | ReconfigureErrorList Args
    | ReconfigureErrorOther Args [String]
  deriving (Typeable)

instance Show ReconfigureError where
  show (ReconfigureErrorHelp args) =
    "reconfigure: unexpected flag '--help', saved command line was:\n"
    ++ intercalate " " args
  show (ReconfigureErrorList args) =
    "reconfigure: unexpected flag '--list-options', saved command line was:\n"
    ++ intercalate " " args
  show (ReconfigureErrorOther args errs) =
    "reconfigure: saved command line was:\n"
    ++ intercalate " " args ++ "\n"
    ++ "encountered errors:\n"
    ++ intercalate "\n" errs

instance Exception ReconfigureError

-- -----------------------------------------------------------------------------
-- * Reconfiguration
-- -----------------------------------------------------------------------------

type Requirement = ConfigFlags -> Maybe (ConfigFlags, String)
type Reconfigure = [Requirement] -> Verbosity -> FilePath -> IO LocalBuildInfo

-- | Read the 'localBuildInfoFile', reconfiguring if necessary. Throws
-- 'ConfigStateFileError' if the file cannot be read and the package cannot
-- be reconfigured.
reconfigure :: CommandUI ConfigFlags
            -> (Args -> ConfigFlags -> Args -> IO LocalBuildInfo)
               -- ^ configure action
            -> (FilePath -> FilePath)
               -- ^ path to saved command-line arguments,
               -- relative to @--build-dir@
            -> Reconfigure
reconfigure cmd configureAction configArgsFile reqs verbosity dist = do
    elbi <- tryGetPersistBuildConfig dist
    lbi <- case elbi of
      Left err -> do info verbosity (show err)
                     forceReconfigure_
      Right lbi_wo_programs -> return lbi_wo_programs
    case pkgDescrFile lbi of
      Nothing -> return lbi
      Just pkg_descr_file -> do
        outdated <- checkPersistBuildConfigOutdated dist pkg_descr_file
        if outdated
          then do
            notice verbosity (pkg_descr_file ++ " has changed; reconfiguring...")
            forceReconfigure_
          else if checkRequirements (configFlags lbi)
               then forceReconfigure_
               else return lbi
  where
    forceReconfigure_ = forceReconfigure cmd configureAction configArgsFile
                                         reqs_ verbosity dist

    reqs_ = reqs ++ extraRequirements dist

    checkRequirements :: ConfigFlags -> Bool -- ^ needs to be reconfigured?
    checkRequirements flags = (not . null . catMaybes) (map ($ flags) reqs_)

extraRequirements :: FilePath -> [Requirement]
extraRequirements dist = [ sameDistPref ]
  where
    sameDistPref config
      | savedDist == dist = Nothing
      | otherwise =
          let flags = config { configDistPref = toFlag dist }
          in Just (flags, "--build-dir changed")
      where savedDist = fromFlagOrDefault defaultDistPref (configDistPref config)

-- | Reconfigure the package unconditionally.
forceReconfigure :: CommandUI ConfigFlags
                 -> (Args -> ConfigFlags -> Args -> IO LocalBuildInfo)
                    -- ^ configure action
                 -> (FilePath -> FilePath)
                    -- ^ path to saved command-line arguments,
                    -- relative to @--build-dir@
                 -> Reconfigure
forceReconfigure cmd configureAction configArgsFile reqs verbosity distPref = do
    saved <- readArgs (configArgsFile distPref)
    let args = fromMaybe ["configure"] saved
        commands = [ commandAddAction cmd configureAction_ ]
    case commandsRun (globalCommand commands) commands args of
      CommandHelp   _    -> throwIO (ReconfigureErrorHelp args)
      CommandList   _    -> throwIO (ReconfigureErrorList args)
      CommandErrors errs -> throwIO (ReconfigureErrorOther args errs)
      CommandReadyToGo (flags, commandParse)  ->
        case commandParse of
          _ | fromFlag (globalVersion flags)
              || fromFlag (globalNumericVersion flags) ->
                throwIO (ReconfigureErrorOther args ["not a 'configure' command"])
          CommandHelp   _    -> throwIO (ReconfigureErrorHelp args)
          CommandList   _    -> throwIO (ReconfigureErrorList args)
          CommandErrors errs -> throwIO (ReconfigureErrorOther args errs)
          CommandReadyToGo action        -> action
  where
    configureAction_ :: ConfigFlags -> Args -> IO LocalBuildInfo
    configureAction_ config extraArgs = do
      flags <- setRequirements config { configVerbosity = toFlag verbosity }
      let args' = "configure" : commandShowOptions cmd flags
      configureAction args' flags extraArgs

    setRequirements :: ConfigFlags -> IO ConfigFlags
    setRequirements orig = foldlM setRequirements_go orig reqs where
      setRequirements_go flags req =
        case req flags of
          Nothing -> return flags
          Just (flags', reason) -> do
            notice verbosity ("reconfigure: " ++ reason)
            return flags'

-- | Write command-line arguments to a file, separated by null characters. This
-- format is also suitable for the @xargs -0@ command. Using the null
-- character also avoids the problem of escaping newlines or spaces,
-- because unlike other whitespace characters, the null character is
-- not valid in command-line arguments.
writeArgs :: Verbosity -> FilePath -> [String] -> IO ()
writeArgs verbosity path args = do
    createDirectoryIfMissingVerbose
      (lessVerbose verbosity) True (takeDirectory path)
    writeFile path (intercalate "\0" args)

-- | Read command-line arguments, separated by null characters, from a file.
-- Returns 'Nothing' if the file does not exist.
readArgs :: FilePath -> IO (Maybe [String])
readArgs path = do
  exists <- doesFileExist path
  if exists
     then liftM (Just . unintersperse '\0') (readFile path)
    else return Nothing

unintersperse :: Eq a => a -> [a] -> [[a]]
unintersperse _ [] = []
unintersperse mark list =
  let (this, rest) = break (== mark) list
  in case rest of
       [] -> [this]
       (_:rest') -> this : unintersperse mark rest'

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
checkPersistBuildConfigOutdated distPref pkg_descr_file = do
  pkg_descr_file `moreRecentFile` (localBuildInfoFile distPref)

-- | Get the path of @dist\/setup-config@.
localBuildInfoFile :: FilePath -- ^ The @dist@ directory path.
                   -> FilePath
localBuildInfoFile distPref = distPref </> "setup-config"
