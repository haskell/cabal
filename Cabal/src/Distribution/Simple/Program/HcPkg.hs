{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Module      :  Distribution.Simple.Program.HcPkg
-- Copyright   :  Duncan Coutts 2009, 2013
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This module provides an library interface to the @hc-pkg@ program.
-- Currently only GHC and GHCJS have hc-pkg programs.
module Distribution.Simple.Program.HcPkg
  ( -- * Types
    ConfiguredProgram (..)
  , RegisterOptions (..)
  , defaultRegisterOptions

    -- * Actions
  , init
  , invoke
  , register
  , unregister
  , recache
  , expose
  , hide
  , dump
  , describe
  , list

    -- * Program invocations
  , initInvocation
  , registerInvocation
  , unregisterInvocation
  , recacheInvocation
  , exposeInvocation
  , hideInvocation
  , dumpInvocation
  , describeInvocation
  , listInvocation
  ) where

import Distribution.Compat.Prelude hiding (init)
import Prelude ()

import Distribution.InstalledPackageInfo (InstalledPackageInfo (..), parseInstalledPackageInfo, showInstalledPackageInfo)
import Distribution.Parsec (simpleParsec)
import Distribution.Pretty (prettyShow)
import Distribution.Simple.Compiler
  ( PackageDB
  , PackageDBS
  , PackageDBStack
  , PackageDBStackS
  , PackageDBX (..)
  , registrationPackageDB
  )
import Distribution.Simple.Errors (CabalException (..))
import Distribution.Simple.Program.Run
  ( IOEncoding (..)
  , ProgramInvocation (..)
  , getProgramInvocationLBS
  , getProgramInvocationOutput
  , programInvocation
  , programInvocationCwd
  , runProgramInvocation
  )
import Distribution.Simple.Program.Types (ConfiguredProgram (..))
import Distribution.Simple.Utils (IOData (..), dieWithException, writeUTF8File)
import Distribution.Types.ComponentId (mkComponentId)
import Distribution.Types.PackageId (PackageId)
import Distribution.Types.UnitId (mkLegacyUnitId, unUnitId)
import Distribution.Utils.Path
  ( CWD
  , FileLike ((<.>))
  , FileOrDir (Dir)
  , PathLike ((</>))
  , Pkg
  , PkgDB
  , SymbolicPath
  , interpretSymbolicPath
  , interpretSymbolicPathCWD
  )
import Distribution.Verbosity (Verbosity, VerbosityLevel (..), verbosityLevel)

import Data.List (stripPrefix)
import System.FilePath as FilePath
  ( isPathSeparator
  , joinPath
  , splitDirectories
  , splitPath
  )

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List.NonEmpty as NE
import qualified System.FilePath.Posix as FilePath.Posix

-- | Call @hc-pkg@ to initialise a package database at the location {path}.
--
-- > hc-pkg init {path}
init :: ConfiguredProgram -> Verbosity -> FilePath -> IO ()
init hpi verbosity path =
  runProgramInvocation verbosity (initInvocation hpi verbosity path)

-- | Run @hc-pkg@ using a given package DB stack, directly forwarding the
-- provided command-line arguments to it.
invoke
  :: ConfiguredProgram
  -> Verbosity
  -> Maybe (SymbolicPath CWD (Dir Pkg))
  -> PackageDBStack
  -> [String]
  -> IO ()
invoke ghcProg verbosity mbWorkDir dbStack extraArgs =
  runProgramInvocation verbosity invocation
  where
    args = packageDbStackOpts dbStack ++ extraArgs
    invocation = programInvocationCwd mbWorkDir ghcProg args

-- | Additional variations in the behaviour for 'register'.
data RegisterOptions = RegisterOptions
  { registerAllowOverwrite :: Bool
  -- ^ Allows re-registering \/ overwriting an existing package
  , registerMultiInstance :: Bool
  -- ^ Insist on the ability to register multiple instances of a
  -- single version of a single package.
  , registerSuppressFilesCheck :: Bool
  -- ^ Require that no checks are performed on the existence of package
  -- files mentioned in the registration info. This must be used if
  -- registering prior to putting the files in their final place. This will
  -- fail if the @hc-pkg@ does not support it, see 'suppressFilesCheck'.
  }

-- | Defaults are @True@, @False@ and @False@
defaultRegisterOptions :: RegisterOptions
defaultRegisterOptions =
  RegisterOptions
    { registerAllowOverwrite = True
    , registerMultiInstance = False
    , registerSuppressFilesCheck = False
    }

-- | Call @hc-pkg@ to register a package.
--
-- > hc-pkg register {filename | -} [--user | --global | --package-db]
register
  :: ConfiguredProgram
  -> Verbosity
  -> Maybe (SymbolicPath CWD (Dir from))
  -> PackageDBStackS from
  -> InstalledPackageInfo
  -> RegisterOptions
  -> IO ()
register hpi verbosity mbWorkDir packagedbs pkgInfo registerOptions
  | registerMultiInstance registerOptions =
      do
        let pkgdb = registrationPackageDB packagedbs
        writeRegistrationFileDirectly verbosity mbWorkDir pkgdb pkgInfo
        recache hpi verbosity mbWorkDir pkgdb
  | otherwise =
      runProgramInvocation
        verbosity
        (registerInvocation hpi (verbosityLevel verbosity) mbWorkDir packagedbs pkgInfo registerOptions)

writeRegistrationFileDirectly
  :: Verbosity
  -> Maybe (SymbolicPath CWD (Dir from))
  -> PackageDBS from
  -> InstalledPackageInfo
  -> IO ()
writeRegistrationFileDirectly verbosity mbWorkDir package pkgInfo =
  case package of
    (SpecificPackageDB dir) -> do
      let pkgfile = interpretSymbolicPath mbWorkDir dir </> prettyShow (installedUnitId pkgInfo) <.> "conf"
      writeUTF8File pkgfile (showInstalledPackageInfo pkgInfo)
    _ -> do
      -- We don't know here what the dir for the global or user dbs are,
      -- if that's needed it'll require a bit more plumbing to support.
      dieWithException verbosity OnlySupportSpecificPackageDb

-- | Call @hc-pkg@ to unregister a package
--
-- > hc-pkg unregister [pkgid] [--user | --global | --package-db]
unregister :: ConfiguredProgram -> Verbosity -> Maybe (SymbolicPath CWD (Dir Pkg)) -> PackageDB -> PackageId -> IO ()
unregister hpi verbosity mbWorkDir packagedb pkgid =
  runProgramInvocation
    verbosity
    (unregisterInvocation hpi (verbosityLevel verbosity) mbWorkDir packagedb pkgid)

-- | Call @hc-pkg@ to recache the registered packages.
--
-- > hc-pkg recache [--user | --global | --package-db]
recache :: ConfiguredProgram -> Verbosity -> Maybe (SymbolicPath CWD (Dir from)) -> PackageDBS from -> IO ()
recache hpi verbosity mbWorkDir packagedb =
  runProgramInvocation
    verbosity
    (recacheInvocation hpi (verbosityLevel verbosity) mbWorkDir packagedb)

-- | Call @hc-pkg@ to expose a package.
--
-- > hc-pkg expose [pkgid] [--user | --global | --package-db]
expose
  :: ConfiguredProgram
  -> Verbosity
  -> Maybe (SymbolicPath CWD (Dir Pkg))
  -> PackageDB
  -> PackageId
  -> IO ()
expose hpi verbosity mbWorkDir packagedb pkgid =
  runProgramInvocation
    verbosity
    (exposeInvocation hpi (verbosityLevel verbosity) mbWorkDir packagedb pkgid)

-- | Call @hc-pkg@ to retrieve a specific package
--
-- > hc-pkg describe [pkgid] [--user | --global | --package-db]
describe
  :: ConfiguredProgram
  -> Verbosity
  -> Maybe (SymbolicPath CWD (Dir Pkg))
  -> PackageDBStack
  -> PackageId
  -> IO [InstalledPackageInfo]
describe ghcProg verbosity mbWorkDir packagedb pid = do
  output <-
    getProgramInvocationLBS
      verbosity
      (describeInvocation ghcProg (verbosityLevel verbosity) mbWorkDir packagedb pid)
      `catchIO` \_ -> return mempty

  case parsePackages output of
    Left ok -> return ok
    _ -> dieWithException verbosity $ FailedToParseOutputDescribe (programId ghcProg) pid

-- | Call @hc-pkg@ to hide a package.
--
-- > hc-pkg hide [pkgid] [--user | --global | --package-db]
hide
  :: ConfiguredProgram
  -> Verbosity
  -> Maybe (SymbolicPath CWD (Dir Pkg))
  -> PackageDB
  -> PackageId
  -> IO ()
hide hpi verbosity mbWorkDir packagedb pkgid =
  runProgramInvocation
    verbosity
    (hideInvocation hpi (verbosityLevel verbosity) mbWorkDir packagedb pkgid)

-- | Call @hc-pkg@ to get all the details of all the packages in the given
-- package database.
dump
  :: ConfiguredProgram
  -> Verbosity
  -> Maybe (SymbolicPath CWD (Dir from))
  -> PackageDBX (SymbolicPath from (Dir PkgDB))
  -> IO [InstalledPackageInfo]
dump ghcProg verbosity mbWorkDir packagedb = do
  output <-
    getProgramInvocationLBS
      verbosity
      (dumpInvocation ghcProg (verbosityLevel verbosity) mbWorkDir packagedb)
      `catchIO` \e ->
        dieWithException verbosity $ DumpFailed (programId ghcProg) (displayException e)

  case parsePackages output of
    Left ok -> return ok
    _ -> dieWithException verbosity $ FailedToParseOutputDump (programId ghcProg)

parsePackages :: LBS.ByteString -> Either [InstalledPackageInfo] [String]
parsePackages lbs0 =
  case traverse parseInstalledPackageInfo $ splitPkgs lbs0 of
    Right ok -> Left [setUnitId . maybe id mungePackagePaths (pkgRoot pkg) $ pkg | (_, pkg) <- ok]
    Left msgs -> Right (NE.toList msgs)
  where
    splitPkgs :: LBS.ByteString -> [BS.ByteString]
    splitPkgs = checkEmpty . doSplit
      where
        -- Handle the case of there being no packages at all.
        checkEmpty [s] | BS.all isSpace8 s = []
        checkEmpty ss = ss

        isSpace8 :: Word8 -> Bool
        isSpace8 9 = True -- '\t'
        isSpace8 10 = True -- '\n'
        isSpace8 13 = True -- '\r'
        isSpace8 32 = True -- ' '
        isSpace8 _ = False

        doSplit :: LBS.ByteString -> [BS.ByteString]
        doSplit lbs = go (LBS.findIndices (\w -> w == 10 || w == 13) lbs)
          where
            go :: [Int64] -> [BS.ByteString]
            go [] = [LBS.toStrict lbs]
            go (idx : idxs) =
              let (pfx, sfx) = LBS.splitAt idx lbs
               in case foldr ((<|>) . (`LBS.stripPrefix` sfx)) Nothing separators of
                    Just sfx' -> LBS.toStrict pfx : doSplit sfx'
                    Nothing -> go idxs

            separators :: [LBS.ByteString]
            separators = ["\n---\n", "\r\n---\r\n", "\r---\r"]

mungePackagePaths :: FilePath -> InstalledPackageInfo -> InstalledPackageInfo
-- Perform path/URL variable substitution as per the Cabal ${pkgroot} spec
-- (http://www.haskell.org/pipermail/libraries/2009-May/011772.html)
-- Paths/URLs can be relative to ${pkgroot} or ${pkgrooturl}.
-- The "pkgroot" is the directory containing the package database.
mungePackagePaths pkgroot pkginfo =
  pkginfo
    { importDirs = mungePaths (importDirs pkginfo)
    , includeDirs = mungePaths (includeDirs pkginfo)
    , libraryDirs = mungePaths (libraryDirs pkginfo)
    , libraryDirsStatic = mungePaths (libraryDirsStatic pkginfo)
    , libraryDynDirs = mungePaths (libraryDynDirs pkginfo)
    , frameworkDirs = mungePaths (frameworkDirs pkginfo)
    , haddockInterfaces = mungePaths (haddockInterfaces pkginfo)
    , haddockHTMLs = mungePaths (mungeUrls (haddockHTMLs pkginfo))
    }
  where
    mungePaths = map mungePath
    mungeUrls = map mungeUrl

    mungePath p = case stripVarPrefix "${pkgroot}" p of
      Just p' -> pkgroot </> p'
      Nothing -> p

    mungeUrl p = case stripVarPrefix "${pkgrooturl}" p of
      Just p' -> toUrlPath pkgroot p'
      Nothing -> p

    toUrlPath r p =
      "file:///"
        -- URLs always use posix style '/' separators:
        ++ FilePath.Posix.joinPath (r : FilePath.splitDirectories p)

    stripVarPrefix var p =
      case splitPath p of
        (root : path') -> case stripPrefix var root of
          Just [sep] | isPathSeparator sep -> Just (joinPath path')
          _ -> Nothing
        _ -> Nothing

-- Older installed package info files did not have the installedUnitId
-- field, so if it is missing then we fill it as the source package ID.
-- NB: Internal libraries not supported.
setUnitId :: InstalledPackageInfo -> InstalledPackageInfo
setUnitId
  pkginfo@InstalledPackageInfo
    { installedUnitId = uid
    , sourcePackageId = pid
    }
    | unUnitId uid == "" =
        pkginfo
          { installedUnitId = mkLegacyUnitId pid
          , installedComponentId_ = mkComponentId (prettyShow pid)
          }
setUnitId pkginfo = pkginfo

-- | Call @hc-pkg@ to get the source package Id of all the packages in the
-- given package database.
--
-- This is much less information than with 'dump', but also rather quicker.
-- Note in particular that it does not include the 'UnitId', just
-- the source 'PackageId' which is not necessarily unique in any package db.
list
  :: ConfiguredProgram
  -> Verbosity
  -> Maybe (SymbolicPath CWD (Dir Pkg))
  -> PackageDB
  -> IO [PackageId]
list ghcProg verbosity mbWorkDir packagedb = do
  output <-
    getProgramInvocationOutput
      verbosity
      (listInvocation ghcProg (verbosityLevel verbosity) mbWorkDir packagedb)
      `catchIO` \_ -> dieWithException verbosity $ ListFailed (programId ghcProg)

  case parsePackageIds output of
    Just ok -> return ok
    _ -> dieWithException verbosity $ FailedToParseOutputList (programId ghcProg)
  where
    parsePackageIds = traverse simpleParsec . words

--------------------------
-- The program invocations
--

initInvocation :: ConfiguredProgram -> Verbosity -> FilePath -> ProgramInvocation
initInvocation ghcProg verbosity path =
  programInvocation ghcProg args
  where
    args =
      ["init", path]
        ++ verbosityOpts (verbosityLevel verbosity)

registerInvocation
  :: ConfiguredProgram
  -> VerbosityLevel
  -> Maybe (SymbolicPath CWD (Dir from))
  -> PackageDBStackS from
  -> InstalledPackageInfo
  -> RegisterOptions
  -> ProgramInvocation
registerInvocation ghcProg verbosity mbWorkDir packagedbs pkgInfo registerOptions =
  (programInvocationCwd mbWorkDir ghcProg (args "-"))
    { progInvokeInput = Just $ IODataText $ showInstalledPackageInfo pkgInfo
    , progInvokeInputEncoding = IOEncodingUTF8
    }
  where
    cmdname
      | registerAllowOverwrite registerOptions = "update"
      | registerMultiInstance registerOptions = "update"
      | otherwise = "register"

    args file =
      [cmdname, file]
        ++ packageDbStackOpts packagedbs
        ++ [ "--enable-multi-instance"
           | registerMultiInstance registerOptions
           ]
        ++ [ "--force-files"
           | registerSuppressFilesCheck registerOptions
           ]
        ++ verbosityOpts verbosity

unregisterInvocation
  :: ConfiguredProgram
  -> VerbosityLevel
  -> Maybe (SymbolicPath CWD (Dir Pkg))
  -> PackageDB
  -> PackageId
  -> ProgramInvocation
unregisterInvocation ghcProg verbosity mbWorkDir packagedb pkgid =
  programInvocationCwd mbWorkDir ghcProg $
    ["unregister", packageDbOpts packagedb, prettyShow pkgid]
      ++ verbosityOpts verbosity

recacheInvocation
  :: ConfiguredProgram
  -> VerbosityLevel
  -> Maybe (SymbolicPath CWD (Dir from))
  -> PackageDBS from
  -> ProgramInvocation
recacheInvocation ghcProg verbosity mbWorkDir packagedb =
  programInvocationCwd mbWorkDir ghcProg $
    ["recache", packageDbOpts packagedb]
      ++ verbosityOpts verbosity

exposeInvocation
  :: ConfiguredProgram
  -> VerbosityLevel
  -> Maybe (SymbolicPath CWD (Dir Pkg))
  -> PackageDB
  -> PackageId
  -> ProgramInvocation
exposeInvocation ghcProg verbosity mbWorkDir packagedb pkgid =
  programInvocationCwd mbWorkDir ghcProg $
    ["expose", packageDbOpts packagedb, prettyShow pkgid]
      ++ verbosityOpts verbosity

describeInvocation
  :: ConfiguredProgram
  -> VerbosityLevel
  -> Maybe (SymbolicPath CWD (Dir Pkg))
  -> PackageDBStack
  -> PackageId
  -> ProgramInvocation
describeInvocation ghcProg verbosity mbWorkDir packagedbs pkgid =
  programInvocationCwd mbWorkDir ghcProg $
    ["describe", prettyShow pkgid]
      ++ packageDbStackOpts packagedbs
      ++ verbosityOpts verbosity

hideInvocation
  :: ConfiguredProgram
  -> VerbosityLevel
  -> Maybe (SymbolicPath CWD (Dir Pkg))
  -> PackageDB
  -> PackageId
  -> ProgramInvocation
hideInvocation ghcProg verbosity mbWorkDir packagedb pkgid =
  programInvocationCwd mbWorkDir ghcProg $
    ["hide", packageDbOpts packagedb, prettyShow pkgid]
      ++ verbosityOpts verbosity

dumpInvocation
  :: ConfiguredProgram
  -> VerbosityLevel
  -> Maybe (SymbolicPath CWD (Dir from))
  -> PackageDBX (SymbolicPath from (Dir PkgDB))
  -> ProgramInvocation
dumpInvocation ghcProg _verbosity mbWorkDir packagedb =
  (programInvocationCwd mbWorkDir ghcProg args)
    { progInvokeOutputEncoding = IOEncodingUTF8
    }
  where
    args =
      ["dump", packageDbOpts packagedb]
        ++ verbosityOpts Silent

-- We use verbosity level 'Silent' because it is important that we
-- do not contaminate the output with info/debug messages.

listInvocation
  :: ConfiguredProgram
  -> VerbosityLevel
  -> Maybe (SymbolicPath CWD (Dir Pkg))
  -> PackageDB
  -> ProgramInvocation
listInvocation ghcProg _verbosity mbWorkDir packagedb =
  (programInvocationCwd mbWorkDir ghcProg args)
    { progInvokeOutputEncoding = IOEncodingUTF8
    }
  where
    args =
      ["list", "--simple-output", packageDbOpts packagedb]
        ++ verbosityOpts Silent

-- We use verbosity level 'Silent' because it is important that we
-- do not contaminate the output with info/debug messages.

packageDbStackOpts :: PackageDBStackS from -> [String]
packageDbStackOpts dbstack = case dbstack of
  (GlobalPackageDB : UserPackageDB : dbs) ->
    "--global"
      : "--user"
      : map specific dbs
  (GlobalPackageDB : dbs) ->
    "--global"
      : "--no-user-package-db"
      : map specific dbs
  _ -> ierror
  where
    specific (SpecificPackageDB db) = "--package-db=" ++ interpretSymbolicPathCWD db
    specific _ = ierror
    ierror :: a
    ierror = error ("internal error: unexpected package db stack: " ++ show dbstack)

packageDbOpts :: PackageDBX (SymbolicPath from (Dir PkgDB)) -> String
packageDbOpts GlobalPackageDB = "--global"
packageDbOpts UserPackageDB = "--user"
packageDbOpts (SpecificPackageDB db) = "--package-db=" ++ interpretSymbolicPathCWD db

verbosityOpts :: VerbosityLevel -> [String]
verbosityOpts v
  | v >= Deafening = ["-v2"]
  | v == Silent = ["-v0"]
  | otherwise = []
