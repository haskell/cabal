-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Program.HcPkg
-- Copyright   :  Duncan Coutts 2009, 2013
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This module provides an library interface to the @hc-pkg@ program.
-- Currently only GHC, GHCJS and LHC have hc-pkg programs.

module Distribution.Simple.Program.HcPkg (
    HcPkgInfo(..),

    init,
    invoke,
    register,
    reregister,
    unregister,
    expose,
    hide,
    dump,
    list,

    -- * Program invocations
    initInvocation,
    registerInvocation,
    reregisterInvocation,
    unregisterInvocation,
    exposeInvocation,
    hideInvocation,
    dumpInvocation,
    listInvocation,
  ) where

import Prelude hiding (init)
import Distribution.Package
         ( PackageId, InstalledPackageId(..) )
import Distribution.InstalledPackageInfo
         ( InstalledPackageInfo, InstalledPackageInfo_(..)
         , showInstalledPackageInfo
         , emptyInstalledPackageInfo, fieldsInstalledPackageInfo )
import Distribution.ParseUtils
import Distribution.Simple.Compiler
         ( PackageDB(..), PackageDBStack )
import Distribution.Simple.Program.Types
         ( ConfiguredProgram(programId) )
import Distribution.Simple.Program.Run
         ( ProgramInvocation(..), IOEncoding(..), programInvocation
         , runProgramInvocation, getProgramInvocationOutput )
import Distribution.Text
         ( display, simpleParse )
import Distribution.Simple.Utils
         ( die )
import Distribution.Verbosity
         ( Verbosity, deafening, silent )
import Distribution.Compat.Exception
         ( catchExit )

import Data.Char
         ( isSpace )
import Data.List
         ( stripPrefix )
import System.FilePath as FilePath
         ( (</>), splitPath, splitDirectories, joinPath, isPathSeparator )
import qualified System.FilePath.Posix as FilePath.Posix

-- | Information about the features and capabilities of an @hc-pkg@
--   program.
--
data HcPkgInfo = HcPkgInfo
  { hcPkgProgram    :: ConfiguredProgram
  , noPkgDbStack    :: Bool -- ^ no package DB stack supported
  , noVerboseFlag   :: Bool -- ^ hc-pkg does not support verbosity flags
  , flagPackageConf :: Bool -- ^ use package-conf option instead of package-db
  , useSingleFileDb :: Bool -- ^ requires single file package database
  , multInstEnabled :: Bool -- ^ ghc-pkg supports --enable-multi-instance
  }

-- | Call @hc-pkg@ to initialise a package database at the location {path}.
--
-- > hc-pkg init {path}
--
init :: HcPkgInfo -> Verbosity -> FilePath -> IO ()
init hpi verbosity path =
  runProgramInvocation verbosity (initInvocation hpi verbosity path)

-- | Run @hc-pkg@ using a given package DB stack, directly forwarding the
-- provided command-line arguments to it.
invoke :: HcPkgInfo -> Verbosity -> PackageDBStack -> [String] -> IO ()
invoke hpi verbosity dbStack extraArgs =
  runProgramInvocation verbosity invocation
  where
    args       = packageDbStackOpts hpi dbStack ++ extraArgs
    invocation = programInvocation (hcPkgProgram hpi) args

-- | Call @hc-pkg@ to register a package.
--
-- > hc-pkg register {filename | -} [--user | --global | --package-db]
--
register :: HcPkgInfo -> Verbosity -> PackageDBStack
         -> Either FilePath
                   InstalledPackageInfo
         -> IO ()
register hpi verbosity packagedb pkgFile =
  runProgramInvocation verbosity
    (registerInvocation hpi verbosity packagedb pkgFile)


-- | Call @hc-pkg@ to re-register a package.
--
-- > hc-pkg register {filename | -} [--user | --global | --package-db]
--
reregister :: HcPkgInfo -> Verbosity -> PackageDBStack
           -> Either FilePath
                     InstalledPackageInfo
           -> IO ()
reregister hpi verbosity packagedb pkgFile =
  runProgramInvocation verbosity
    (reregisterInvocation hpi verbosity packagedb pkgFile)


-- | Call @hc-pkg@ to unregister a package
--
-- > hc-pkg unregister [pkgid] [--user | --global | --package-db]
--
unregister :: HcPkgInfo -> Verbosity -> PackageDB -> PackageId -> IO ()
unregister hpi verbosity packagedb pkgid =
  runProgramInvocation verbosity
    (unregisterInvocation hpi verbosity packagedb pkgid)


-- | Call @hc-pkg@ to expose a package.
--
-- > hc-pkg expose [pkgid] [--user | --global | --package-db]
--
expose :: HcPkgInfo -> Verbosity -> PackageDB -> PackageId -> IO ()
expose hpi verbosity packagedb pkgid =
  runProgramInvocation verbosity
    (exposeInvocation hpi verbosity packagedb pkgid)


-- | Call @hc-pkg@ to hide a package.
--
-- > hc-pkg hide [pkgid] [--user | --global | --package-db]
--
hide :: HcPkgInfo -> Verbosity -> PackageDB -> PackageId -> IO ()
hide hpi verbosity packagedb pkgid =
  runProgramInvocation verbosity
    (hideInvocation hpi verbosity packagedb pkgid)


-- | Call @hc-pkg@ to get all the details of all the packages in the given
-- package database.
--
dump :: HcPkgInfo -> Verbosity -> PackageDB -> IO [InstalledPackageInfo]
dump hpi verbosity packagedb = do

  output <- getProgramInvocationOutput verbosity
              (dumpInvocation hpi verbosity packagedb)
    `catchExit` \_ -> die $ programId (hcPkgProgram hpi) ++ " dump failed"

  case parsePackages output of
    Left ok -> return ok
    _       -> die $ "failed to parse output of '"
                  ++ programId (hcPkgProgram hpi) ++ " dump'"

  where
    parsePackages str =
      let parsed = map parseInstalledPackageInfo' (splitPkgs str)
       in case [ msg | ParseFailed msg <- parsed ] of
            []   -> Left [   setInstalledPackageId
                           . maybe id mungePackagePaths (pkgRoot pkg)
                           $ pkg
                         | ParseOk _ pkg <- parsed ]
            msgs -> Right msgs

    parseInstalledPackageInfo' =
      parseFieldsFlat fieldsInstalledPackageInfo emptyInstalledPackageInfo

    --TODO: this could be a lot faster. We're doing normaliseLineEndings twice
    -- and converting back and forth with lines/unlines.
    splitPkgs :: String -> [String]
    splitPkgs = checkEmpty . map unlines . splitWith ("---" ==) . lines
      where
        -- Handle the case of there being no packages at all.
        checkEmpty [s] | all isSpace s = []
        checkEmpty ss                  = ss

        splitWith :: (a -> Bool) -> [a] -> [[a]]
        splitWith p xs = ys : case zs of
                           []   -> []
                           _:ws -> splitWith p ws
          where (ys,zs) = break p xs

mungePackagePaths :: FilePath -> InstalledPackageInfo -> InstalledPackageInfo
-- Perform path/URL variable substitution as per the Cabal ${pkgroot} spec
-- (http://www.haskell.org/pipermail/libraries/2009-May/011772.html)
-- Paths/URLs can be relative to ${pkgroot} or ${pkgrooturl}.
-- The "pkgroot" is the directory containing the package database.
mungePackagePaths pkgroot pkginfo =
    pkginfo {
      importDirs        = mungePaths (importDirs  pkginfo),
      includeDirs       = mungePaths (includeDirs pkginfo),
      libraryDirs       = mungePaths (libraryDirs pkginfo),
      frameworkDirs     = mungePaths (frameworkDirs pkginfo),
      haddockInterfaces = mungePaths (haddockInterfaces pkginfo),
      haddockHTMLs      = mungeUrls  (haddockHTMLs pkginfo)
    }
  where
    mungePaths = map mungePath
    mungeUrls  = map mungeUrl

    mungePath p = case stripVarPrefix "${pkgroot}" p of
      Just p' -> pkgroot </> p'
      Nothing -> p

    mungeUrl p = case stripVarPrefix "${pkgrooturl}" p of
      Just p' -> toUrlPath pkgroot p'
      Nothing -> p

    toUrlPath r p = "file:///"
                 -- URLs always use posix style '/' separators:
                 ++ FilePath.Posix.joinPath (r : FilePath.splitDirectories p)

    stripVarPrefix var p =
      case splitPath p of
        (root:path') -> case stripPrefix var root of
          Just [sep] | isPathSeparator sep -> Just (joinPath path')
          _                                -> Nothing
        _                                  -> Nothing


-- Older installed package info files did not have the installedPackageId
-- field, so if it is missing then we fill it as the source package ID.
setInstalledPackageId :: InstalledPackageInfo -> InstalledPackageInfo
setInstalledPackageId pkginfo@InstalledPackageInfo {
                        installedPackageId = InstalledPackageId "",
                        sourcePackageId    = pkgid
                      }
                    = pkginfo {
                        --TODO use a proper named function for the conversion
                        -- from source package id to installed package id
                        installedPackageId = InstalledPackageId (display pkgid)
                      }
setInstalledPackageId pkginfo = pkginfo


-- | Call @hc-pkg@ to get the source package Id of all the packages in the
-- given package database.
--
-- This is much less information than with 'dump', but also rather quicker.
-- Note in particular that it does not include the 'InstalledPackageId', just
-- the source 'PackageId' which is not necessarily unique in any package db.
--
list :: HcPkgInfo -> Verbosity -> PackageDB
     -> IO [PackageId]
list hpi verbosity packagedb = do

  output <- getProgramInvocationOutput verbosity
              (listInvocation hpi verbosity packagedb)
    `catchExit` \_ -> die $ programId (hcPkgProgram hpi) ++ " list failed"

  case parsePackageIds output of
    Just ok -> return ok
    _       -> die $ "failed to parse output of '"
                  ++ programId (hcPkgProgram hpi) ++ " list'"

  where
    parsePackageIds = sequence . map simpleParse . words

--------------------------
-- The program invocations
--

initInvocation :: HcPkgInfo -> Verbosity -> FilePath -> ProgramInvocation
initInvocation hpi verbosity path =
    programInvocation (hcPkgProgram hpi) args
  where
    args = ["init", path]
        ++ verbosityOpts hpi verbosity

registerInvocation, reregisterInvocation
  :: HcPkgInfo -> Verbosity -> PackageDBStack
  -> Either FilePath InstalledPackageInfo
  -> ProgramInvocation
registerInvocation   = registerInvocation' "register"
reregisterInvocation = registerInvocation' "update"


registerInvocation' :: String -> HcPkgInfo -> Verbosity -> PackageDBStack
                    -> Either FilePath InstalledPackageInfo
                    -> ProgramInvocation
registerInvocation' cmdname hpi verbosity packagedbs (Left pkgFile) =
    programInvocation (hcPkgProgram hpi) args
  where
    args' = [cmdname, pkgFile]
        ++ (if noPkgDbStack hpi
              then [packageDbOpts hpi (last packagedbs)]
              else packageDbStackOpts hpi packagedbs)
        ++ verbosityOpts hpi verbosity
    args = (if multInstEnabled hpi
              then args' ++ ["--enable-multi-instance"]
              else args')

registerInvocation' cmdname hpi verbosity packagedbs (Right pkgInfo) =
    (programInvocation (hcPkgProgram hpi) args) {
      progInvokeInput         = Just (showInstalledPackageInfo pkgInfo),
      progInvokeInputEncoding = IOEncodingUTF8
    }
  where
    args' = [cmdname, "-"]
        ++ (if noPkgDbStack hpi
              then [packageDbOpts hpi (last packagedbs)]
              else packageDbStackOpts hpi packagedbs)
        ++ verbosityOpts hpi verbosity
    args = (if multInstEnabled hpi
              then args' ++ ["--enable-multi-instance"]
              else args')

unregisterInvocation :: HcPkgInfo -> Verbosity -> PackageDB -> PackageId
                     -> ProgramInvocation
unregisterInvocation hpi verbosity packagedb pkgid =
  programInvocation (hcPkgProgram hpi) $
       ["unregister", packageDbOpts hpi packagedb, display pkgid]
    ++ verbosityOpts hpi verbosity


exposeInvocation :: HcPkgInfo -> Verbosity -> PackageDB -> PackageId
                 -> ProgramInvocation
exposeInvocation hpi verbosity packagedb pkgid =
  programInvocation (hcPkgProgram hpi) $
       ["expose", packageDbOpts hpi packagedb, display pkgid]
    ++ verbosityOpts hpi verbosity


hideInvocation :: HcPkgInfo -> Verbosity -> PackageDB -> PackageId
               -> ProgramInvocation
hideInvocation hpi verbosity packagedb pkgid =
  programInvocation (hcPkgProgram hpi) $
       ["hide", packageDbOpts hpi packagedb, display pkgid]
    ++ verbosityOpts hpi verbosity


dumpInvocation :: HcPkgInfo -> Verbosity -> PackageDB -> ProgramInvocation
dumpInvocation hpi _verbosity packagedb =
    (programInvocation (hcPkgProgram hpi) args) {
      progInvokeOutputEncoding = IOEncodingUTF8
    }
  where
    args = ["dump", packageDbOpts hpi packagedb]
        ++ verbosityOpts hpi silent
           -- We use verbosity level 'silent' because it is important that we
           -- do not contaminate the output with info/debug messages.

listInvocation :: HcPkgInfo -> Verbosity -> PackageDB -> ProgramInvocation
listInvocation hpi _verbosity packagedb =
    (programInvocation (hcPkgProgram hpi) args) {
      progInvokeOutputEncoding = IOEncodingUTF8
    }
  where
    args = ["list", "--simple-output", packageDbOpts hpi packagedb]
        ++ verbosityOpts hpi silent
           -- We use verbosity level 'silent' because it is important that we
           -- do not contaminate the output with info/debug messages.


packageDbStackOpts :: HcPkgInfo -> PackageDBStack -> [String]
packageDbStackOpts hpi dbstack = case dbstack of
  (GlobalPackageDB:UserPackageDB:dbs) -> "--global"
                                       : "--user"
                                       : map specific dbs
  (GlobalPackageDB:dbs)               -> "--global"
                                       : ("--no-user-" ++ packageDbFlag hpi)
                                       : map specific dbs
  _                                   -> ierror
  where
    specific (SpecificPackageDB db) = "--" ++ packageDbFlag hpi ++ "=" ++ db
    specific _ = ierror
    ierror :: a
    ierror     = error ("internal error: unexpected package db stack: " ++ show dbstack)

packageDbFlag :: HcPkgInfo -> String
packageDbFlag hpi
  | flagPackageConf hpi
  = "package-conf"
  | otherwise
  = "package-db"

packageDbOpts :: HcPkgInfo -> PackageDB -> String
packageDbOpts _ GlobalPackageDB        = "--global"
packageDbOpts _ UserPackageDB          = "--user"
packageDbOpts hpi (SpecificPackageDB db) = "--" ++ packageDbFlag hpi ++ "=" ++ db

verbosityOpts :: HcPkgInfo -> Verbosity -> [String]
verbosityOpts hpi v
  | noVerboseFlag hpi
                   = []
  | v >= deafening = ["-v2"]
  | v == silent    = ["-v0"]
  | otherwise      = []

