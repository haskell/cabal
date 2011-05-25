-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Program.HcPkg
-- Copyright   :  Duncan Coutts 2009
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This module provides an library interface to the @hc-pkg@ program.
-- Currently only GHC and LHC have hc-pkg programs.

module Distribution.Simple.Program.HcPkg (
    register,
    reregister,
    unregister,
    expose,
    hide,
    dump,

    -- * Program invocations
    registerInvocation,
    reregisterInvocation,
    unregisterInvocation,
    exposeInvocation,
    hideInvocation,
    dumpInvocation,
  ) where

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
         ( ConfiguredProgram(programId, programVersion) )
import Distribution.Simple.Program.Run
         ( ProgramInvocation(..), IOEncoding(..), programInvocation
         , runProgramInvocation, getProgramInvocationOutput )
import Distribution.Version
         ( Version(..) )
import Distribution.Text
         ( display )
import Distribution.Simple.Utils
         ( die )
import Distribution.Verbosity
         ( Verbosity, deafening, silent )
import Distribution.Compat.Exception
         ( catchExit )

import Data.Char
         ( isSpace )
import Data.Maybe
         ( fromMaybe )
import Data.List
         ( stripPrefix )
import System.FilePath as FilePath
         ( (</>), splitPath, splitDirectories, joinPath, isPathSeparator )
import qualified System.FilePath.Posix as FilePath.Posix


-- | Call @hc-pkg@ to register a package.
--
-- > hc-pkg register {filename | -} [--user | --global | --package-conf]
--
register :: Verbosity -> ConfiguredProgram -> PackageDBStack
         -> Either FilePath
                   InstalledPackageInfo
         -> IO ()
register verbosity hcPkg packagedb pkgFile =
  runProgramInvocation verbosity
    (registerInvocation hcPkg verbosity packagedb pkgFile)


-- | Call @hc-pkg@ to re-register a package.
--
-- > hc-pkg register {filename | -} [--user | --global | --package-conf]
--
reregister :: Verbosity -> ConfiguredProgram -> PackageDBStack
           -> Either FilePath
                     InstalledPackageInfo
           -> IO ()
reregister verbosity hcPkg packagedb pkgFile =
  runProgramInvocation verbosity
    (reregisterInvocation hcPkg verbosity packagedb pkgFile)


-- | Call @hc-pkg@ to unregister a package
--
-- > hc-pkg unregister [pkgid] [--user | --global | --package-conf]
--
unregister :: Verbosity -> ConfiguredProgram -> PackageDB -> PackageId -> IO ()
unregister verbosity hcPkg packagedb pkgid =
  runProgramInvocation verbosity
    (unregisterInvocation hcPkg verbosity packagedb pkgid)


-- | Call @hc-pkg@ to expose a package.
--
-- > hc-pkg expose [pkgid] [--user | --global | --package-conf]
--
expose :: Verbosity -> ConfiguredProgram -> PackageDB -> PackageId -> IO ()
expose verbosity hcPkg packagedb pkgid =
  runProgramInvocation verbosity
    (exposeInvocation hcPkg verbosity packagedb pkgid)


-- | Call @hc-pkg@ to expose a package.
--
-- > hc-pkg expose [pkgid] [--user | --global | --package-conf]
--
hide :: Verbosity -> ConfiguredProgram -> PackageDB -> PackageId -> IO ()
hide verbosity hcPkg packagedb pkgid =
  runProgramInvocation verbosity
    (hideInvocation hcPkg verbosity packagedb pkgid)


-- | Call @hc-pkg@ to get all the installed packages.
--
dump :: Verbosity -> ConfiguredProgram -> PackageDB -> IO [InstalledPackageInfo]
dump verbosity hcPkg packagedb = do

  output <- getProgramInvocationOutput verbosity
              (dumpInvocation hcPkg verbosity packagedb)
    `catchExit` \_ -> die $ programId hcPkg ++ " dump failed"

  case parsePackages output of
    Left ok -> return ok
    _       -> die $ "failed to parse output of '"
                  ++ programId hcPkg ++ " dump'"

  where
    parsePackages str =
      let parsed = map parseInstalledPackageInfo' (splitPkgs str)
       in case [ msg | ParseFailed msg <- parsed ] of
            []   -> Left [   setInstalledPackageId
                           . maybe id mungePackagePaths pkgroot
                           $ pkg
                         | ParseOk _ (pkgroot, pkg) <- parsed ]
            msgs -> Right msgs

    parseInstalledPackageInfo' =
        parseFieldsFlat fields (Nothing, emptyInstalledPackageInfo)
      where
        fields =     liftFieldFst pkgrootField
               : map liftFieldSnd fieldsInstalledPackageInfo

        pkgrootField =
          simpleField "pkgroot"
            showFilePath    parseFilePathQ
            (fromMaybe "")  (\x _ -> Just x)

        liftFieldFst = liftField fst (\x (_x,y) -> (x,y))
        liftFieldSnd = liftField snd (\y (x,_y) -> (x,y))

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


--------------------------
-- The program invocations
--

registerInvocation, reregisterInvocation
  :: ConfiguredProgram -> Verbosity -> PackageDBStack
  -> Either FilePath InstalledPackageInfo
  -> ProgramInvocation
registerInvocation   = registerInvocation' "register"
reregisterInvocation = registerInvocation' "update"


registerInvocation' :: String
                    -> ConfiguredProgram -> Verbosity -> PackageDBStack
                    -> Either FilePath InstalledPackageInfo
                    -> ProgramInvocation
registerInvocation' cmdname hcPkg verbosity packagedbs (Left pkgFile) =
    programInvocation hcPkg args
  where
    args = [cmdname, pkgFile]
        ++ (if legacyVersion hcPkg
              then [packageDbOpts (last packagedbs)]
              else packageDbStackOpts packagedbs)
        ++ verbosityOpts hcPkg verbosity

registerInvocation' cmdname hcPkg verbosity packagedbs (Right pkgInfo) =
    (programInvocation hcPkg args) {
      progInvokeInput         = Just (showInstalledPackageInfo pkgInfo),
      progInvokeInputEncoding = IOEncodingUTF8
    }
  where
    args = [cmdname, "-"]
        ++ (if legacyVersion hcPkg
              then [packageDbOpts (last packagedbs)]
              else packageDbStackOpts packagedbs)
        ++ verbosityOpts hcPkg verbosity


unregisterInvocation :: ConfiguredProgram
                     -> Verbosity -> PackageDB -> PackageId
                     -> ProgramInvocation
unregisterInvocation hcPkg verbosity packagedb pkgid =
  programInvocation hcPkg $
       ["unregister", packageDbOpts packagedb, display pkgid]
    ++ verbosityOpts hcPkg verbosity


exposeInvocation :: ConfiguredProgram
                 -> Verbosity -> PackageDB -> PackageId -> ProgramInvocation
exposeInvocation hcPkg verbosity packagedb pkgid =
  programInvocation hcPkg $
       ["expose", packageDbOpts packagedb, display pkgid]
    ++ verbosityOpts hcPkg verbosity


hideInvocation :: ConfiguredProgram
               -> Verbosity -> PackageDB -> PackageId -> ProgramInvocation
hideInvocation hcPkg verbosity packagedb pkgid =
  programInvocation hcPkg $
       ["hide", packageDbOpts packagedb, display pkgid]
    ++ verbosityOpts hcPkg verbosity


dumpInvocation :: ConfiguredProgram
               -> Verbosity -> PackageDB -> ProgramInvocation
dumpInvocation hcPkg _verbosity packagedb =
    (programInvocation hcPkg args) {
      progInvokeOutputEncoding = IOEncodingUTF8
    }
  where
    args = ["dump", packageDbOpts packagedb]
        ++ verbosityOpts hcPkg silent
           -- We use verbosity level 'silent' because it is important that we
           -- do not contaminate the output with info/debug messages.


packageDbStackOpts :: PackageDBStack -> [String]
packageDbStackOpts dbstack = case dbstack of
  (GlobalPackageDB:UserPackageDB:dbs) -> "--global"
                                       : "--user"
                                       : map specific dbs
  (GlobalPackageDB:dbs)               -> "--global"
                                       : "--no-user-package-conf"
                                       : map specific dbs
  _                                   -> ierror
  where
    specific (SpecificPackageDB db) = "--package-conf=" ++ db
    specific _ = ierror
    ierror :: a
    ierror     = error ("internal error: unexpected package db stack: " ++ show dbstack)

packageDbOpts :: PackageDB -> String
packageDbOpts GlobalPackageDB        = "--global"
packageDbOpts UserPackageDB          = "--user"
packageDbOpts (SpecificPackageDB db) = "--package-conf=" ++ db

verbosityOpts :: ConfiguredProgram -> Verbosity -> [String]
verbosityOpts hcPkg v

  -- ghc-pkg < 6.11 does not support -v
  | programId hcPkg == "ghc-pkg"
 && programVersion hcPkg < Just (Version [6,11] [])
                   = []

  | v >= deafening = ["-v2"]
  | v == silent    = ["-v0"]
  | otherwise      = []

-- Handle quirks in ghc-pkg 6.8 and older
legacyVersion :: ConfiguredProgram -> Bool
legacyVersion hcPkg = programId hcPkg == "ghc-pkg"
                   && programVersion hcPkg < Just (Version [6,9] [])
