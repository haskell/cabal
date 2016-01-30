-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.NHC
-- Copyright   :  Isaac Jones 2003-2006
--                Duncan Coutts 2009
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This module contains most of the NHC-specific code for configuring, building
-- and installing packages.

module Distribution.Simple.NHC (
    configure,
    getInstalledPackages,
    buildLib,
    buildExe,
    installLib,
    installExe,
  ) where

import Distribution.Package
         ( PackageName, PackageIdentifier(..), InstalledPackageId(..)
         , packageName )
import Distribution.InstalledPackageInfo
         ( InstalledPackageInfo
         , InstalledPackageInfo_( InstalledPackageInfo, installedPackageId
                                , sourcePackageId )
         , emptyInstalledPackageInfo, parseInstalledPackageInfo )
import Distribution.PackageDescription
        ( PackageDescription(..), BuildInfo(..), Library(..), Executable(..)
        , hcOptions, usedExtensions )
import Distribution.ModuleName (ModuleName)
import qualified Distribution.ModuleName as ModuleName
import Distribution.Simple.LocalBuildInfo
        ( LocalBuildInfo(..), ComponentLocalBuildInfo(..) )
import Distribution.Simple.BuildPaths
        ( mkLibName, objExtension, exeExtension )
import Distribution.Simple.Compiler
         ( CompilerFlavor(..), CompilerId(..), Compiler(..)
         , Flag, languageToFlags, extensionsToFlags
         , PackageDB(..), PackageDBStack )
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.Simple.PackageIndex (PackageIndex)
import Language.Haskell.Extension
         ( Language(Haskell98), Extension(..), KnownExtension(..) )
import Distribution.Simple.Program
         ( ProgramConfiguration, userMaybeSpecifyPath, programPath
         , requireProgram, requireProgramVersion, lookupProgram
         , nhcProgram, hmakeProgram, ldProgram, arProgram
         , rawSystemProgramConf )
import Distribution.Simple.Utils
        ( die, info, findFileWithExtension, findModuleFiles
        , installOrdinaryFile, installExecutableFile, installOrdinaryFiles
        , createDirectoryIfMissingVerbose, withUTF8FileContents )
import Distribution.Version
        ( Version(..), orLaterVersion )
import Distribution.Verbosity
import Distribution.Text
         ( display, simpleParse )
import Distribution.ParseUtils
         ( ParseResult(..) )

import System.FilePath
        ( (</>), (<.>), normalise, takeDirectory, dropExtension )
import System.Directory
         ( doesFileExist, doesDirectoryExist, getDirectoryContents
         , removeFile, getHomeDirectory )

import Data.Char               ( toLower )
import Data.List               ( nub )
import Data.Maybe              ( catMaybes )
import qualified Data.Map as M ( empty )
import Data.Monoid             ( Monoid(..) )
import Control.Monad           ( when, unless )
import Distribution.Compat.Exception
import Distribution.System ( Platform )

-- -----------------------------------------------------------------------------
-- Configuring

configure :: Verbosity -> Maybe FilePath -> Maybe FilePath
          -> ProgramConfiguration -> IO (Compiler, Maybe Platform, ProgramConfiguration)
configure verbosity hcPath _hcPkgPath conf = do

  (_nhcProg, nhcVersion, conf') <-
    requireProgramVersion verbosity nhcProgram
      (orLaterVersion (Version [1,20] []))
      (userMaybeSpecifyPath "nhc98" hcPath conf)

  (_hmakeProg, _hmakeVersion, conf'') <-
    requireProgramVersion verbosity hmakeProgram
     (orLaterVersion (Version [3,13] [])) conf'
  (_ldProg, conf''')   <- requireProgram verbosity ldProgram conf''
  (_arProg, conf'''')  <- requireProgram verbosity arProgram conf'''

  --TODO: put this stuff in a monad so we can say just:
  -- requireProgram hmakeProgram (orLaterVersion (Version [3,13] []))
  -- requireProgram ldProgram anyVersion
  -- requireProgram ldPrograrProgramam anyVersion
  -- unless (null (cSources bi)) $ requireProgram ccProgram anyVersion

  let comp = Compiler {
        compilerId         = CompilerId NHC nhcVersion,
        compilerLanguages  = nhcLanguages,
        compilerExtensions = nhcLanguageExtensions,
        compilerProperties = M.empty
      }
      compPlatform = Nothing
  return (comp, compPlatform,  conf'''')

nhcLanguages :: [(Language, Flag)]
nhcLanguages = [(Haskell98, "-98")]

-- | The flags for the supported extensions
nhcLanguageExtensions :: [(Extension, Flag)]
nhcLanguageExtensions =
    -- TODO: pattern guards in 1.20
     -- NHC doesn't enforce the monomorphism restriction at all.
     -- Technically it therefore doesn't support MonomorphismRestriction,
     -- but that would mean it doesn't support Haskell98, so we pretend
     -- that it does.
    [(EnableExtension  MonomorphismRestriction,   "")
    ,(DisableExtension MonomorphismRestriction,   "")
     -- Similarly, I assume the FFI is always on
    ,(EnableExtension  ForeignFunctionInterface,  "")
    ,(DisableExtension ForeignFunctionInterface,  "")
     -- Similarly, I assume existential quantification is always on
    ,(EnableExtension  ExistentialQuantification, "")
    ,(DisableExtension ExistentialQuantification, "")
     -- Similarly, I assume empty data decls is always on
    ,(EnableExtension  EmptyDataDecls,            "")
    ,(DisableExtension EmptyDataDecls,            "")
    ,(EnableExtension  NamedFieldPuns,            "-puns")
    ,(DisableExtension NamedFieldPuns,            "-nopuns")
     -- CPP can't actually be turned off, but we pretend that it can
    ,(EnableExtension  CPP,                       "-cpp")
    ,(DisableExtension CPP,                       "")
    ]

getInstalledPackages :: Verbosity -> PackageDBStack -> ProgramConfiguration
                     -> IO PackageIndex
getInstalledPackages verbosity packagedbs conf = do
  homedir      <- getHomeDirectory
  (nhcProg, _) <- requireProgram verbosity nhcProgram conf
  let bindir = takeDirectory (programPath nhcProg)
      incdir = takeDirectory bindir </> "include" </> "nhc98"
      dbdirs = nub (concatMap (packageDbPaths homedir incdir) packagedbs)
  indexes  <- mapM getIndividualDBPackages dbdirs
  return $! mconcat indexes

  where
    getIndividualDBPackages :: FilePath -> IO PackageIndex
    getIndividualDBPackages dbdir = do
      pkgdirs <- getPackageDbDirs dbdir
      pkgs    <- sequence [ getInstalledPackage pkgname pkgdir
                          | (pkgname, pkgdir) <- pkgdirs ]
      let pkgs' = map setInstalledPackageId (catMaybes pkgs)
      return (PackageIndex.fromList pkgs')

packageDbPaths :: FilePath -> FilePath -> PackageDB -> [FilePath]
packageDbPaths _home incdir db = case db of
  GlobalPackageDB        -> [ incdir </> "packages" ]
  UserPackageDB          -> [] --TODO any standard per-user db?
  SpecificPackageDB path -> [ path ]

getPackageDbDirs :: FilePath -> IO [(PackageName, FilePath)]
getPackageDbDirs dbdir = do
  dbexists <- doesDirectoryExist dbdir
  if not dbexists
    then return []
    else do
      entries  <- getDirectoryContents dbdir
      pkgdirs  <- sequence
        [ do pkgdirExists <- doesDirectoryExist pkgdir
             return (pkgname, pkgdir, pkgdirExists)
        | (entry, Just pkgname) <- [ (entry, simpleParse entry)
                                   | entry <- entries ]
        , let pkgdir = dbdir </> entry ]
      return [ (pkgname, pkgdir) | (pkgname, pkgdir, True) <- pkgdirs ]

getInstalledPackage :: PackageName -> FilePath -> IO (Maybe InstalledPackageInfo)
getInstalledPackage pkgname pkgdir = do
  let pkgconfFile = pkgdir </> "package.conf"
  pkgconfExists <- doesFileExist pkgconfFile

  let cabalFile = pkgdir <.> "cabal"
  cabalExists <- doesFileExist cabalFile

  case () of
    _ | pkgconfExists -> getFullInstalledPackageInfo pkgname pkgconfFile
      | cabalExists   -> getPhonyInstalledPackageInfo pkgname cabalFile
      | otherwise     -> return Nothing

getFullInstalledPackageInfo :: PackageName -> FilePath
                            -> IO (Maybe InstalledPackageInfo)
getFullInstalledPackageInfo pkgname pkgconfFile =
  withUTF8FileContents pkgconfFile $ \contents ->
    case parseInstalledPackageInfo contents of
      ParseOk _ pkginfo | packageName pkginfo == pkgname
                        -> return (Just pkginfo)
      _                 -> return Nothing

-- | This is a backup option for existing versions of nhc98 which do not supply
-- proper installed package info files for the bundled libs. Instead we look
-- for the .cabal file and extract the package version from that.
-- We don't know any other details for such packages, in particular we pretend
-- that they have no dependencies.
--
getPhonyInstalledPackageInfo :: PackageName -> FilePath
                             -> IO (Maybe InstalledPackageInfo)
getPhonyInstalledPackageInfo pkgname pathsModule = do
  content <- readFile pathsModule
  case extractVersion content of
    Nothing      -> return Nothing
    Just version -> return (Just pkginfo)
      where
        pkgid   = PackageIdentifier pkgname version
        pkginfo = emptyInstalledPackageInfo { sourcePackageId = pkgid }
  where
    -- search through the .cabal file, looking for a line like:
    --
    -- > version: 2.0
    --
    extractVersion :: String -> Maybe Version
    extractVersion content =
      case catMaybes (map extractVersionLine (lines content)) of
        [version] -> Just version
        _         -> Nothing
    extractVersionLine :: String -> Maybe Version
    extractVersionLine line =
      case words line of
        [versionTag, ":", versionStr]
          | map toLower versionTag == "version"  -> simpleParse versionStr
        [versionTag,      versionStr]
          | map toLower versionTag == "version:" -> simpleParse versionStr
        _                                        -> Nothing

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

-- -----------------------------------------------------------------------------
-- Building

-- |FIX: For now, the target must contain a main module.  Not used
-- ATM. Re-add later.
buildLib :: Verbosity -> PackageDescription -> LocalBuildInfo
                      -> Library            -> ComponentLocalBuildInfo -> IO ()
buildLib verbosity pkg_descr lbi lib clbi = do
  libName <- case componentLibraries clbi of
             [libName] -> return libName
             [] -> die "No library name found when building library"
             _  -> die "Multiple library names found when building library"
  let conf = withPrograms lbi
      Just nhcProg = lookupProgram nhcProgram conf
  let bi = libBuildInfo lib
      modules = exposedModules lib ++ otherModules bi
      -- Unsupported extensions have already been checked by configure
      languageFlags = languageToFlags (compiler lbi) (defaultLanguage bi)
                   ++ extensionsToFlags (compiler lbi) (usedExtensions bi)
  inFiles <- getModulePaths lbi bi modules
  let targetDir = buildDir lbi
      srcDirs  = nub (map takeDirectory inFiles)
      destDirs = map (targetDir </>) srcDirs
  mapM_ (createDirectoryIfMissingVerbose verbosity True) destDirs
  rawSystemProgramConf verbosity hmakeProgram conf $
       ["-hc=" ++ programPath nhcProg]
    ++ nhcVerbosityOptions verbosity
    ++ ["-d", targetDir, "-hidir", targetDir]
    ++ maybe [] (hcOptions NHC . libBuildInfo)
                           (library pkg_descr)
    ++ languageFlags
    ++ concat [ ["-package", display (packageName pkgid) ]
              | (_, pkgid) <- componentPackageDeps clbi ]
    ++ inFiles
{-
  -- build any C sources
  unless (null (cSources bi)) $ do
     info verbosity "Building C Sources..."
     let commonCcArgs = (if verbosity >= deafening then ["-v"] else [])
                     ++ ["-I" ++ dir | dir <- includeDirs bi]
                     ++ [opt | opt <- ccOptions bi]
                     ++ (if withOptimization lbi then ["-O2"] else [])
     flip mapM_ (cSources bi) $ \cfile -> do
       let ofile = targetDir </> cfile `replaceExtension` objExtension
       createDirectoryIfMissingVerbose verbosity True (takeDirectory ofile)
       rawSystemProgramConf verbosity hmakeProgram conf
         (commonCcArgs ++ ["-c", cfile, "-o", ofile])
-}
  -- link:
  info verbosity "Linking..."
  let --cObjs = [ targetDir </> cFile `replaceExtension` objExtension
      --        | cFile <- cSources bi ]
      libFilePath = targetDir </> mkLibName libName
      hObjs = [ targetDir </> ModuleName.toFilePath m <.> objExtension
              | m <- modules ]

  unless (null hObjs {-&& null cObjs-}) $ do
    -- first remove library if it exists
    removeFile libFilePath `catchIO` \_ -> return ()

    let arVerbosity | verbosity >= deafening = "v"
                    | verbosity >= normal = ""
                    | otherwise = "c"

    rawSystemProgramConf verbosity arProgram (withPrograms lbi) $
         ["q"++ arVerbosity, libFilePath]
      ++ hObjs
--    ++ cObjs

-- | Building an executable for NHC.
buildExe :: Verbosity -> PackageDescription -> LocalBuildInfo
                      -> Executable         -> ComponentLocalBuildInfo -> IO ()
buildExe verbosity pkg_descr lbi exe clbi = do
  let conf = withPrograms lbi
      Just nhcProg = lookupProgram nhcProgram conf
  when (dropExtension (modulePath exe) /= exeName exe) $
    die $ "hmake does not support exe names that do not match the name of "
       ++ "the 'main-is' file. You will have to rename your executable to "
       ++ show (dropExtension (modulePath exe))
  let bi = buildInfo exe
      modules = otherModules bi
      -- Unsupported extensions have already been checked by configure
      languageFlags = languageToFlags (compiler lbi) (defaultLanguage bi)
                   ++ extensionsToFlags (compiler lbi) (usedExtensions bi)
  inFiles <- getModulePaths lbi bi modules
  let targetDir = buildDir lbi </> exeName exe
      exeDir    = targetDir </> (exeName exe ++ "-tmp")
      srcDirs   = nub (map takeDirectory (modulePath exe : inFiles))
      destDirs  = map (exeDir </>) srcDirs
  mapM_ (createDirectoryIfMissingVerbose verbosity True) destDirs
  rawSystemProgramConf verbosity hmakeProgram conf $
       ["-hc=" ++ programPath nhcProg]
    ++ nhcVerbosityOptions verbosity
    ++ ["-d", targetDir, "-hidir", targetDir]
    ++ maybe [] (hcOptions NHC . libBuildInfo)
                           (library pkg_descr)
    ++ languageFlags
    ++ concat [ ["-package", display (packageName pkgid) ]
              | (_, pkgid) <- componentPackageDeps clbi ]
    ++ inFiles
    ++ [exeName exe]

nhcVerbosityOptions :: Verbosity -> [String]
nhcVerbosityOptions verbosity
     | verbosity >= deafening = ["-v"]
     | verbosity >= normal    = []
     | otherwise              = ["-q"]

--TODO: where to put this? it's duplicated in .Simple too
getModulePaths :: LocalBuildInfo -> BuildInfo -> [ModuleName] -> IO [FilePath]
getModulePaths lbi bi modules = sequence
   [ findFileWithExtension ["hs", "lhs"] (buildDir lbi : hsSourceDirs bi)
       (ModuleName.toFilePath module_) >>= maybe (notFound module_) (return . normalise)
   | module_ <- modules ]
   where notFound module_ = die $ "can't find source for module " ++ display module_

-- -----------------------------------------------------------------------------
-- Installing

-- |Install executables for NHC.
installExe :: Verbosity -- ^verbosity
           -> FilePath  -- ^install location
           -> FilePath  -- ^Build location
           -> (FilePath, FilePath)  -- ^Executable (prefix,suffix)
           -> Executable
           -> IO ()
installExe verbosity pref buildPref (progprefix,progsuffix) exe
    = do createDirectoryIfMissingVerbose verbosity True pref
         let exeBaseName = exeName exe
             exeFileName = exeBaseName <.> exeExtension
             fixedExeFileName = (progprefix ++ exeBaseName ++ progsuffix) <.> exeExtension
         installExecutableFile verbosity
           (buildPref </> exeBaseName </> exeFileName)
           (pref </> fixedExeFileName)

-- |Install for nhc98: .hi and .a files
installLib    :: Verbosity -- ^verbosity
              -> FilePath  -- ^install location
              -> FilePath  -- ^Build location
              -> PackageIdentifier
              -> Library
              -> ComponentLocalBuildInfo
              -> IO ()
installLib verbosity pref buildPref _pkgid lib clbi
    = do let bi = libBuildInfo lib
             modules = exposedModules lib ++ otherModules bi
         findModuleFiles [buildPref] ["hi"] modules
           >>= installOrdinaryFiles verbosity pref
         let libNames = map mkLibName (componentLibraries clbi)
             installLib' libName = installOrdinaryFile verbosity
                                                       (buildPref </> libName)
                                                       (pref </> libName)
         mapM_ installLib' libNames
