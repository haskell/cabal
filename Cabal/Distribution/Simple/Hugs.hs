-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Hugs
-- Copyright   :  Isaac Jones 2003-2006
--                Duncan Coutts 2009
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This module contains most of the NHC-specific code for configuring, building
-- and installing packages.

module Distribution.Simple.Hugs (
    configure,
    getInstalledPackages,
    buildLib,
    buildExe,
    install,
    registerPackage,
  ) where

import Distribution.Package
         ( PackageName, PackageIdentifier(..), InstalledPackageId(..)
         , packageName )
import Distribution.InstalledPackageInfo
         ( InstalledPackageInfo, emptyInstalledPackageInfo
         , InstalledPackageInfo_( InstalledPackageInfo, installedPackageId
                                , sourcePackageId )
         , parseInstalledPackageInfo, showInstalledPackageInfo )
import Distribution.PackageDescription
         ( PackageDescription(..), BuildInfo(..), hcOptions, allExtensions
         , Executable(..), withExe, Library(..), withLib, libModules )
import Distribution.ModuleName (ModuleName)
import qualified Distribution.ModuleName as ModuleName
import Distribution.Simple.Compiler
         ( CompilerFlavor(..), CompilerId(..)
         , Compiler(..), Flag, languageToFlags, extensionsToFlags
         , PackageDB(..), PackageDBStack )
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.Simple.PackageIndex (PackageIndex)
import Distribution.Simple.Program
         ( Program(programFindVersion)
         , ProgramConfiguration, userMaybeSpecifyPath
         , requireProgram, requireProgramVersion
         , rawSystemProgramConf, programPath
         , ffihugsProgram, hugsProgram )
import Distribution.Version
         ( Version(..), orLaterVersion )
import Distribution.Simple.PreProcess   ( ppCpp, runSimplePreProcessor )
import Distribution.Simple.PreProcess.Unlit
                                ( unlit )
import Distribution.Simple.LocalBuildInfo
         ( LocalBuildInfo(..), ComponentLocalBuildInfo(..)
         , InstallDirs(..), absoluteInstallDirs )
import Distribution.Simple.BuildPaths
                                ( autogenModuleName, autogenModulesDir,
                                  dllExtension )
import Distribution.Simple.Setup
         ( CopyDest(..) )
import Distribution.Simple.Utils
         ( createDirectoryIfMissingVerbose
         , installOrdinaryFiles, setFileExecutable
         , withUTF8FileContents, writeFileAtomic, writeUTF8File
         , copyFileVerbose, findFile, findFileWithExtension, findModuleFiles
         , rawSystemStdInOut
         , die, info, notice )
import Language.Haskell.Extension
         ( Language(Haskell98), Extension(..), KnownExtension(..) )
import System.FilePath          ( (</>), takeExtension, (<.>),
                                  searchPathSeparator, normalise, takeDirectory )
import Distribution.System
         ( OS(..), buildOS )
import Distribution.Text
         ( display, simpleParse )
import Distribution.ParseUtils
         ( ParseResult(..) )
import Distribution.Verbosity

import Data.Char                ( isSpace )
import qualified Data.Map as M  ( empty )
import Data.Maybe               ( mapMaybe, catMaybes )
import Data.Monoid              ( Monoid(..) )
import Control.Monad            ( unless, when, filterM )
import Data.List                ( nub, sort, isSuffixOf )
import System.Directory
         ( doesFileExist, doesDirectoryExist, getDirectoryContents
         , removeDirectoryRecursive, getHomeDirectory )
import System.Exit
         ( ExitCode(ExitSuccess) )
import Distribution.Compat.Exception
import Distribution.System ( Platform )

import qualified Data.ByteString.Lazy.Char8 as BS.Char8

-- -----------------------------------------------------------------------------
-- Configuring

configure :: Verbosity -> Maybe FilePath -> Maybe FilePath
          -> ProgramConfiguration -> IO (Compiler, Maybe Platform, ProgramConfiguration)
configure verbosity hcPath _hcPkgPath conf = do

  (_ffihugsProg, conf') <- requireProgram verbosity ffihugsProgram
                            (userMaybeSpecifyPath "ffihugs" hcPath conf)
  (_hugsProg, version, conf'')
                        <- requireProgramVersion verbosity hugsProgram'
                            (orLaterVersion (Version [2006] [])) conf'

  let comp = Compiler {
        compilerId             = CompilerId Hugs version,
        compilerLanguages      = hugsLanguages,
        compilerExtensions     = hugsLanguageExtensions,
        compilerProperties     = M.empty
      }
      compPlatform = Nothing
  return (comp, compPlatform, conf'')

  where
    hugsProgram' = hugsProgram { programFindVersion = getVersion }

getVersion :: Verbosity -> FilePath -> IO (Maybe Version)
getVersion verbosity hugsPath = do
  (output, _err, exit) <- rawSystemStdInOut verbosity hugsPath []
                              Nothing Nothing
                              (Just (":quit", False)) False
  if exit == ExitSuccess
    then return $! findVersion output
    else return Nothing

  where
    findVersion output = do
      (monthStr, yearStr) <- selectWords output
      year  <- convertYear yearStr
      month <- convertMonth monthStr
      return (Version [year, month] [])

    selectWords output =
      case [ (month, year)
           | [_,_,"Version:", month, year,_] <- map words (lines output) ] of
        [(month, year)] -> Just (month, year)
        _               -> Nothing
    convertYear year = case reads year of
      [(y, [])] | y >= 1999 && y < 2020 -> Just y
      _                                 -> Nothing
    convertMonth month = lookup month (zip months [1..])
    months = [ "January", "February", "March", "April", "May", "June", "July"
             , "August", "September", "October", "November", "December" ]

hugsLanguages :: [(Language, Flag)]
hugsLanguages = [(Haskell98, "")] --default is 98 mode

-- | The flags for the supported extensions
hugsLanguageExtensions :: [(Extension, Flag)]
hugsLanguageExtensions =
    let doFlag (f, (enable, disable)) = [(EnableExtension  f, enable),
                                         (DisableExtension f, disable)]
        alwaysOn = ("", ""{- wrong -})
        ext98 = ("-98", ""{- wrong -})
    in concatMap doFlag
    [(OverlappingInstances       , ("+o",  "-o"))
    ,(IncoherentInstances        , ("+oO", "-O"))
    ,(HereDocuments              , ("+H",  "-H"))
    ,(TypeSynonymInstances       , ext98)
    ,(RecursiveDo                , ext98)
    ,(ParallelListComp           , ext98)
    ,(MultiParamTypeClasses      , ext98)
    ,(FunctionalDependencies     , ext98)
    ,(Rank2Types                 , ext98)
    ,(PolymorphicComponents      , ext98)
    ,(ExistentialQuantification  , ext98)
    ,(ScopedTypeVariables        , ext98)
    ,(ImplicitParams             , ext98)
    ,(ExtensibleRecords          , ext98)
    ,(RestrictedTypeSynonyms     , ext98)
    ,(FlexibleContexts           , ext98)
    ,(FlexibleInstances          , ext98)
    ,(ForeignFunctionInterface   , alwaysOn)
    ,(EmptyDataDecls             , alwaysOn)
    ,(CPP                        , alwaysOn)
    ]

getInstalledPackages :: Verbosity -> PackageDBStack -> ProgramConfiguration
                     -> IO PackageIndex
getInstalledPackages verbosity packagedbs conf = do
  homedir       <- getHomeDirectory
  (hugsProg, _) <- requireProgram verbosity hugsProgram conf
  let hugsbindir = takeDirectory (programPath hugsProg)
      hugslibdir = takeDirectory hugsbindir </> "lib" </> "hugs"
      dbdirs = nub (concatMap (packageDbPaths homedir hugslibdir) packagedbs)
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
packageDbPaths home hugslibdir db = case db of
  GlobalPackageDB        -> [ hugslibdir </> "packages"
                            , "/usr/local/lib/hugs/packages" ]
  UserPackageDB          -> [ home </> "lib/hugs/packages" ]
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

  let pathsModule = pkgdir </> ("Paths_" ++ display pkgname)  <.> "hs"
  pathsModuleExists <- doesFileExist pathsModule

  case () of
    _ | pkgconfExists     -> getFullInstalledPackageInfo pkgname pkgconfFile
      | pathsModuleExists -> getPhonyInstalledPackageInfo pkgname pathsModule
      | otherwise         -> return Nothing

getFullInstalledPackageInfo :: PackageName -> FilePath
                            -> IO (Maybe InstalledPackageInfo)
getFullInstalledPackageInfo pkgname pkgconfFile =
  withUTF8FileContents pkgconfFile $ \contents ->
    case parseInstalledPackageInfo contents of
      ParseOk _ pkginfo | packageName pkginfo == pkgname
                        -> return (Just pkginfo)
      _                 -> return Nothing

-- | This is a backup option for existing versions of Hugs which do not supply
-- proper installed package info files for the bundled libs. Instead we look
-- for the Paths_pkgname.hs file and extract the package version from that.
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
    -- search through the Paths_pkgname.hs file, looking for a line like:
    --
    -- > version = Version {versionBranch = [2,0], versionTags = []}
    --
    -- and parse it using 'Read'. Yes we are that evil.
    --
    extractVersion content =
      case [ version
           | ("version":"=":rest) <- map words (lines content)
           , (version, []) <- reads (concat rest) ] of
        [version] -> Just version
        _         -> Nothing

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

-- |Building a package for Hugs.
buildLib :: Verbosity -> PackageDescription -> LocalBuildInfo
                      -> Library            -> ComponentLocalBuildInfo -> IO ()
buildLib verbosity pkg_descr lbi lib _clbi = do
    let pref = scratchDir lbi
    createDirectoryIfMissingVerbose verbosity True pref
    copyFileVerbose verbosity (autogenModulesDir lbi </> paths_modulename)
                              (pref </> paths_modulename)
    compileBuildInfo verbosity pref [] (libModules lib) (libBuildInfo lib) lbi
  where
    paths_modulename = ModuleName.toFilePath (autogenModuleName pkg_descr)
                         <.> ".hs"
    --TODO: switch to using autogenModulesDir as a search dir, rather than
    --      always copying the file over.

-- |Building an executable for Hugs.
buildExe :: Verbosity -> PackageDescription -> LocalBuildInfo
                      -> Executable         -> ComponentLocalBuildInfo -> IO ()
buildExe verbosity pkg_descr lbi
  exe@Executable {modulePath=mainPath, buildInfo=bi} _clbi = do
    let pref = scratchDir lbi
    createDirectoryIfMissingVerbose verbosity True pref
    
    let destDir = pref </> "programs"
    let exeMods = otherModules bi
    srcMainFile <- findFile (hsSourceDirs bi) mainPath
    let exeDir = destDir </> exeName exe
    let destMainFile = exeDir </> hugsMainFilename exe
    copyModule verbosity (EnableExtension CPP `elem` allExtensions bi) bi lbi srcMainFile destMainFile
    let destPathsFile = exeDir </> paths_modulename
    copyFileVerbose verbosity (autogenModulesDir lbi </> paths_modulename)
                              destPathsFile
    compileBuildInfo verbosity exeDir 
      (maybe [] (hsSourceDirs . libBuildInfo) (library pkg_descr)) exeMods bi lbi
    compileFiles verbosity bi lbi exeDir [destMainFile, destPathsFile]

  where
    paths_modulename = ModuleName.toFilePath (autogenModuleName pkg_descr)
                         <.> ".hs"

compileBuildInfo :: Verbosity
                 -> FilePath -- ^output directory
                 -> [FilePath] -- ^library source dirs, if building exes
                 -> [ModuleName] -- ^Modules
                 -> BuildInfo
                 -> LocalBuildInfo
                 -> IO ()
--TODO: should not be using mLibSrcDirs at all
compileBuildInfo verbosity destDir mLibSrcDirs mods bi lbi = do
    -- Pass 1: copy or cpp files from build directory to scratch directory
    let useCpp = EnableExtension CPP `elem` allExtensions bi
    let srcDir = buildDir lbi
        srcDirs = nub $ srcDir : hsSourceDirs bi ++ mLibSrcDirs
    info verbosity $ "Source directories: " ++ show srcDirs
    flip mapM_ mods $ \ m -> do
        fs <- findFileWithExtension suffixes srcDirs (ModuleName.toFilePath m)
        case fs of
          Nothing ->
            die ("can't find source for module " ++ display m)
          Just srcFile -> do
            let ext = takeExtension srcFile
            copyModule verbosity useCpp bi lbi srcFile
                (destDir </> ModuleName.toFilePath m <.> ext)
    -- Pass 2: compile foreign stubs in scratch directory
    stubsFileLists <- fmap catMaybes $ sequence
      [ findFileWithExtension suffixes [destDir] (ModuleName.toFilePath modu)
      | modu <- mods]
    compileFiles verbosity bi lbi destDir stubsFileLists

suffixes :: [String]
suffixes = ["hs", "lhs"]

-- Copy or cpp a file from the source directory to the build directory.
copyModule :: Verbosity -> Bool -> BuildInfo -> LocalBuildInfo -> FilePath -> FilePath -> IO ()
copyModule verbosity cppAll bi lbi srcFile destFile = do
    createDirectoryIfMissingVerbose verbosity True (takeDirectory destFile)
    (exts, opts, _) <- getOptionsFromSource srcFile
    let ghcOpts = [ op | (GHC, ops) <- opts, op <- ops ]
    if cppAll || EnableExtension CPP `elem` exts || "-cpp" `elem` ghcOpts then do
        runSimplePreProcessor (ppCpp bi lbi) srcFile destFile verbosity
        return ()
      else
        copyFileVerbose verbosity srcFile destFile

compileFiles :: Verbosity -> BuildInfo -> LocalBuildInfo -> FilePath -> [FilePath] -> IO ()
compileFiles verbosity bi lbi modDir fileList = do
    ffiFileList <- filterM testFFI fileList
    unless (null ffiFileList) $ do
        notice verbosity "Compiling FFI stubs"
        mapM_ (compileFFI verbosity bi lbi modDir) ffiFileList

-- Only compile FFI stubs for a file if it contains some FFI stuff
testFFI :: FilePath -> IO Bool
testFFI file =
  withHaskellFile file $ \inp ->
    return $! "foreign" `elem` symbols (stripComments False inp)

compileFFI :: Verbosity -> BuildInfo -> LocalBuildInfo -> FilePath -> FilePath -> IO ()
compileFFI verbosity bi lbi modDir file = do
    (_, opts, file_incs) <- getOptionsFromSource file
    let ghcOpts = [ op | (GHC, ops) <- opts, op <- ops ]
    let pkg_incs = ["\"" ++ inc ++ "\"" | inc <- includes bi]
    let incs = nub (sort (file_incs ++ includeOpts ghcOpts ++ pkg_incs))
    let pathFlag = "-P" ++ modDir ++ [searchPathSeparator]
    let hugsArgs = "-98" : pathFlag : map ("-i" ++) incs
    cfiles <- getCFiles file
    let cArgs =
            ["-I" ++ dir | dir <- includeDirs bi] ++
            ccOptions bi ++
            cfiles ++
            ["-L" ++ dir | dir <- extraLibDirs bi] ++
            ldOptions bi ++
            ["-l" ++ lib | lib <- extraLibs bi] ++
            concat [["-framework", f] | f <- frameworks bi]
    rawSystemProgramConf verbosity ffihugsProgram (withPrograms lbi)
      (hugsArgs ++ file : cArgs)

includeOpts :: [String] -> [String]
includeOpts [] = []
includeOpts ("-#include" : arg : opts) = arg : includeOpts opts
includeOpts (_ : opts) = includeOpts opts

-- get C file names from CFILES pragmas throughout the source file
getCFiles :: FilePath -> IO [String]
getCFiles file =
  withHaskellFile file $ \inp ->
    let cfiles =
          [ normalise cfile
          | "{-#" : "CFILES" : rest <- map words
                                     $ lines
                                     $ stripComments True inp
          , last rest == "#-}"
          , cfile <- init rest]
     in seq (length cfiles) (return cfiles)

-- List of terminal symbols in a source file.
symbols :: String -> [String]
symbols cs = case lex cs of
    (sym, cs'):_ | not (null sym) -> sym : symbols cs'
    _ -> []

-- Get the non-literate source of a Haskell module.
withHaskellFile :: FilePath -> (String -> IO a) -> IO a
withHaskellFile file action =
    withUTF8FileContents file $ \text ->
        if ".lhs" `isSuffixOf` file
          then either action die (unlit file text)
          else action text

-- ------------------------------------------------------------
-- * options in source files
-- ------------------------------------------------------------

-- |Read the initial part of a source file, before any Haskell code,
-- and return the contents of any LANGUAGE, OPTIONS and INCLUDE pragmas.
getOptionsFromSource
    :: FilePath
    -> IO ([Extension],                 -- LANGUAGE pragma, if any
           [(CompilerFlavor,[String])], -- OPTIONS_FOO pragmas
           [String]                     -- INCLUDE pragmas
          )
getOptionsFromSource file =
    withHaskellFile file $
        (return $!)
      . foldr appendOptions ([],[],[]) . map getOptions
      . takeWhileJust . map getPragma
      . filter textLine . map (dropWhile isSpace) . lines
      . stripComments True

  where textLine [] = False
        textLine ('#':_) = False
        textLine _ = True

        getPragma :: String -> Maybe [String]
        getPragma line = case words line of
            ("{-#" : rest) | last rest == "#-}" -> Just (init rest)
            _ -> Nothing

        getOptions ("OPTIONS":opts) = ([], [(GHC, opts)], [])
        getOptions ("OPTIONS_GHC":opts) = ([], [(GHC, opts)], [])
        getOptions ("OPTIONS_NHC98":opts) = ([], [(NHC, opts)], [])
        getOptions ("OPTIONS_HUGS":opts) = ([], [(Hugs, opts)], [])
        getOptions ("LANGUAGE":ws) = (mapMaybe readExtension ws, [], [])
          where readExtension :: String -> Maybe Extension
                readExtension w = case reads w of
                    [(ext, "")] -> Just ext
                    [(ext, ",")] -> Just ext
                    _ -> Nothing
        getOptions ("INCLUDE":ws) = ([], [], ws)
        getOptions _ = ([], [], [])

        appendOptions (exts, opts, incs) (exts', opts', incs')
          = (exts++exts', opts++opts', incs++incs')

-- takeWhileJust f = map fromJust . takeWhile isJust
takeWhileJust :: [Maybe a] -> [a]
takeWhileJust (Just x:xs) = x : takeWhileJust xs
takeWhileJust _ = []

-- |Strip comments from Haskell source.
stripComments
    :: Bool     -- ^ preserve pragmas?
    -> String   -- ^ input source text
    -> String
stripComments keepPragmas = stripCommentsLevel 0
  where stripCommentsLevel :: Int -> String -> String
        stripCommentsLevel 0 ('"':cs) = '"':copyString cs
        stripCommentsLevel 0 ('-':'-':cs) =     -- FIX: symbols like -->
            stripCommentsLevel 0 (dropWhile (/= '\n') cs)
        stripCommentsLevel 0 ('{':'-':'#':cs)
          | keepPragmas = '{' : '-' : '#' : copyPragma cs
        stripCommentsLevel n ('{':'-':cs) = stripCommentsLevel (n+1) cs
        stripCommentsLevel 0 (c:cs) = c : stripCommentsLevel 0 cs
        stripCommentsLevel n ('-':'}':cs) = stripCommentsLevel (n-1) cs
        stripCommentsLevel n (_:cs) = stripCommentsLevel n cs
        stripCommentsLevel _ [] = []

        copyString ('\\':c:cs) = '\\' : c : copyString cs
        copyString ('"':cs) = '"' : stripCommentsLevel 0 cs
        copyString (c:cs) = c : copyString cs
        copyString [] = []

        copyPragma ('#':'-':'}':cs) = '#' : '-' : '}' : stripCommentsLevel 0 cs
        copyPragma (c:cs) = c : copyPragma cs
        copyPragma [] = []

-- -----------------------------------------------------------------------------
-- |Install for Hugs.
-- For install, copy-prefix = prefix, but for copy they're different.
-- The library goes in \<copy-prefix>\/lib\/hugs\/packages\/\<pkgname>
-- (i.e. \<prefix>\/lib\/hugs\/packages\/\<pkgname> on the target system).
-- Each executable goes in \<copy-prefix>\/lib\/hugs\/programs\/\<exename>
-- (i.e. \<prefix>\/lib\/hugs\/programs\/\<exename> on the target system)
-- with a script \<copy-prefix>\/bin\/\<exename> pointing at
-- \<prefix>\/lib\/hugs\/programs\/\<exename>.
install
    :: Verbosity -- ^verbosity
    -> LocalBuildInfo
    -> FilePath  -- ^Library install location
    -> FilePath  -- ^Program install location
    -> FilePath  -- ^Executable install location
    -> FilePath  -- ^Program location on target system
    -> FilePath  -- ^Build location
    -> (FilePath,FilePath)  -- ^Executable (prefix,suffix)
    -> PackageDescription
    -> IO ()
--FIXME: this script should be generated at build time, just installed at this stage
install verbosity lbi libDir installProgDir binDir targetProgDir buildPref (progprefix,progsuffix) pkg_descr = do
    removeDirectoryRecursive libDir `catchIO` \_ -> return ()
    withLib pkg_descr $ \ lib ->
      findModuleFiles [buildPref] hugsInstallSuffixes (libModules lib)
        >>= installOrdinaryFiles verbosity libDir
    let buildProgDir = buildPref </> "programs"
    when (any (buildable . buildInfo) (executables pkg_descr)) $
        createDirectoryIfMissingVerbose verbosity True binDir
    withExe pkg_descr $ \ exe -> do
        let bi = buildInfo exe
        let theBuildDir = buildProgDir </> exeName exe
        let installDir = installProgDir </> exeName exe
        let targetDir = targetProgDir </> exeName exe
        removeDirectoryRecursive installDir `catchIO` \_ -> return ()
        findModuleFiles [theBuildDir] hugsInstallSuffixes
                        (ModuleName.main : autogenModuleName pkg_descr
                                         : otherModules (buildInfo exe))
          >>= installOrdinaryFiles verbosity installDir
        let targetName = "\"" ++ (targetDir </> hugsMainFilename exe) ++ "\""
        let hugsOptions = hcOptions Hugs (buildInfo exe)
                       ++ languageToFlags (compiler lbi) (defaultLanguage bi)
                       ++ extensionsToFlags (compiler lbi) (allExtensions bi)
            --TODO: also need to consider options, extensions etc of deps
            --      see ticket #43
        let baseExeFile = progprefix ++ (exeName exe) ++ progsuffix
        let exeFile = case buildOS of
                          Windows -> binDir </> baseExeFile <.> ".bat"
                          _       -> binDir </> baseExeFile
        let script = case buildOS of
                         Windows ->
                             let args = hugsOptions ++ [targetName, "%*"]
                             in unlines ["@echo off",
                                         unwords ("runhugs" : args)]
                         _ ->
                             let args = hugsOptions ++ [targetName, "\"$@\""]
                             in unlines ["#! /bin/sh",
                                         unwords ("runhugs" : args)]
        writeFileAtomic exeFile (BS.Char8.pack script)
        setFileExecutable exeFile

hugsInstallSuffixes :: [String]
hugsInstallSuffixes = [".hs", ".lhs", dllExtension]

-- |Filename used by Hugs for the main module of an executable.
-- This is a simple filename, so that Hugs will look for any auxiliary
-- modules it uses relative to the directory it's in.
hugsMainFilename :: Executable -> FilePath
hugsMainFilename exe = "Main" <.> ext
  where ext = takeExtension (modulePath exe)

-- -----------------------------------------------------------------------------
-- Registering

registerPackage
  :: Verbosity
  -> InstalledPackageInfo
  -> PackageDescription
  -> LocalBuildInfo
  -> Bool
  -> PackageDBStack
  -> IO ()
registerPackage verbosity installedPkgInfo pkg lbi inplace _packageDbs = do
  --TODO: prefer to have it based on the packageDbs, but how do we know
  -- the package subdir based on the name? the user can set crazy libsubdir
  let installDirs = absoluteInstallDirs pkg lbi NoCopyDest
      pkgdir  | inplace   = buildDir lbi
              | otherwise = libdir installDirs
  createDirectoryIfMissingVerbose verbosity True pkgdir
  writeUTF8File (pkgdir </> "package.conf")
                (showInstalledPackageInfo installedPkgInfo)
