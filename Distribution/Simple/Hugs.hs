-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Hugs
-- Copyright   :  Isaac Jones 2003-2006
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This module contains most of the NHC-specific code for configuring, building
-- and installing packages.

{- Copyright (c) 2003-2005, Isaac Jones
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Isaac Jones nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. -}

module Distribution.Simple.Hugs (
        configure, build, install
 ) where

import Distribution.PackageDescription
         ( PackageDescription(..), BuildInfo(..), hcOptions,
           Executable(..), withExe, Library(..), withLib, libModules )
import Distribution.ModuleName (ModuleName)
import qualified Distribution.ModuleName as ModuleName
import Distribution.Simple.Compiler
         ( CompilerFlavor(..), CompilerId(..), Compiler(..), Flag )
import Distribution.Simple.Program     ( ProgramConfiguration, userMaybeSpecifyPath,
                                  requireProgram, rawSystemProgramConf,
                                  ffihugsProgram, hugsProgram )
import Distribution.Version     ( Version(..), VersionRange(AnyVersion) )
import Distribution.Simple.PreProcess   ( ppCpp, runSimplePreProcessor )
import Distribution.Simple.PreProcess.Unlit
                                ( unlit )
import Distribution.Simple.LocalBuildInfo
                                ( LocalBuildInfo(..))
import Distribution.Simple.BuildPaths
                                ( autogenModuleName, autogenModulesDir,
                                  dllExtension )
import Distribution.Simple.Utils
         ( createDirectoryIfMissingVerbose
         , withUTF8FileContents, writeFileAtomic
         , findFile, findFileWithExtension, smartCopySources
         , die, info, notice )
import Language.Haskell.Extension
                                ( Extension(..) )
import System.FilePath          ( (</>), takeExtension, (<.>),
                                  searchPathSeparator, normalise, takeDirectory )
import Distribution.System
         ( OS(..), buildOS )
import Distribution.Text
         ( display )
import Distribution.Verbosity

import Data.Char                ( isSpace )
import Data.Maybe               ( mapMaybe, catMaybes )
import Control.Monad            ( unless, when, filterM )
import Data.List                ( nub, sort, isSuffixOf )
import System.Directory         ( Permissions(..), getPermissions,
                                  setPermissions, copyFile,
                                  removeDirectoryRecursive )
import Distribution.Compat.Exception

-- -----------------------------------------------------------------------------
-- Configuring

configure :: Verbosity -> Maybe FilePath -> Maybe FilePath
          -> ProgramConfiguration -> IO (Compiler, ProgramConfiguration)
configure verbosity hcPath _hcPkgPath conf = do

  (_ffihugsProg, conf') <- requireProgram verbosity ffihugsProgram AnyVersion
                            (userMaybeSpecifyPath "ffihugs" hcPath conf)
  (_hugsProg, conf'')   <- requireProgram verbosity hugsProgram AnyVersion conf'

  let comp = Compiler {
        compilerId             = CompilerId Hugs (Version [] []),
        compilerExtensions     = hugsLanguageExtensions
      }
  return (comp, conf'')

-- | The flags for the supported extensions
hugsLanguageExtensions :: [(Extension, Flag)]
hugsLanguageExtensions =
    [(OverlappingInstances       , "+o")
    ,(IncoherentInstances        , "+oO")
    ,(HereDocuments              , "+H")
    ,(TypeSynonymInstances       , "-98")
    ,(RecursiveDo                , "-98")
    ,(ParallelListComp           , "-98")
    ,(MultiParamTypeClasses      , "-98")
    ,(FunctionalDependencies     , "-98")
    ,(Rank2Types                 , "-98")
    ,(PolymorphicComponents      , "-98")
    ,(ExistentialQuantification  , "-98")
    ,(ScopedTypeVariables        , "-98")
    ,(ImplicitParams             , "-98")
    ,(ExtensibleRecords          , "-98")
    ,(RestrictedTypeSynonyms     , "-98")
    ,(FlexibleContexts           , "-98")
    ,(FlexibleInstances          , "-98")
    ,(ForeignFunctionInterface   , "")
    ,(EmptyDataDecls             , "")
    ,(CPP                        , "")
    ]

-- -----------------------------------------------------------------------------
-- Building

-- |Building a package for Hugs.
build :: PackageDescription -> LocalBuildInfo -> Verbosity -> IO ()
build pkg_descr lbi verbosity = do
    let pref = scratchDir lbi
    createDirectoryIfMissingVerbose verbosity True pref
    withLib pkg_descr () $ \ l -> do
        copyFile (autogenModulesDir lbi </> paths_modulename)
                (pref </> paths_modulename)
        compileBuildInfo pref [] (libModules pkg_descr) (libBuildInfo l)
    withExe pkg_descr $ compileExecutable (pref </> "programs")
  where
        srcDir = buildDir lbi

        paths_modulename = ModuleName.toFilePath (autogenModuleName pkg_descr)
                             <.> ".hs"

        compileExecutable :: FilePath -> Executable -> IO ()
        compileExecutable destDir (exe@Executable {modulePath=mainPath, buildInfo=bi}) = do
            let exeMods = otherModules bi
            srcMainFile <- findFile (hsSourceDirs bi) mainPath
            let exeDir = destDir </> exeName exe
            let destMainFile = exeDir </> hugsMainFilename exe
            copyModule (CPP `elem` extensions bi) bi srcMainFile destMainFile
            let destPathsFile = exeDir </> paths_modulename
            copyFile (autogenModulesDir lbi </> paths_modulename)
                     destPathsFile
            compileBuildInfo exeDir (maybe [] (hsSourceDirs . libBuildInfo) (library pkg_descr)) exeMods bi
            compileFiles bi exeDir [destMainFile, destPathsFile]

        compileBuildInfo :: FilePath -- ^output directory
                         -> [FilePath] -- ^library source dirs, if building exes
                         -> [ModuleName] -- ^Modules
                         -> BuildInfo -> IO ()
        compileBuildInfo destDir mLibSrcDirs mods bi = do
            -- Pass 1: copy or cpp files from build directory to scratch directory
            let useCpp = CPP `elem` extensions bi
            let srcDirs = nub $ srcDir : hsSourceDirs bi ++ mLibSrcDirs
            info verbosity $ "Source directories: " ++ show srcDirs
            flip mapM_ mods $ \ m -> do
                fs <- findFileWithExtension suffixes srcDirs (ModuleName.toFilePath m)
                case fs of
                  Nothing ->
                    die ("can't find source for module " ++ display m)
                  Just srcFile -> do
                    let ext = takeExtension srcFile
                    copyModule useCpp bi srcFile
                        (destDir </> ModuleName.toFilePath m <.> ext)
            -- Pass 2: compile foreign stubs in scratch directory
            stubsFileLists <- fmap catMaybes $ sequence
              [ findFileWithExtension suffixes [destDir] (ModuleName.toFilePath modu)
              | modu <- mods]
            compileFiles bi destDir stubsFileLists

        suffixes = ["hs", "lhs"]

        -- Copy or cpp a file from the source directory to the build directory.
        copyModule :: Bool -> BuildInfo -> FilePath -> FilePath -> IO ()
        copyModule cppAll bi srcFile destFile = do
            createDirectoryIfMissingVerbose verbosity True (takeDirectory destFile)
            (exts, opts, _) <- getOptionsFromSource srcFile
            let ghcOpts = [ op | (GHC, ops) <- opts, op <- ops ]
            if cppAll || CPP `elem` exts || "-cpp" `elem` ghcOpts then do
                runSimplePreProcessor (ppCpp bi lbi) srcFile destFile verbosity
                return ()
              else
                copyFile srcFile destFile

        compileFiles :: BuildInfo -> FilePath -> [FilePath] -> IO ()
        compileFiles bi modDir fileList = do
            ffiFileList <- filterM testFFI fileList
            unless (null ffiFileList) $ do
                notice verbosity "Compiling FFI stubs"
                mapM_ (compileFFI bi modDir) ffiFileList

        -- Only compile FFI stubs for a file if it contains some FFI stuff
        testFFI :: FilePath -> IO Bool
        testFFI file =
          withHaskellFile file $ \inp ->
            return $! "foreign" `elem` symbols (stripComments False inp)

        compileFFI :: BuildInfo -> FilePath -> FilePath -> IO ()
        compileFFI bi modDir file = do
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
    -> FilePath  -- ^Library install location
    -> FilePath  -- ^Program install location
    -> FilePath  -- ^Executable install location
    -> FilePath  -- ^Program location on target system
    -> FilePath  -- ^Build location
    -> (FilePath,FilePath)  -- ^Executable (prefix,suffix)
    -> PackageDescription
    -> IO ()
install verbosity libDir installProgDir binDir targetProgDir buildPref (progprefix,progsuffix) pkg_descr = do
    removeDirectoryRecursive libDir `catchIO` \_ -> return ()
    smartCopySources verbosity [buildPref] libDir (libModules pkg_descr) hugsInstallSuffixes
    let buildProgDir = buildPref </> "programs"
    when (any (buildable . buildInfo) (executables pkg_descr)) $
        createDirectoryIfMissingVerbose verbosity True binDir
    withExe pkg_descr $ \ exe -> do
        let theBuildDir = buildProgDir </> exeName exe
        let installDir = installProgDir </> exeName exe
        let targetDir = targetProgDir </> exeName exe
        removeDirectoryRecursive installDir `catchIO` \_ -> return ()
        smartCopySources verbosity [theBuildDir] installDir
            (ModuleName.main : autogenModuleName pkg_descr
                             : otherModules (buildInfo exe))
            hugsInstallSuffixes
        let targetName = "\"" ++ (targetDir </> hugsMainFilename exe) ++ "\""
        -- FIX (HUGS): use extensions, and options from file too?
        -- see http://hackage.haskell.org/trac/hackage/ticket/43
        let hugsOptions = hcOptions Hugs (buildInfo exe)
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
        writeFileAtomic exeFile script
        perms <- getPermissions exeFile
        setPermissions exeFile perms { executable = True, readable = True }

hugsInstallSuffixes :: [String]
hugsInstallSuffixes = [".hs", ".lhs", dllExtension]

-- |Filename used by Hugs for the main module of an executable.
-- This is a simple filename, so that Hugs will look for any auxiliary
-- modules it uses relative to the directory it's in.
hugsMainFilename :: Executable -> FilePath
hugsMainFilename exe = "Main" <.> ext
  where ext = takeExtension (modulePath exe)
