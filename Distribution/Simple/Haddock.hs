{-# OPTIONS -cpp #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Haddock
-- Copyright   :  Isaac Jones 2003-2005
-- 
-- Maintainer  :  Isaac Jones <ijones@syntaxpolice.org>
-- Stability   :  alpha
-- Portability :  portable
--
-- Invokes haddock to generate api documentation for libraries and optinally
-- executables in this package. Also has support for generating
-- syntax-highlighted source with HsColour and linking the haddock docs to it.

{- All rights reserved.

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

module Distribution.Simple.Haddock (
  haddock, hscolour
  ) where

-- local
import Distribution.Compat.ReadP(readP_to_S)
import Distribution.Package (showPackageId)
import Distribution.PackageDescription
import Distribution.ParseUtils(Field(..), readFields, parseCommaList, parseFilePathQ)
import Distribution.Program(ConfiguredProgram(..), requireProgram, programPath,
                            hscolourProgram, haddockProgram, rawSystemProgram)
import Distribution.PreProcess (ppCpp', ppUnlit, preprocessSources,
                                PPSuffixHandler, runSimplePreProcessor)
import Distribution.Setup
import Distribution.Simple.InstallDirs (InstallDirTemplates(..),
                                        PathTemplateVariable(..),
                                        toPathTemplate, fromPathTemplate,
                                        substPathTemplate,
                                        initialPathTemplateEnv)
import Distribution.Simple.LocalBuildInfo ( LocalBuildInfo(..), hscolourPref,
                                            haddockPref, distPref )
import Distribution.Simple.Utils (die, warn, createDirectoryIfMissingVerbose,
                                  moduleToFilePath, findFile)

import Distribution.Simple.Utils (rawSystemStdout)
import Distribution.Verbosity
import Language.Haskell.Extension
-- Base
import System.Directory(removeFile)

import Control.Monad (liftM, when, join)
import Data.Maybe    ( isJust, catMaybes )

import Distribution.Compat.Directory(removeDirectoryRecursive, copyFile)
import System.FilePath((</>), (<.>), splitFileName, splitExtension,
                       replaceExtension)
import Distribution.Version

-- --------------------------------------------------------------------------
-- Haddock support

haddock :: PackageDescription -> LocalBuildInfo -> [PPSuffixHandler] -> HaddockFlags -> IO ()
haddock pkg_descr _ _ haddockFlags
  | not (hasLibs pkg_descr) && not (haddockExecutables haddockFlags) = do
      when (haddockVerbose haddockFlags >= normal) $
        putStrLn $ "No documentation was generated as this package does not contain a library.\n"
                ++ "Perhaps you want to use the haddock command with the --executables flag."

haddock pkg_descr lbi suffixes haddockFlags@HaddockFlags {
      haddockExecutables = doExes,
      haddockHscolour = hsColour,
      haddockHscolourCss = hsColourCss,
      haddockVerbose = verbosity
    } = do
    when hsColour $ hscolour pkg_descr lbi suffixes $
             HscolourFlags hsColourCss doExes verbosity

    (confHaddock, _) <- requireProgram verbosity haddockProgram
                        (orLaterVersion (Version [0,6] [])) (withPrograms lbi)

    let tmpDir = buildDir lbi </> "tmp"
    createDirectoryIfMissingVerbose verbosity True tmpDir
    createDirectoryIfMissingVerbose verbosity True $ haddockPref pkg_descr
    preprocessSources pkg_descr lbi False verbosity suffixes

    setupMessage verbosity "Running Haddock for" pkg_descr

    let replaceLitExts = map ( (tmpDir </>) . (`replaceExtension` "hs") )
    let mockAll bi = mapM_ (mockPP ["-D__HADDOCK__"] bi tmpDir)
    let showPkg    = showPackageId (package pkg_descr)
    let outputFlag = if haddockHoogle haddockFlags
                     then "--hoogle"
                     else "--html"
    let Just version = programVersion confHaddock
    let have_src_hyperlink_flags = version >= Version [0,8] []
        have_new_flags           = version >  Version [0,8] []
    let comp = compiler lbi
        pkgTool = compilerPkgTool comp
    let ghcpkgFlags = if have_new_flags
                      then ["--ghc-pkg=" ++ programPath pkgTool]
                      else []
    let cssFileFlag = case haddockCss haddockFlags of
                        Nothing -> []
                        Just cssFile -> ["--css=" ++ cssFile]
    let verboseFlags = if verbosity > deafening then ["--verbose"] else []
    let allowMissingHtmlFlags = if have_new_flags
                                then ["--allow-missing-html"]
                                else []
    when (hsColour && not have_src_hyperlink_flags) $
         die "haddock --hyperlink-source requires Haddock version 0.8 or later"
    let linkToHscolour = if hsColour
            then ["--source-module=src/%{MODULE/./-}.html"
                 ,"--source-entity=src/%{MODULE/./-}.html#%{NAME}"]
            else []

    let getField pkgId f = do
            let name = showPackageId pkgId
            s <- rawSystemStdout verbosity (programPath pkgTool) ["field", name, f]
            case readFields s of
                (ParseOk _ ((F _ _ fieldVal):_)) ->
                    return . join . join . take 1 . map fst . filter (null . snd)
                        . readP_to_S (parseCommaList parseFilePathQ) $ fieldVal
                _ -> do
                    warn verbosity $ "Unrecognised output from ghc-pkg field "
		                  ++ name ++ " " ++ f ++ ": " ++ s
                    return []
    let makeReadInterface pkgId = do
            interface <- getField pkgId "haddock-interfaces"
            html <- case haddockHtmlLocation haddockFlags of
                Nothing -> getField pkgId "haddock-html"
                Just htmlStrTemplate ->
                  let env0 = initialPathTemplateEnv pkgId (compilerId comp)
                      prefixSubst = prefixDirTemplate (installDirTemplates lbi)
                      env = (PrefixVar, prefixSubst) : env0
                      expandTemplateVars = fromPathTemplate
                                         . substPathTemplate env
                                         . toPathTemplate
                   in return (expandTemplateVars htmlStrTemplate)
            return $ if null interface
                then Nothing
                else Just $ "--read-interface=" ++
                            (if null html then "" else html ++ ",") ++
                            interface
    packageFlags <- liftM catMaybes $ mapM makeReadInterface (packageDeps lbi)

    withLib pkg_descr () $ \lib -> do
        let bi = libBuildInfo lib
        inFiles <- getModulePaths lbi bi (exposedModules lib ++ otherModules bi)
        mockAll bi inFiles
        let prologName = distPref </> showPkg ++ "-haddock-prolog.txt"
            prolog | null (description pkg_descr) = synopsis pkg_descr
                   | otherwise                    = description pkg_descr
            subtitle | null (synopsis pkg_descr) = ""
                     | otherwise                 = ": " ++ synopsis pkg_descr
        writeFile prologName (prolog ++ "\n")
        let outFiles = replaceLitExts inFiles
        let haddockFile = haddockPref pkg_descr </> haddockName pkg_descr
        -- FIX: replace w/ rawSystemProgramConf?
        rawSystemProgram verbosity confHaddock
                ([outputFlag,
                  "--odir=" ++ haddockPref pkg_descr,
                  "--title=" ++ showPkg ++ subtitle,
                  "--package=" ++ showPkg,
                  "--dump-interface=" ++ haddockFile,
                  "--prologue=" ++ prologName]
                 ++ ghcpkgFlags
                 ++ allowMissingHtmlFlags
		 ++ cssFileFlag
                 ++ linkToHscolour
                 ++ packageFlags
                 ++ programArgs confHaddock
                 ++ verboseFlags
                 ++ outFiles
                 ++ map ("--hide=" ++) (otherModules bi)
                )
        removeFile prologName
        when (verbosity >= normal) $
          putStrLn $ "Documentation created: "
                  ++ (haddockPref pkg_descr </> "index.html")

    withExe pkg_descr $ \exe -> when doExes $ do
        let bi = buildInfo exe
            exeTargetDir = haddockPref pkg_descr </> exeName exe
        createDirectoryIfMissingVerbose verbosity True exeTargetDir
        inFiles' <- getModulePaths lbi bi (otherModules bi)
        srcMainPath <- findFile (hsSourceDirs bi) (modulePath exe)
        let inFiles = srcMainPath : inFiles'
        mockAll bi inFiles
        let prologName = distPref </> showPkg ++ "-haddock-prolog.txt"
            prolog | null (description pkg_descr) = synopsis pkg_descr
                   | otherwise                    = description pkg_descr
        writeFile prologName (prolog ++ "\n")
        let outFiles = replaceLitExts inFiles
        rawSystemProgram verbosity confHaddock
                ([outputFlag,
                  "--odir=" ++ exeTargetDir,
                  "--title=" ++ exeName exe,
                  "--prologue=" ++ prologName]
                 ++ ghcpkgFlags
                 ++ allowMissingHtmlFlags
                 ++ linkToHscolour
                 ++ packageFlags
                 ++ programArgs confHaddock
                 ++ verboseFlags
                 ++ outFiles
                )
        removeFile prologName
        when (verbosity >= normal) $
          putStrLn $ "Documentation created: "
                  ++ (exeTargetDir </> "index.html")

    removeDirectoryRecursive tmpDir
  where
        mockPP inputArgs bi pref file
            = do let (filePref, fileName) = splitFileName file
                 let targetDir  = pref </> filePref
                 let targetFile = targetDir </> fileName
                 let (targetFileNoext, targetFileExt) = splitExtension targetFile
                 createDirectoryIfMissingVerbose verbosity True targetDir
                 if needsCpp bi
                    then runSimplePreProcessor (ppCpp' inputArgs bi lbi)
                           file targetFile verbosity
                    else copyFile file targetFile
                 when (targetFileExt == ".lhs") $ do
                       runSimplePreProcessor ppUnlit
                         targetFile (targetFileNoext <.> "hs") verbosity
                       return ()
        needsCpp :: BuildInfo -> Bool
        needsCpp bi = CPP `elem` extensions bi

-- --------------------------------------------------------------------------
-- hscolour support

hscolour :: PackageDescription -> LocalBuildInfo -> [PPSuffixHandler] -> HscolourFlags -> IO ()
hscolour pkg_descr lbi suffixes (HscolourFlags stylesheet doExes verbosity) = do
    (confHscolour, _) <- requireProgram verbosity hscolourProgram
                         (orLaterVersion (Version [1,8] [])) (withPrograms lbi)

    createDirectoryIfMissingVerbose verbosity True $ hscolourPref pkg_descr
    preprocessSources pkg_descr lbi False verbosity suffixes

    setupMessage verbosity "Running hscolour for" pkg_descr
    let replaceDot = map (\c -> if c == '.' then '-' else c)

    withLib pkg_descr () $ \lib -> when (isJust $ library pkg_descr) $ do
        let bi = libBuildInfo lib
        let modules = exposedModules lib ++ otherModules bi
        inFiles <- getModulePaths lbi bi modules
        flip mapM_ (zip modules inFiles) $ \(mo, inFile) -> do
            let outputDir = hscolourPref pkg_descr </> "src"
            let outFile = outputDir </> replaceDot mo <.> "html"
            createDirectoryIfMissingVerbose verbosity True outputDir
            copyCSS outputDir
            rawSystemProgram verbosity confHscolour
                     ["-css", "-anchor", "-o" ++ outFile, inFile]

    withExe pkg_descr $ \exe -> when doExes $ do
        let bi = buildInfo exe
        let modules = "Main" : otherModules bi
        let outputDir = hscolourPref pkg_descr </> exeName exe </> "src"
        createDirectoryIfMissingVerbose verbosity True outputDir
        copyCSS outputDir
        srcMainPath <- findFile (hsSourceDirs bi) (modulePath exe)
        inFiles <- liftM (srcMainPath :) $ getModulePaths lbi bi (otherModules bi)
        flip mapM_ (zip modules inFiles) $ \(mo, inFile) -> do
            let outFile = outputDir </> replaceDot mo <.> "html"
            rawSystemProgram verbosity confHscolour
                     ["-css", "-anchor", "-o" ++ outFile, inFile]
  where copyCSS dir = case stylesheet of
                      Nothing -> return ()
                      Just s -> copyFile s (dir </> "hscolour.css")


--TODO: where to put this? it's duplicated in .Simple too
getModulePaths :: LocalBuildInfo -> BuildInfo -> [String] -> IO [FilePath]
getModulePaths lbi bi =
   fmap concat .
      mapM (flip (moduleToFilePath (buildDir lbi : hsSourceDirs bi)) ["hs", "lhs"])
