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
import Distribution.Simple.Program(ConfiguredProgram(..), requireProgram, 
                            lookupProgram, programPath, ghcPkgProgram,
			    hscolourProgram, haddockProgram, rawSystemProgram, rawSystemProgramStdoutConf,
          ghcProgram)
import Distribution.Simple.PreProcess (ppCpp', ppUnlit, preprocessSources,
                                PPSuffixHandler, runSimplePreProcessor)
import Distribution.Simple.Setup
import Distribution.Simple.Build (initialBuildSteps)
import Distribution.Simple.InstallDirs (InstallDirs(..),
                                        PathTemplateVariable(..),
                                        toPathTemplate, fromPathTemplate,
                                        substPathTemplate,
                                        initialPathTemplateEnv)
import Distribution.Simple.LocalBuildInfo ( LocalBuildInfo(..), hscolourPref,
                                            haddockPref, distPref, autogenModulesDir )
import Distribution.Simple.Utils (die, warn, notice, createDirectoryIfMissingVerbose,
                                  moduleToFilePath, findFile)

import Distribution.Simple.Utils (rawSystemStdout)
import Distribution.Verbosity
import Language.Haskell.Extension
-- Base
import System.Directory(removeFile, doesFileExist,
                        removeDirectoryRecursive, copyFile)

import Control.Monad (liftM, when, unless, join)
import Data.Maybe    ( isJust, catMaybes, fromJust )
import Data.Char     (isSpace)
import Data.List     (nub)

import System.FilePath((</>), (<.>), splitFileName, splitExtension,
                       replaceExtension)
import Distribution.Version
import Distribution.Simple.Compiler (compilerVersion, extensionsToFlags)

-- --------------------------------------------------------------------------
-- Haddock support

haddock :: PackageDescription -> LocalBuildInfo -> [PPSuffixHandler] -> HaddockFlags -> IO ()
haddock pkg_descr _ _ haddockFlags
  | not (hasLibs pkg_descr) && not (fromFlag $ haddockExecutables haddockFlags) =
      warn (fromFlag $ haddockVerbose haddockFlags) $
           "No documentation was generated as this package does not contain "
        ++ "a\nlibrary. Perhaps you want to use the haddock command with the "
        ++ "--executables flag."

haddock pkg_descr lbi suffixes flags = do
    let doExes   = fromFlag (haddockExecutables flags)
        hsColour = fromFlag (haddockHscolour flags)
    when hsColour $ hscolour pkg_descr lbi suffixes defaultHscolourFlags {
      hscolourCSS         = haddockHscolourCss flags,
      hscolourExecutables = haddockExecutables flags,
      hscolourVerbose     = haddockVerbose flags
    }

    (confHaddock, _) <- requireProgram verbosity haddockProgram
                        (orLaterVersion (Version [0,6] [])) (withPrograms lbi)

    let tmpDir = buildDir lbi </> "tmp"
    createDirectoryIfMissingVerbose verbosity True tmpDir
    createDirectoryIfMissingVerbose verbosity True $ haddockPref pkg_descr
    preprocessSources pkg_descr lbi False verbosity suffixes

    setupMessage verbosity "Running Haddock for" pkg_descr

    let replaceLitExts = map ( (tmpDir </>) . (`replaceExtension` "hs") )
    let showPkg    = showPackageId (package pkg_descr)
    let outputFlag = if fromFlag (haddockHoogle flags)
                     then "--hoogle"
                     else "--html"
    let Just version = programVersion confHaddock
    let have_src_hyperlink_flags = version >= Version [0,8] []
        isVersion2               = version >= Version [2,0] []

    let mockFlags
          | isVersion2 = []
          | otherwise  = ["-D__HADDOCK__"]

    let mockAll bi = mapM_ (mockPP mockFlags bi tmpDir)

    let comp = compiler lbi
        Just pkgTool = lookupProgram ghcPkgProgram (withPrograms lbi)
    let cssFileFlag = case flagToMaybe $ haddockCss flags of
                        Nothing -> []
                        Just cssFile -> ["--css=" ++ cssFile]
    let verboseFlags = if verbosity > deafening then ["--verbose"] else []
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
            html <- case flagToMaybe $ haddockHtmlLocation flags of
                Nothing -> getField pkgId "haddock-html"
                Just htmlStrTemplate ->
                  let env0 = initialPathTemplateEnv pkgId (compilerId comp)
                      prefixSubst = prefix (installDirTemplates lbi)
                      env = (PrefixVar, prefixSubst) : env0
                      expandTemplateVars = fromPathTemplate
                                         . substPathTemplate env
                                         . toPathTemplate
                   in return (expandTemplateVars htmlStrTemplate)
            interfaceExists <- doesFileExist interface
            if interfaceExists
              then return $ Just $ "--read-interface="
                         ++ (if null html then "" else html ++ ",")
                         ++ interface
              else do warn verbosity $ "The documentation for package "
                         ++ showPackageId pkgId ++ " is not installed. "
                         ++ "No links to it will be generated."
                      return Nothing

    packageFlags <- liftM catMaybes $ mapM makeReadInterface (packageDeps lbi)

    when isVersion2 $ do
      strHadGhcVers <- rawSystemProgramStdoutConf verbosity haddockProgram (withPrograms lbi) ["--ghc-version"]
      let mHadGhcVers = readVersion strHadGhcVers
      when (mHadGhcVers == Nothing) $ die "Could not get GHC version from Haddock"
      when (fromJust mHadGhcVers /= compilerVersion comp) $
        die "Haddock's internal GHC version must match the configured GHC version"

    ghcLibDir0 <- rawSystemProgramStdoutConf verbosity ghcProgram (withPrograms lbi) ["--print-libdir"]
    let ghcLibDir = reverse $ dropWhile isSpace $ reverse ghcLibDir0

    let packageName = if isVersion2
          then ["--optghc=-package-name", "--optghc=" ++ showPkg]
          else ["--package=" ++ showPkg]

    let haddock2options bi preprocessDir = if isVersion2
          then ("-B" ++ ghcLibDir) : map ("--optghc=" ++) (ghcSimpleOptions lbi bi preprocessDir)
          else []

    when isVersion2 $ initialBuildSteps pkg_descr lbi verbosity suffixes

    withLib pkg_descr () $ \lib -> do
        let bi = libBuildInfo lib
            modules = exposedModules lib ++ otherModules bi
        inFiles <- getModulePaths lbi bi modules
        unless isVersion2 $ mockAll bi inFiles
        let prologName = distPref </> showPkg ++ "-haddock-prolog.txt"
            prolog | null (description pkg_descr) = synopsis pkg_descr
                   | otherwise                    = description pkg_descr
            subtitle | null (synopsis pkg_descr) = ""
                     | otherwise                 = ": " ++ synopsis pkg_descr
        writeFile prologName (prolog ++ "\n")
        let targets
              | isVersion2 = modules
              | otherwise  = replaceLitExts inFiles
        let haddockFile = haddockPref pkg_descr </> haddockName pkg_descr
        -- FIX: replace w/ rawSystemProgramConf?
        rawSystemProgram verbosity confHaddock
                ([outputFlag,
                  "--odir=" ++ haddockPref pkg_descr,
                  "--title=" ++ showPkg ++ subtitle,
                  "--dump-interface=" ++ haddockFile,
                  "--prologue=" ++ prologName]
                 ++ packageName
		 ++ cssFileFlag
                 ++ linkToHscolour
                 ++ packageFlags
                 ++ programArgs confHaddock
                 ++ verboseFlags
                 ++ map ("--hide=" ++) (otherModules bi)
                 ++ haddock2options bi (buildDir lbi)
                 ++ targets
                )
        removeFile prologName
        notice verbosity $ "Documentation created: "
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
        let targets
              | isVersion2 = srcMainPath : otherModules bi
              | otherwise = replaceLitExts inFiles
        let preprocessDir = buildDir lbi </> exeName exe </> exeName exe ++ "-tmp"
        rawSystemProgram verbosity confHaddock
                ([outputFlag,
                  "--odir=" ++ exeTargetDir,
                  "--title=" ++ exeName exe,
                  "--prologue=" ++ prologName]
                 ++ linkToHscolour
                 ++ packageFlags
                 ++ programArgs confHaddock
                 ++ verboseFlags
                 ++ haddock2options bi preprocessDir
                 ++ targets
                )
        removeFile prologName
        notice verbosity $ "Documentation created: "
                       ++ (exeTargetDir </> "index.html")

    removeDirectoryRecursive tmpDir
  where
        verbosity = fromFlag (haddockVerbose flags)
        mockPP inputArgs bi pref file
            = do let (filePref, fileName) = splitFileName file
                 let targetDir  = pref </> filePref
                 let targetFile = targetDir </> fileName
                 let (targetFileNoext, targetFileExt) = splitExtension targetFile
                 let cppOutput = targetFileNoext <.> "hspp"
                 let hsFile = targetFileNoext <.> "hs"
                 createDirectoryIfMissingVerbose verbosity True targetDir
                 -- Run unlit first, then CPP
                 if (targetFileExt == ".lhs")
                     then runSimplePreProcessor ppUnlit file hsFile verbosity
                     else copyFile file hsFile
                 when (needsCpp bi) $ do
                     runSimplePreProcessor (ppCpp' inputArgs bi lbi)
                       hsFile cppOutput verbosity
                     removeFile hsFile
                     copyFile cppOutput hsFile
                     removeFile cppOutput
        needsCpp :: BuildInfo -> Bool
        needsCpp bi = CPP `elem` extensions bi


ghcSimpleOptions :: LocalBuildInfo -> BuildInfo -> FilePath -> [String]
ghcSimpleOptions lbi bi mockDir
  =  ["-hide-all-packages"]
  ++ (concat [ ["-package", showPackageId pkg] | pkg <- packageDeps lbi ])
  ++ ["-i"]
  ++ hcOptions GHC (options bi)
  ++ ["-i" ++ autogenModulesDir lbi]
  ++ ["-i" ++ l | l <- nub (hsSourceDirs bi)]
  ++ ["-i" ++ mockDir]
  ++ ["-I" ++ dir | dir <- includeDirs bi]
  ++ ["-odir", mockDir]
  ++ ["-hidir", mockDir]
  ++ extensionsToFlags c (extensions bi)
  where c = compiler lbi


-- --------------------------------------------------------------------------
-- hscolour support

hscolour :: PackageDescription -> LocalBuildInfo -> [PPSuffixHandler] -> HscolourFlags -> IO ()
hscolour pkg_descr lbi suffixes flags = do
    (hscolourProg, _) <- requireProgram verbosity hscolourProgram
                         (orLaterVersion (Version [1,8] [])) (withPrograms lbi)

    createDirectoryIfMissingVerbose verbosity True $ hscolourPref pkg_descr
    preprocessSources pkg_descr lbi False verbosity suffixes

    setupMessage verbosity "Running hscolour for" pkg_descr
    let replaceDot = map (\c -> if c == '.' then '-' else c)

    withLib pkg_descr () $ \lib -> when (isJust $ library pkg_descr) $ do
        let bi = libBuildInfo lib
            modules = exposedModules lib ++ otherModules bi
	    outputDir = hscolourPref pkg_descr </> "src"
	createDirectoryIfMissingVerbose verbosity True outputDir
	copyCSS hscolourProg outputDir
        inFiles <- getModulePaths lbi bi modules
        flip mapM_ (zip modules inFiles) $ \(mo, inFile) ->
            let outFile = outputDir </> replaceDot mo <.> "html"
             in rawSystemProgram verbosity hscolourProg
                     ["-css", "-anchor", "-o" ++ outFile, inFile]

    withExe pkg_descr $ \exe -> when doExes $ do
        let bi = buildInfo exe
            modules = "Main" : otherModules bi
            outputDir = hscolourPref pkg_descr </> exeName exe </> "src"
        createDirectoryIfMissingVerbose verbosity True outputDir
        copyCSS hscolourProg outputDir
        srcMainPath <- findFile (hsSourceDirs bi) (modulePath exe)
        inFiles <- liftM (srcMainPath :) $ getModulePaths lbi bi (otherModules bi)
        flip mapM_ (zip modules inFiles) $ \(mo, inFile) ->
            let outFile = outputDir </> replaceDot mo <.> "html"
            in rawSystemProgram verbosity hscolourProg
                     ["-css", "-anchor", "-o" ++ outFile, inFile]

  where copyCSS hscolourProg dir = case stylesheet of
          Nothing | programVersion hscolourProg >= Just (Version [1,9] []) ->
                    rawSystemProgram verbosity hscolourProg
                      ["-print-css", "-o" ++ dir </> "hscolour.css"]
                  | otherwise -> return ()
          Just s -> copyFile s (dir </> "hscolour.css")
        doExes     = fromFlag (hscolourExecutables flags)
        stylesheet = flagToMaybe (hscolourCSS flags)
        verbosity  = fromFlag (hscolourVerbose flags)


--TODO: where to put this? it's duplicated in .Simple too
getModulePaths :: LocalBuildInfo -> BuildInfo -> [String] -> IO [FilePath]
getModulePaths lbi bi =
   fmap concat .
      mapM (flip (moduleToFilePath (buildDir lbi : hsSourceDirs bi)) ["hs", "lhs"])
