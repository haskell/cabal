-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Haddock
-- Copyright   :  Isaac Jones 2003-2005
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This module deals with the @haddock@ and @hscolour@ commands. Sadly this is
-- a rather complicated module. It deals with two versions of haddock (0.x and
-- 2.x). It has to do pre-processing for haddock 0.x which involves
-- \'unlit\'ing and using @-DHADDOCK@ for any source code that uses @cpp@. It
-- uses information about installed packages (from @ghc-pkg@) to find the
-- locations of documentation for dependent packages, so it can create links.
--
-- The @hscolour@ support allows generating html versions of the original
-- source, with coloured syntax highlighting.

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
import Distribution.Package
         ( PackageIdentifier, Package(..) )
import qualified Distribution.ModuleName as ModuleName
import Distribution.PackageDescription as PD
         (PackageDescription(..), BuildInfo(..), hcOptions,
          Library(..), hasLibs, withLib,
          Executable(..), withExe)
import Distribution.Simple.Compiler
         ( Compiler(..), CompilerFlavor(..), compilerVersion
         , extensionsToFlags )
import Distribution.Simple.Program
         ( ConfiguredProgram(..), requireProgram
         , rawSystemProgram, rawSystemProgramStdoutConf, rawSystemProgramStdout
         , hscolourProgram, haddockProgram, ghcProgram )
import Distribution.Simple.PreProcess (ppCpp', ppUnlit, preprocessSources,
                                PPSuffixHandler, runSimplePreProcessor)
import Distribution.Simple.Setup
import Distribution.Simple.Build (initialBuildSteps)
import Distribution.Simple.InstallDirs (InstallDirs(..), PathTemplate,
                                        PathTemplateVariable(..),
                                        toPathTemplate, fromPathTemplate,
                                        substPathTemplate,
                                        initialPathTemplateEnv)
import Distribution.Simple.LocalBuildInfo ( LocalBuildInfo(..) )
import Distribution.Simple.BuildPaths ( haddockPref, haddockName,
                                        hscolourPref, autogenModulesDir,
                                        cppHeaderName )
import Distribution.Simple.PackageIndex (dependencyClosure, allPackages)
import qualified Distribution.Simple.PackageIndex as PackageIndex
         ( lookupPackageId )
import qualified Distribution.InstalledPackageInfo as InstalledPackageInfo
         ( InstalledPackageInfo_(..) )
import Distribution.Simple.Utils
         ( die, warn, notice, intercalate, setupMessage
         , createDirectoryIfMissingVerbose, withTempFile
         , findFileWithExtension, findFile )
import Distribution.Text
         ( display, simpleParse )

import Distribution.Verbosity
import Language.Haskell.Extension
-- Base
import System.Directory(removeFile, doesFileExist,
                        removeDirectoryRecursive)
import Distribution.Compat.CopyFile
         ( copyFile )
import Control.Monad ( when, unless )
import Data.Maybe    ( isJust, fromJust, listToMaybe )
import Data.Char     (isSpace)
import Data.List     (nub)

import System.FilePath((</>), (<.>), splitFileName, splitExtension,
                       replaceExtension, normalise)
import System.IO (hClose, hPutStrLn)
import Distribution.Version

-- --------------------------------------------------------------------------
-- Haddock support

haddock :: PackageDescription -> LocalBuildInfo -> [PPSuffixHandler] -> HaddockFlags -> IO ()
haddock pkg_descr _ _ haddockFlags
  |    not (hasLibs pkg_descr)
    && not (fromFlag $ haddockExecutables haddockFlags) =
      warn (fromFlag $ haddockVerbosity haddockFlags) $
           "No documentation was generated as this package does not contain "
        ++ "a library. Perhaps you want to use the --executables flag."

haddock pkg_descr lbi suffixes flags = do
    let distPref = fromFlag (haddockDistPref flags)
        doExes   = fromFlag (haddockExecutables flags)
        hsColour = fromFlag (haddockHscolour flags)
    when hsColour $ hscolour pkg_descr lbi suffixes defaultHscolourFlags {
      hscolourCSS         = haddockHscolourCss flags,
      hscolourExecutables = haddockExecutables flags,
      hscolourVerbosity   = haddockVerbosity flags
    }

    (confHaddock, _) <- requireProgram verbosity haddockProgram
                        (orLaterVersion (Version [0,6] [])) (withPrograms lbi)

    let tmpDir = buildDir lbi </> "tmp"
    createDirectoryIfMissingVerbose verbosity True tmpDir
    createDirectoryIfMissingVerbose verbosity True $
        haddockPref distPref pkg_descr
    initialBuildSteps distPref pkg_descr lbi verbosity suffixes

    setupMessage verbosity "Running Haddock for" (packageId pkg_descr)

    let replaceLitExts = map ( (tmpDir </>) . (`replaceExtension` "hs") )
    let showPkg    = display (packageId pkg_descr)
    let hoogle     = fromFlag (haddockHoogle flags)
        outputFlag | hoogle    = "--hoogle"
                   | otherwise = "--html"
    let Just version = programVersion confHaddock
    let have_src_hyperlink_flags = version >= Version [0,8] []
        isVersion2               = version >= Version [2,0] []

    when (hoogle && version > Version [2] []
                 && version < Version [2,2] []) $
      die $ "haddock 2.0 and 2.1 do not support the --hoogle flag."

    let mockFlags
          | isVersion2 = []
          | otherwise  = ["-D__HADDOCK__"]

    let mockAll bi = mapM_ (mockPP mockFlags bi tmpDir)

    let comp = compiler lbi
    let cssFileFlag = case flagToMaybe $ haddockCss flags of
                        Nothing -> []
                        Just cssFile -> ["--css=" ++ cssFile]
    let verboseFlags = if verbosity >= deafening then ["--verbose"] else []
    when (hsColour && not have_src_hyperlink_flags) $
         die "haddock --hyperlink-source requires Haddock version 0.8 or later"
    let linkToHscolour = if hsColour
            then ["--source-module=src/%{MODULE/./-}.html"
                 ,"--source-entity=src/%{MODULE/./-}.html#%{NAME}"]
            else []

    let htmlTemplate = fmap toPathTemplate $
                         flagToMaybe (haddockHtmlLocation flags)
    packageFlags <- do
      (packageFlags, warnings) <- haddockPackageFlags lbi htmlTemplate
      maybe (return ()) (warn verbosity) warnings
      return packageFlags

    when isVersion2 $ do
      strHadGhcVers <- rawSystemProgramStdout verbosity confHaddock ["--ghc-version"]
      let mHadGhcVers :: Maybe Version
          mHadGhcVers = simpleParse strHadGhcVers
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

    withLib pkg_descr () $ \lib -> do
        let bi = libBuildInfo lib
            modules = PD.exposedModules lib ++ otherModules bi
        inFiles <- getLibSourceFiles lbi lib
        unless isVersion2 $ mockAll bi inFiles
        let template = showPkg ++ "-haddock-prolog.txt"
            prolog | null (PD.description pkg_descr) = synopsis pkg_descr
                   | otherwise                       = PD.description pkg_descr
            subtitle | null (synopsis pkg_descr) = ""
                     | otherwise                 = ": " ++ synopsis pkg_descr
            titleComment | fromFlag (haddockInternal flags) = " (internal documentation)"
                         | otherwise                        = ""
        withTempFile distPref template $ \prologFileName prologFileHandle -> do
          hPutStrLn prologFileHandle prolog
          hClose prologFileHandle
          let targets
                | isVersion2 = map display modules
                | otherwise  = replaceLitExts inFiles
          let haddockFile = haddockPref distPref pkg_descr
                        </> haddockName pkg_descr
          -- FIX: replace w/ rawSystemProgramConf?
          let hideArgs | fromFlag (haddockInternal flags) = []
                       | otherwise = [ "--hide=" ++ display m
                                     | m <- otherModules bi ]
          let exportsFlags | fromFlag (haddockInternal flags) = ["--ignore-all-exports"]
                           | otherwise                        = []
          rawSystemProgram verbosity confHaddock
                  ([ outputFlag
                   , "--odir=" ++ haddockPref distPref pkg_descr
                   , "--title=" ++ showPkg ++ subtitle ++ titleComment
                   , "--dump-interface=" ++ haddockFile
                   , "--prologue=" ++ prologFileName ]
                   ++ packageName
                   ++ cssFileFlag
                   ++ linkToHscolour
                   ++ packageFlags
                   ++ verboseFlags
                   ++ hideArgs
                   ++ exportsFlags
                   ++ haddock2options bi (buildDir lbi)
                   ++ targets
                  )
          notice verbosity $ "Documentation created: "
                          ++ (haddockPref distPref pkg_descr </> "index.html")

    withExe pkg_descr $ \exe -> when doExes $ do
        let bi = buildInfo exe
            exeTargetDir = haddockPref distPref pkg_descr </> exeName exe
        createDirectoryIfMissingVerbose verbosity True exeTargetDir
        inFiles@(srcMainPath:_) <- getExeSourceFiles lbi exe
        mockAll bi inFiles
        let template = showPkg ++ "-haddock-prolog.txt"
            prolog | null (PD.description pkg_descr) = synopsis pkg_descr
                   | otherwise                    = PD.description pkg_descr
            titleComment | fromFlag (haddockInternal flags) = " (internal documentation)"
                         | otherwise                        = ""
        withTempFile distPref template $ \prologFileName prologFileHandle -> do
          hPutStrLn prologFileHandle prolog
          hClose prologFileHandle
          let targets
                | isVersion2 = srcMainPath : map display (otherModules bi)
                | otherwise = replaceLitExts inFiles
          let preprocessDir = buildDir lbi </> exeName exe </> exeName exe ++ "-tmp"
          let exportsFlags | fromFlag (haddockInternal flags) = ["--ignore-all-exports"]
                           | otherwise                        = []
          rawSystemProgram verbosity confHaddock
                  ([ outputFlag
                   , "--odir=" ++ exeTargetDir
                   , "--title=" ++ exeName exe ++ titleComment
                   , "--prologue=" ++ prologFileName ]
                   ++ linkToHscolour
                   ++ packageFlags
                   ++ verboseFlags
                   ++ exportsFlags
                   ++ haddock2options bi preprocessDir
                   ++ targets
                  )
          notice verbosity $ "Documentation created: "
                         ++ (exeTargetDir </> "index.html")

    removeDirectoryRecursive tmpDir
  where
        verbosity = fromFlag (haddockVerbosity flags)
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

haddockPackageFlags :: LocalBuildInfo
                    -> Maybe PathTemplate
                    -> IO ([String], Maybe String)
haddockPackageFlags lbi htmlTemplate = do
  let allPkgs = installedPkgs lbi
      directDeps = packageDeps lbi
  transitiveDeps <- case dependencyClosure allPkgs directDeps of
                    Left x -> return x
                    Right _ -> die "Can't find transitive deps for haddock"
  interfaces <- sequence
    [ case interfaceAndHtmlPath pkgid of
        Nothing -> return (pkgid, Nothing)
        Just (interface, html) -> do
          exists <- doesFileExist interface
          if exists
            then return (pkgid, Just (interface, html))
            else return (pkgid, Nothing)
    | pkgid <- map InstalledPackageInfo.package $ allPackages transitiveDeps ]

  let missing = [ pkgid | (pkgid, Nothing) <- interfaces ]
      warning = "The documentation for the following packages are not "
             ++ "installed. No links will be generated to these packages: "
             ++ intercalate ", " (map display missing)
      flags = [ "--read-interface="
             ++ (if null html then "" else html ++ ",") ++ interface
              | (_, Just (interface, html)) <- interfaces ]

  return (flags, if null missing then Nothing else Just warning)

  where
    interfaceAndHtmlPath :: PackageIdentifier -> Maybe (FilePath, FilePath)
    interfaceAndHtmlPath pkgId = do
      pkg <- PackageIndex.lookupPackageId (installedPkgs lbi) pkgId
      interface <- listToMaybe (InstalledPackageInfo.haddockInterfaces pkg)
      html <- case htmlTemplate of
        Nothing -> listToMaybe (InstalledPackageInfo.haddockHTMLs pkg)
        Just htmlPathTemplate -> Just (expandTemplateVars htmlPathTemplate)
      return (interface, html)

      where expandTemplateVars = fromPathTemplate . substPathTemplate env
            env = (PrefixVar, prefix (installDirTemplates lbi))
                : initialPathTemplateEnv pkgId (compilerId (compiler lbi))


ghcSimpleOptions :: LocalBuildInfo -> BuildInfo -> FilePath -> [String]
ghcSimpleOptions lbi bi mockDir
  =  ["-hide-all-packages"]
  ++ (concat [ ["-package", display pkg] | pkg <- packageDeps lbi ])
  ++ ["-i"]
  ++ hcOptions GHC bi
  ++ ["-i" ++ l | l <- nub (hsSourceDirs bi)]
  ++ ["-i" ++ autogenModulesDir lbi]
  ++ ["-i" ++ mockDir]
  ++ ["-I" ++ dir | dir <- PD.includeDirs bi]
  ++ ["-optP" ++ opt | opt <- cppOptions bi]
  ++ [ "-optP-include", "-optP"++ (autogenModulesDir lbi </> cppHeaderName) ]
  ++ ["-odir", mockDir]
  ++ ["-hidir", mockDir]
  ++ extensionsToFlags c (extensions bi)
  where c = compiler lbi


-- --------------------------------------------------------------------------
-- hscolour support

hscolour :: PackageDescription -> LocalBuildInfo -> [PPSuffixHandler] -> HscolourFlags -> IO ()
hscolour pkg_descr lbi suffixes flags = do
    let distPref = fromFlag $ hscolourDistPref flags
    (hscolourProg, _) <- requireProgram verbosity hscolourProgram
                         (orLaterVersion (Version [1,8] [])) (withPrograms lbi)

    createDirectoryIfMissingVerbose verbosity True $
        hscolourPref distPref pkg_descr
    preprocessSources pkg_descr lbi False verbosity suffixes

    setupMessage verbosity "Running hscolour for" (packageId pkg_descr)
    let moduleNameToHtmlFilePath mn =
          intercalate "-" (ModuleName.components mn) <.> "html"

    withLib pkg_descr () $ \lib -> when (isJust $ library pkg_descr) $ do
        let bi = libBuildInfo lib
            modules = PD.exposedModules lib ++ otherModules bi
            outputDir = hscolourPref distPref pkg_descr </> "src"
        createDirectoryIfMissingVerbose verbosity True outputDir
        copyCSS hscolourProg outputDir
        inFiles <- getLibSourceFiles lbi lib
        flip mapM_ (zip modules inFiles) $ \(mo, inFile) ->
            let outFile = outputDir </> moduleNameToHtmlFilePath mo
             in rawSystemProgram verbosity hscolourProg
                     ["-css", "-anchor", "-o" ++ outFile, inFile]

    withExe pkg_descr $ \exe -> when doExes $ do
        let bi = buildInfo exe
            modules = ModuleName.main : otherModules bi
            outputDir = hscolourPref distPref pkg_descr </> exeName exe </> "src"
        createDirectoryIfMissingVerbose verbosity True outputDir
        copyCSS hscolourProg outputDir
        inFiles <- getExeSourceFiles lbi exe
        flip mapM_ (zip modules inFiles) $ \(mo, inFile) ->
            let outFile = outputDir </> moduleNameToHtmlFilePath mo
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
        verbosity  = fromFlag (hscolourVerbosity flags)


getLibSourceFiles :: LocalBuildInfo -> Library -> IO [FilePath]
getLibSourceFiles lbi lib = sequence
  [ findFileWithExtension ["hs", "lhs"] (autogenModulesDir lbi: preprocessDir : hsSourceDirs bi)
      (ModuleName.toFilePath module_) >>= maybe (notFound module_) (return . normalise)
  | module_ <- modules ]
  where
    bi               = libBuildInfo lib
    modules          = PD.exposedModules lib ++ otherModules bi
    preprocessDir    = buildDir lbi
    notFound module_ = die $ "can't find source for module " ++ display module_

getExeSourceFiles :: LocalBuildInfo -> Executable -> IO [FilePath]
getExeSourceFiles lbi exe = do
  srcMainPath <- findFile (hsSourceDirs bi) (modulePath exe)
  moduleFiles <- sequence
    [ findFileWithExtension ["hs", "lhs"] (autogenModulesDir lbi : preprocessDir : hsSourceDirs bi)
        (ModuleName.toFilePath module_) >>= maybe (notFound module_) (return . normalise)
    | module_ <- modules ]
  return (srcMainPath : moduleFiles)
  where
    bi               = buildInfo exe
    modules          = otherModules bi
    preprocessDir    = buildDir lbi </> exeName exe </> exeName exe ++ "-tmp"
    notFound module_ = die $ "can't find source for module " ++ display module_
