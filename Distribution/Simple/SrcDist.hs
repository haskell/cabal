-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.SrcDist
-- Copyright   :  Simon Marlow 2004
-- 
-- Maintainer  :  Isaac Jones <ijones@syntaxpolice.org>
-- Stability   :  alpha
-- Portability :  portable
--
-- Implements the \"@.\/setup sdist@\" command, which creates a source
-- distribution for this package.  That is, packs up the source code
-- into a tarball.

{- Copyright (c) 2003-2004, Simon Marlow
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

-- NOTE: FIX: we don't have a great way of testing this module, since
-- we can't easily look inside a tarball once its created.

module Distribution.Simple.SrcDist (
	 sdist
        ,createArchive
        ,prepareTree
        ,tarBallName
        ,copyFileTo
  )  where

import Distribution.PackageDescription
         ( PackageDescription(..), BuildInfo(..), Executable(..), Library(..) )
import Distribution.PackageDescription.Check
import Distribution.Package (showPackageId, PackageIdentifier(pkgVersion), Package(..))
import Distribution.Version (Version(versionBranch), VersionRange(AnyVersion))
import Distribution.Simple.Utils
        ( createDirectoryIfMissingVerbose, readUTF8File, writeUTF8File
        , copyFiles, copyFileVerbose, findFile, findFileWithExtension, dotToSep
        , die, warn, notice, setupMessage, defaultPackageDesc )
import Distribution.Simple.Setup (SDistFlags(..), fromFlag)
import Distribution.Simple.PreProcess (PPSuffixHandler, ppSuffixes, preprocessSources)
import Distribution.Simple.LocalBuildInfo ( LocalBuildInfo(..) )
import Distribution.Simple.Program ( defaultProgramConfiguration, requireProgram,
                              rawSystemProgram, tarProgram )

import Control.Exception (finally)
import Control.Monad(when, unless)
import Data.Char (isSpace, toLower)
import Data.List (partition, isPrefixOf)
import Data.Maybe (catMaybes)
import System.Time (getClockTime, toCalendarTime, CalendarTime(..))
import System.Directory (doesFileExist, doesDirectoryExist,
                         removeDirectoryRecursive)
import Distribution.Verbosity (Verbosity)
import System.FilePath ((</>), takeDirectory, dropExtension, isAbsolute)

-- |Create a source distribution.
sdist :: PackageDescription -- ^information from the tarball
      -> Maybe LocalBuildInfo -- ^Information from configure
      -> SDistFlags -- ^verbosity & snapshot
      -> FilePath -- ^build prefix (temp dir)
      -> FilePath -- ^TargetPrefix
      -> [PPSuffixHandler]  -- ^ extra preprocessors (includes suffixes)
      -> IO ()
sdist pkg_descr_orig mb_lbi flags tmpDir targetPref pps = do
    let snapshot  = fromFlag (sDistSnapshot flags)
        verbosity = fromFlag (sDistVerbose flags)
    time <- getClockTime
    ct <- toCalendarTime time
    let date = ctYear ct*10000 + (fromEnum (ctMonth ct) + 1)*100 + ctDay ct
    let pkg_descr
          | snapshot  = updatePackage (updatePkgVersion
                          (updateVersionBranch (++ [date]))) pkg_descr_orig
          | otherwise = pkg_descr_orig

    -- do some QA
    printPackageProblems verbosity pkg_descr

    prepareTree pkg_descr verbosity mb_lbi snapshot tmpDir pps date
    createArchive pkg_descr verbosity mb_lbi tmpDir targetPref
    return ()
  where
    updatePackage f pd = pd { package = f (package pd) }
    updatePkgVersion f pkg = pkg { pkgVersion = f (pkgVersion pkg) }
    updateVersionBranch f v = v { versionBranch = f (versionBranch v) }

-- |Prepare a directory tree of source files.
prepareTree :: PackageDescription -- ^info from the cabal file
            -> Verbosity          -- ^verbosity
            -> Maybe LocalBuildInfo
            -> Bool               -- ^snapshot
            -> FilePath           -- ^source tree to populate
            -> [PPSuffixHandler]  -- ^extra preprocessors (includes suffixes)
            -> Int                -- ^date
            -> IO FilePath

prepareTree pkg_descr verbosity mb_lbi snapshot tmpDir pps date = do
  setupMessage verbosity "Building source dist for" (packageId pkg_descr)
  ex <- doesDirectoryExist tmpDir
  when ex (die $ "Source distribution already in place. please move: " ++ tmpDir)
  let targetDir = tmpDir </> (nameVersion pkg_descr)
  createDirectoryIfMissingVerbose verbosity True targetDir
  -- maybe move the library files into place
  withLib $ \ l ->
    prepareDir verbosity targetDir pps (exposedModules l) (libBuildInfo l)
  -- move the executables into place
  withExe $ \Executable { modulePath = mainPath, buildInfo = exeBi } -> do
    prepareDir verbosity targetDir pps [] exeBi
    srcMainFile <- do
      ppFile <- findFileWithExtension (ppSuffixes pps) (hsSourceDirs exeBi) (dropExtension mainPath)
      case ppFile of
        Nothing -> findFile (hsSourceDirs exeBi) mainPath
        Just pp -> return pp
    copyFileTo verbosity targetDir srcMainFile
  flip mapM_ (dataFiles pkg_descr) $ \ file -> do
    let dir = takeDirectory file
    createDirectoryIfMissingVerbose verbosity True (targetDir </> dir)
    copyFileVerbose verbosity file (targetDir </> file)

  when (not (null (licenseFile pkg_descr))) $
    copyFileTo verbosity targetDir (licenseFile pkg_descr)
  flip mapM_ (extraSrcFiles pkg_descr) $ \ fpath -> do
    copyFileTo verbosity targetDir fpath

  -- copy the install-include files
  withLib $ \ l -> do
    let lbi = libBuildInfo l
        relincdirs = "." : filter (not.isAbsolute) (includeDirs lbi)
    incs <- mapM (findInc relincdirs) (installIncludes lbi)
    flip mapM_ incs $ \(_,fpath) ->
       copyFileTo verbosity targetDir fpath

  -- we have some preprocessors specified, try to generate those files
  when (not (null pps)) $
    case mb_lbi of
      Just lbi -> preprocessSources pkg_descr (lbi { buildDir = targetDir }) 
                                    True verbosity pps
      Nothing -> warn verbosity
          "Cannot run preprocessors.  Run 'configure' command first."

  -- setup isn't listed in the description file.
  hsExists <- doesFileExist "Setup.hs"
  lhsExists <- doesFileExist "Setup.lhs"
  if hsExists then copyFileTo verbosity targetDir "Setup.hs"
    else if lhsExists then copyFileTo verbosity targetDir "Setup.lhs"
    else writeUTF8File (targetDir </> "Setup.hs") $ unlines [
                "import Distribution.Simple",
                "main = defaultMainWithHooks defaultUserHooks"]
  -- the description file itself
  descFile <- defaultPackageDesc verbosity
  let targetDescFile = targetDir </> descFile
  -- We could just writePackageDescription targetDescFile pkg_descr,
  -- but that would lose comments and formatting.
  if snapshot then do
      contents <- readUTF8File descFile
      writeUTF8File targetDescFile $
          unlines $ map (appendVersion date) $ lines $ contents
    else copyFileVerbose verbosity descFile targetDescFile
  return targetDir

  where

    appendVersion :: Int -> String -> String
    appendVersion n line
      | "version:" `isPrefixOf` map toLower line =
            trimTrailingSpace line ++ "." ++ show n
      | otherwise = line

    trimTrailingSpace :: String -> String
    trimTrailingSpace = reverse . dropWhile isSpace . reverse

    findInc [] f = die ("can't find include file " ++ f)
    findInc (d:ds) f = do
      let path = (d </> f)
      b <- doesFileExist path
      if b then return (f,path) else findInc ds f

    -- We have to deal with all libs and executables, so we have local
    -- versions of these functions that ignore the 'buildable' attribute:
    withLib action = maybe (return ()) action (library pkg_descr)
    withExe action = mapM_ action (executables pkg_descr)

-- |Create an archive from a tree of source files, and clean up the tree.
createArchive :: PackageDescription   -- ^info from cabal file
              -> Verbosity            -- ^verbosity
              -> Maybe LocalBuildInfo -- ^info from configure
              -> FilePath             -- ^source tree to archive
              -> FilePath             -- ^name of archive to create
              -> IO FilePath

createArchive pkg_descr verbosity mb_lbi tmpDir targetPref = do
  let tarBallFilePath = targetPref </> tarBallName pkg_descr

  (tarProg, _) <- requireProgram verbosity tarProgram AnyVersion
                    (maybe defaultProgramConfiguration withPrograms mb_lbi)

   -- Hmm: I could well be skating on thinner ice here by using the -C option (=> GNU tar-specific?)
   -- [The prev. solution used pipes and sub-command sequences to set up the paths correctly,
   -- which is problematic in a Windows setting.]
  rawSystemProgram verbosity tarProg
           ["-C", tmpDir, "-czf", tarBallFilePath, nameVersion pkg_descr]
      -- XXX this should be done back where tmpDir is made, not here
      `finally` removeDirectoryRecursive tmpDir
  notice verbosity $ "Source tarball created: " ++ tarBallFilePath
  return tarBallFilePath

-- |Move the sources into place based on buildInfo
prepareDir :: Verbosity -- ^verbosity
           -> FilePath  -- ^TargetPrefix
           -> [PPSuffixHandler]  -- ^ extra preprocessors (includes suffixes)
           -> [String]  -- ^Exposed modules
           -> BuildInfo
           -> IO ()
prepareDir verbosity inPref pps modules bi
    = do sources <- sequence
           [ let file = dotToSep module_
              in findFileWithExtension suffixes (hsSourceDirs bi) file
             >>= maybe (notFound module_) return
           | module_ <- modules ++ otherModules bi ]
         bootFiles <- sequence
           [ let file = dotToSep module_
              in findFileWithExtension ["hs-boot"] (hsSourceDirs bi) file
           | module_ <- modules ++ otherModules bi ]

         let allSources = sources ++ catMaybes bootFiles ++ cSources bi
         copyFiles verbosity inPref (zip (repeat []) allSources)

    where suffixes = ppSuffixes pps ++ ["hs", "lhs"]
          notFound m = die $ "Error: Could not find module: " ++ m
                          ++ " with any suffix: " ++ show suffixes

copyFileTo :: Verbosity -> FilePath -> FilePath -> IO ()
copyFileTo verbosity dir file = do
  let targetFile = dir </> file
  createDirectoryIfMissingVerbose verbosity True (takeDirectory targetFile)
  copyFileVerbose verbosity file targetFile

printPackageProblems :: Verbosity -> PackageDescription -> IO ()
printPackageProblems verbosity pkg_descr = do
  ioChecks      <- checkPackageFiles pkg_descr "."
  let pureChecks = checkPackage      pkg_descr
      isDistError (PackageDistSuspicious _) = False
      isDistError _                         = True
      (errors, warnings) = partition isDistError (pureChecks ++ ioChecks)
  unless (null errors) $
      notice verbosity $ "Distribution quality errors:\n"
                      ++ unlines (map explanation errors)
  unless (null warnings) $
      notice verbosity $ "Distribution quality warnings:\n"
    	              ++ unlines (map explanation warnings)
  unless (null errors) $
      notice verbosity
	"Note: the public hackage server would reject this package."

------------------------------------------------------------

-- |The file name of the tarball
tarBallName :: PackageDescription -> FilePath
tarBallName p = (nameVersion p) ++ ".tar.gz"

nameVersion :: PackageDescription -> String
nameVersion = showPackageId . packageId
