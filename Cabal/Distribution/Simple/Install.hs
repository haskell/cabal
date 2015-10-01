-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Install
-- Copyright   :  Isaac Jones 2003-2004
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This is the entry point into installing a built package. Performs the
-- \"@.\/setup install@\" and \"@.\/setup copy@\" actions. It moves files into
-- place based on the prefix argument. It does the generic bits and then calls
-- compiler-specific functions to do the rest.

module Distribution.Simple.Install (
        install,
  ) where

import Distribution.PackageDescription
import Distribution.Package (Package(..))
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.BuildPaths (haddockName, haddockPref)
import Distribution.Simple.Utils
         ( createDirectoryIfMissingVerbose
         , installDirectoryContents, installOrdinaryFile, isInSearchPath
         , die, info, notice, warn, matchDirFileGlob )
import Distribution.Simple.Compiler
         ( CompilerFlavor(..), compilerFlavor )
import Distribution.Simple.Setup (CopyFlags(..), fromFlag)

import qualified Distribution.Simple.GHC   as GHC
import qualified Distribution.Simple.GHCJS as GHCJS
import qualified Distribution.Simple.JHC   as JHC
import qualified Distribution.Simple.LHC   as LHC
import qualified Distribution.Simple.UHC   as UHC
import qualified Distribution.Simple.HaskellSuite as HaskellSuite

import Control.Monad (when, unless)
import System.Directory
         ( doesDirectoryExist, doesFileExist )
import System.FilePath
         ( takeFileName, takeDirectory, (</>), isAbsolute )

import Distribution.Verbosity
import Distribution.Text
         ( display )

-- |Perform the \"@.\/setup install@\" and \"@.\/setup copy@\"
-- actions.  Move files into place based on the prefix argument.

install :: PackageDescription -- ^information from the .cabal file
        -> LocalBuildInfo -- ^information from the configure step
        -> CopyFlags -- ^flags sent to copy or install
        -> IO ()
install pkg_descr lbi flags = do
  let distPref  = fromFlag (copyDistPref flags)
      verbosity = fromFlag (copyVerbosity flags)
      copydest  = fromFlag (copyDest flags)
      installDirs@(InstallDirs {
         bindir     = binPref,
         libdir     = libPref,
--         dynlibdir  = dynlibPref, --see TODO below
         datadir    = dataPref,
         docdir     = docPref,
         htmldir    = htmlPref,
         haddockdir = interfacePref,
         includedir = incPref})
             -- Using the library clbi for binPref is a hack;
             -- binPref should be computed per executable
             = absoluteInstallDirs pkg_descr lbi copydest

      --TODO: decide if we need the user to be able to control the libdir
      -- for shared libs independently of the one for static libs. If so
      -- it should also have a flag in the command line UI
      -- For the moment use dynlibdir = libdir
      dynlibPref = libPref
      progPrefixPref = substPathTemplate (packageId pkg_descr) lbi (progPrefix lbi)
      progSuffixPref = substPathTemplate (packageId pkg_descr) lbi (progSuffix lbi)

  unless (hasLibs pkg_descr || hasExes pkg_descr) $
      die "No executables and no library found. Nothing to do."
  docExists <- doesDirectoryExist $ haddockPref distPref pkg_descr
  info verbosity ("directory " ++ haddockPref distPref pkg_descr ++
                  " does exist: " ++ show docExists)

  installDataFiles verbosity pkg_descr dataPref

  when docExists $ do
      createDirectoryIfMissingVerbose verbosity True htmlPref
      installDirectoryContents verbosity
          (haddockPref distPref pkg_descr) htmlPref
      -- setPermissionsRecursive [Read] htmlPref
      -- The haddock interface file actually already got installed
      -- in the recursive copy, but now we install it where we actually
      -- want it to be (normally the same place). We could remove the
      -- copy in htmlPref first.
      let haddockInterfaceFileSrc  = haddockPref distPref pkg_descr
                                                   </> haddockName pkg_descr
          haddockInterfaceFileDest = interfacePref </> haddockName pkg_descr
      -- We only generate the haddock interface file for libs, So if the
      -- package consists only of executables there will not be one:
      exists <- doesFileExist haddockInterfaceFileSrc
      when exists $ do
        createDirectoryIfMissingVerbose verbosity True interfacePref
        installOrdinaryFile verbosity haddockInterfaceFileSrc
                                      haddockInterfaceFileDest

  let lfiles = licenseFiles pkg_descr
  unless (null lfiles) $ do
    createDirectoryIfMissingVerbose verbosity True docPref
    sequence_
      [ installOrdinaryFile verbosity lfile (docPref </> takeFileName lfile)
      | lfile <- lfiles ]

  let buildPref = buildDir lbi
  when (hasLibs pkg_descr) $
    notice verbosity ("Installing library in " ++ libPref)
  when (hasExes pkg_descr) $ do
    notice verbosity ("Installing executable(s) in " ++ binPref)
    inPath <- isInSearchPath binPref
    when (not inPath) $
      warn verbosity ("The directory " ++ binPref
                      ++ " is not in the system search path.")

  -- install include files for all compilers - they may be needed to compile
  -- haskell files (using the CPP extension)
  --
  when (hasLibs pkg_descr) $ installIncludeFiles verbosity pkg_descr incPref

  withLibLBI pkg_descr lbi $
    case compilerFlavor (compiler lbi) of
      GHC   -> GHC.installLib   verbosity lbi libPref dynlibPref buildPref pkg_descr
      GHCJS -> GHCJS.installLib verbosity lbi libPref dynlibPref buildPref pkg_descr
      LHC   -> LHC.installLib   verbosity lbi libPref dynlibPref buildPref pkg_descr
      JHC   -> JHC.installLib   verbosity lbi libPref dynlibPref buildPref pkg_descr
      UHC   -> UHC.installLib   verbosity lbi libPref dynlibPref buildPref pkg_descr
      HaskellSuite _ -> HaskellSuite.installLib
                                verbosity lbi libPref dynlibPref buildPref pkg_descr
      _ -> \_ _ -> die $ "installing with "
                      ++ display (compilerFlavor (compiler lbi))
                      ++ " is not implemented"

  withExe pkg_descr $
    case compilerFlavor (compiler lbi) of
      GHC   -> GHC.installExe   verbosity lbi installDirs buildPref (progPrefixPref, progSuffixPref) pkg_descr
      GHCJS -> GHCJS.installExe verbosity lbi installDirs buildPref (progPrefixPref, progSuffixPref) pkg_descr
      LHC   -> LHC.installExe   verbosity lbi installDirs buildPref (progPrefixPref, progSuffixPref) pkg_descr
      JHC   -> JHC.installExe   verbosity binPref buildPref (progPrefixPref, progSuffixPref) pkg_descr
      UHC   -> \_ -> return ()
      HaskellSuite {} -> \_ -> return ()
      _ -> \_ -> die $ "installing with "
                    ++ display (compilerFlavor (compiler lbi))
                    ++ " is not implemented"
  -- register step should be performed by caller.

-- | Install the files listed in data-files
--
installDataFiles :: Verbosity -> PackageDescription -> FilePath -> IO ()
installDataFiles verbosity pkg_descr destDataDir =
  flip mapM_ (dataFiles pkg_descr) $ \ file -> do
    let srcDataDir = dataDir pkg_descr
    files <- matchDirFileGlob srcDataDir file
    let dir = takeDirectory file
    createDirectoryIfMissingVerbose verbosity True (destDataDir </> dir)
    sequence_ [ installOrdinaryFile verbosity (srcDataDir  </> file')
                                              (destDataDir </> file')
              | file' <- files ]

-- | Install the files listed in install-includes
--
installIncludeFiles :: Verbosity -> PackageDescription -> FilePath -> IO ()
installIncludeFiles verbosity
  PackageDescription { library = Just lib } destIncludeDir = do

  incs <- mapM (findInc relincdirs) (installIncludes lbi)
  sequence_
    [ do createDirectoryIfMissingVerbose verbosity True destDir
         installOrdinaryFile verbosity srcFile destFile
    | (relFile, srcFile) <- incs
    , let destFile = destIncludeDir </> relFile
          destDir  = takeDirectory destFile ]
  where
   relincdirs = "." : filter (not.isAbsolute) (includeDirs lbi)
   lbi = libBuildInfo lib

   findInc []         file = die ("can't find include file " ++ file)
   findInc (dir:dirs) file = do
     let path = dir </> file
     exists <- doesFileExist path
     if exists then return (file, path) else findInc dirs file
installIncludeFiles _ _ _ = die "installIncludeFiles: Can't happen?"
