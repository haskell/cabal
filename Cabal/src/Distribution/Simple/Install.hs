{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

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
module Distribution.Simple.Install
  ( install
  , install_setupHooks
  , installFileGlob
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.CabalSpecVersion (CabalSpecVersion)

import Distribution.Types.ExecutableScope
import Distribution.Types.ForeignLib
import Distribution.Types.LocalBuildInfo
import Distribution.Types.PackageDescription
import Distribution.Types.TargetInfo
import Distribution.Types.UnqualComponentName

import Distribution.Package
import Distribution.PackageDescription
import Distribution.Simple.BuildPaths (haddockName, haddockPref)
import Distribution.Simple.BuildTarget
import Distribution.Simple.Compiler
  ( CompilerFlavor (..)
  , compilerFlavor
  )
import Distribution.Simple.Glob (matchDirFileGlob)
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup.Config
import Distribution.Simple.Setup.Copy
  ( CopyFlags (..)
  )
import Distribution.Simple.Setup.Haddock
  ( HaddockTarget (ForDevelopment)
  )
import Distribution.Simple.SetupHooks.Internal
  ( InstallHooks (..)
  )
import qualified Distribution.Simple.SetupHooks.Internal as SetupHooks
import Distribution.Simple.Utils
  ( createDirectoryIfMissingVerbose
  , dieWithException
  , info
  , installDirectoryContents
  , installOrdinaryFile
  , isAbsoluteOnAnyPlatform
  , isInSearchPath
  , noticeNoWrap
  , warn
  )
import Distribution.Utils.Path

import Distribution.Compat.Graph (IsNode (..))
import Distribution.Simple.Errors
import qualified Distribution.Simple.GHC as GHC
import qualified Distribution.Simple.GHCJS as GHCJS
import qualified Distribution.Simple.HaskellSuite as HaskellSuite
import Distribution.Simple.Setup.Common
import qualified Distribution.Simple.UHC as UHC

import System.Directory
  ( doesDirectoryExist
  , doesFileExist
  )
import System.FilePath
  ( takeDirectory
  , takeFileName
  )

import Distribution.Pretty
  ( prettyShow
  )
import Distribution.Verbosity

-- | Perform the \"@.\/setup install@\" and \"@.\/setup copy@\"
--  actions.  Move files into place based on the prefix argument.
--
--  This does NOT register libraries, you should call 'register'
--  to do that.
install
  :: PackageDescription
  -- ^ information from the .cabal file
  -> LocalBuildInfo
  -- ^ information from the configure step
  -> CopyFlags
  -- ^ flags sent to copy or install
  -> IO ()
install = install_setupHooks SetupHooks.noInstallHooks

install_setupHooks
  :: InstallHooks
  -> PackageDescription
  -- ^ information from the .cabal file
  -> LocalBuildInfo
  -- ^ information from the configure step
  -> CopyFlags
  -- ^ flags sent to copy or install
  -> IO ()
install_setupHooks
  (InstallHooks{installComponentHook})
  pkg_descr
  lbi
  flags = do
    checkHasLibsOrExes
    targets <- readTargetInfos verbosity pkg_descr lbi (copyTargets flags)

    copyPackage verbosity pkg_descr lbi distPref copydest

    -- It's not necessary to do these in build-order, but it's harmless
    withNeededTargetsInBuildOrder' pkg_descr lbi (map nodeKey targets) $ \target -> do
      let comp = targetComponent target
          clbi = targetCLBI target
      copyComponent verbosity pkg_descr lbi comp clbi copydest
      for_ installComponentHook $ \instAction ->
        let inputs =
              SetupHooks.InstallComponentInputs
                { copyFlags = flags
                , localBuildInfo = lbi
                , targetInfo = target
                }
         in instAction inputs
    where
      common = copyCommonFlags flags
      distPref = fromFlag $ setupDistPref common
      verbosity = fromFlag $ setupVerbosity common
      copydest = fromFlag (copyDest flags)

      checkHasLibsOrExes =
        unless (hasLibs pkg_descr || hasForeignLibs pkg_descr || hasExes pkg_descr) $
          warn verbosity "No executables and no library found. Nothing to do."

-- | Copy package global files.
copyPackage
  :: Verbosity
  -> PackageDescription
  -> LocalBuildInfo
  -> SymbolicPath Pkg (Dir Dist)
  -> CopyDest
  -> IO ()
copyPackage verbosity pkg_descr lbi distPref copydest = do
  let
    -- This is a bit of a hack, to handle files which are not
    -- per-component (data files and Haddock files.)
    InstallDirs
      { datadir = dataPref
      , docdir = docPref
      , htmldir = htmlPref
      , haddockdir = interfacePref
      } = absoluteInstallCommandDirs pkg_descr lbi (localUnitId lbi) copydest
    mbWorkDir = mbWorkDirLBI lbi
    i = interpretSymbolicPath mbWorkDir -- See Note [Symbolic paths] in Distribution.Utils.Path

  -- Install (package-global) data files
  installDataFiles verbosity mbWorkDir pkg_descr $ makeSymbolicPath dataPref

  -- Install (package-global) Haddock files
  -- TODO: these should be done per-library
  docExists <- doesDirectoryExist $ i $ haddockPref ForDevelopment distPref pkg_descr
  info
    verbosity
    ( "directory "
        ++ getSymbolicPath (haddockPref ForDevelopment distPref pkg_descr)
        ++ " does exist: "
        ++ show docExists
    )

  -- TODO: this is a bit questionable, Haddock files really should
  -- be per library (when there are convenience libraries.)
  when docExists $ do
    createDirectoryIfMissingVerbose verbosity True htmlPref
    installDirectoryContents
      verbosity
      (i $ haddockPref ForDevelopment distPref pkg_descr)
      htmlPref
    -- setPermissionsRecursive [Read] htmlPref
    -- The haddock interface file actually already got installed
    -- in the recursive copy, but now we install it where we actually
    -- want it to be (normally the same place). We could remove the
    -- copy in htmlPref first.
    let haddockInterfaceFileSrc =
          haddockPref ForDevelopment distPref pkg_descr
            </> makeRelativePathEx (haddockName pkg_descr)
        haddockInterfaceFileDest = interfacePref </> haddockName pkg_descr
    -- We only generate the haddock interface file for libs, So if the
    -- package consists only of executables there will not be one:
    exists <- doesFileExist $ i haddockInterfaceFileSrc
    when exists $ do
      createDirectoryIfMissingVerbose verbosity True interfacePref
      installOrdinaryFile
        verbosity
        (i haddockInterfaceFileSrc)
        haddockInterfaceFileDest

  let lfiles = licenseFiles pkg_descr
  unless (null lfiles) $ do
    createDirectoryIfMissingVerbose verbosity True docPref
    for_ lfiles $ \lfile -> do
      installOrdinaryFile
        verbosity
        (i lfile)
        (docPref </> takeFileName (getSymbolicPath lfile))

-- | Copy files associated with a component.
copyComponent
  :: Verbosity
  -> PackageDescription
  -> LocalBuildInfo
  -> Component
  -> ComponentLocalBuildInfo
  -> CopyDest
  -> IO ()
copyComponent verbosity pkg_descr lbi (CLib lib) clbi copydest = do
  let InstallDirs
        { libdir = libPref
        , dynlibdir = dynlibPref
        , includedir = incPref
        } = absoluteInstallCommandDirs pkg_descr lbi (componentUnitId clbi) copydest
      buildPref = interpretSymbolicPathLBI lbi $ componentBuildDir lbi clbi

  case libName lib of
    LMainLibName -> noticeNoWrap verbosity ("Installing library in " ++ libPref)
    LSubLibName n -> noticeNoWrap verbosity ("Installing internal library " ++ prettyShow n ++ " in " ++ libPref)

  -- install include files for all compilers - they may be needed to compile
  -- haskell files (using the CPP extension)
  installIncludeFiles verbosity (libBuildInfo lib) lbi buildPref incPref

  case compilerFlavor (compiler lbi) of
    GHC -> GHC.installLib verbosity lbi libPref dynlibPref buildPref pkg_descr lib clbi
    GHCJS -> GHCJS.installLib verbosity lbi libPref dynlibPref buildPref pkg_descr lib clbi
    UHC -> UHC.installLib verbosity lbi libPref dynlibPref buildPref pkg_descr lib clbi
    HaskellSuite _ ->
      HaskellSuite.installLib
        verbosity
        lbi
        libPref
        dynlibPref
        buildPref
        pkg_descr
        lib
        clbi
    _ ->
      dieWithException verbosity $ CompilerNotInstalled (compilerFlavor (compiler lbi))
copyComponent verbosity pkg_descr lbi (CFLib flib) clbi copydest = do
  let InstallDirs
        { flibdir = flibPref
        , includedir = incPref
        } = absoluteComponentInstallDirs pkg_descr lbi (componentUnitId clbi) copydest
      buildPref = interpretSymbolicPathLBI lbi $ componentBuildDir lbi clbi

  noticeNoWrap verbosity ("Installing foreign library " ++ unUnqualComponentName (foreignLibName flib) ++ " in " ++ flibPref)
  installIncludeFiles verbosity (foreignLibBuildInfo flib) lbi buildPref incPref

  case compilerFlavor (compiler lbi) of
    GHC -> GHC.installFLib verbosity lbi flibPref buildPref pkg_descr flib
    GHCJS -> GHCJS.installFLib verbosity lbi flibPref buildPref pkg_descr flib
    _ -> dieWithException verbosity $ CompilerNotInstalled (compilerFlavor (compiler lbi))
copyComponent verbosity pkg_descr lbi (CExe exe) clbi copydest = do
  let installDirs = absoluteComponentInstallDirs pkg_descr lbi (componentUnitId clbi) copydest
      -- the installers know how to find the actual location of the
      -- binaries
      buildPref = interpretSymbolicPathLBI lbi $ buildDir lbi
      uid = componentUnitId clbi
      pkgid = packageId pkg_descr
      binPref
        | ExecutablePrivate <- exeScope exe = libexecdir installDirs
        | otherwise = bindir installDirs
      progPrefixPref = substPathTemplate pkgid lbi uid (progPrefix lbi)
      progSuffixPref = substPathTemplate pkgid lbi uid (progSuffix lbi)
      progFix = (progPrefixPref, progSuffixPref)
  noticeNoWrap
    verbosity
    ( "Installing executable "
        ++ prettyShow (exeName exe)
        ++ " in "
        ++ binPref
    )
  inPath <- isInSearchPath binPref
  when (not inPath) $
    warn
      verbosity
      ( "The directory "
          ++ binPref
          ++ " is not in the system search path."
      )
  case compilerFlavor (compiler lbi) of
    GHC -> GHC.installExe verbosity lbi binPref buildPref progFix pkg_descr exe
    GHCJS -> GHCJS.installExe verbosity lbi binPref buildPref progFix pkg_descr exe
    UHC -> return ()
    HaskellSuite{} -> return ()
    _ ->
      dieWithException verbosity $ CompilerNotInstalled (compilerFlavor (compiler lbi))

-- Nothing to do for benchmark/testsuite
copyComponent _ _ _ (CBench _) _ _ = return ()
copyComponent _ _ _ (CTest _) _ _ = return ()

-- | Install the files listed in data-files
installDataFiles
  :: Verbosity
  -> Maybe (SymbolicPath CWD (Dir Pkg))
  -> PackageDescription
  -> SymbolicPath Pkg (Dir DataDir)
  -> IO ()
installDataFiles verbosity mbWorkDir pkg_descr destDataDir =
  traverse_
    (installFileGlob verbosity (specVersion pkg_descr) mbWorkDir (srcDataDir, destDataDir))
    (dataFiles pkg_descr)
  where
    srcDataDirRaw = getSymbolicPath $ dataDir pkg_descr
    srcDataDir :: Maybe (SymbolicPath CWD (Dir DataDir))
    srcDataDir
      | null srcDataDirRaw =
          Nothing
      | isAbsoluteOnAnyPlatform srcDataDirRaw =
          Just $ makeSymbolicPath srcDataDirRaw
      | otherwise =
          Just $ fromMaybe sameDirectory mbWorkDir </> makeRelativePathEx srcDataDirRaw

-- | Install the files specified by the given glob pattern.
installFileGlob
  :: Verbosity
  -> CabalSpecVersion
  -> Maybe (SymbolicPath CWD (Dir Pkg))
  -> (Maybe (SymbolicPath CWD (Dir DataDir)), SymbolicPath Pkg (Dir DataDir))
  -- ^ @(src_dir, dest_dir)@
  -> RelativePath DataDir File
  -- ^ file glob pattern
  -> IO ()
installFileGlob verbosity spec_version mbWorkDir (srcDir, destDir) glob = do
  files <- matchDirFileGlob verbosity spec_version srcDir glob
  for_ files $ \file' -> do
    let src = getSymbolicPath (fromMaybe sameDirectory srcDir </> file')
        dst = interpretSymbolicPath mbWorkDir (destDir </> file')
    createDirectoryIfMissingVerbose verbosity True (takeDirectory dst)
    installOrdinaryFile verbosity src dst

-- | Install the files listed in install-includes for a library
installIncludeFiles :: Verbosity -> BuildInfo -> LocalBuildInfo -> FilePath -> FilePath -> IO ()
installIncludeFiles verbosity libBi lbi buildPref destIncludeDir = do
  let relincdirs = sameDirectory : mapMaybe symbolicPathRelative_maybe (includeDirs libBi)
      incdirs =
        [ root </> getSymbolicPath dir
        | -- NB: both baseDir and buildPref are already interpreted,
        -- so we don't need to interpret these paths in the call to findInc.
        dir <- relincdirs
        , root <- [baseDir lbi, buildPref]
        ]
  incs <- traverse (findInc incdirs . getSymbolicPath) (installIncludes libBi)
  sequence_
    [ do
      createDirectoryIfMissingVerbose verbosity True destDir
      installOrdinaryFile verbosity srcFile destFile
    | (relFile, srcFile) <- incs
    , let destFile = destIncludeDir </> relFile
          destDir = takeDirectory destFile
    ]
  where
    baseDir lbi' = packageRoot $ configCommonFlags $ configFlags lbi'
    findInc [] file = dieWithException verbosity $ CantFindIncludeFile file
    findInc (dir : dirs) file = do
      let path = dir </> file
      exists <- doesFileExist path
      if exists then return (file, path) else findInc dirs file
