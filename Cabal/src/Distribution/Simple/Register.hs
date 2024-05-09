{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

-----------------------------------------------------------------------------

-- |
-- Module      :  Distribution.Simple.Register
-- Copyright   :  Isaac Jones 2003-2004
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This module deals with registering and unregistering packages. There are a
-- couple ways it can do this, one is to do it directly. Another is to generate
-- a script that can be run later to do it. The idea here being that the user
-- is shielded from the details of what command to use for package registration
-- for a particular compiler. In practice this aspect was not especially
-- popular so we also provide a way to simply generate the package registration
-- file which then must be manually passed to @ghc-pkg@. It is possible to
-- generate registration information for where the package is to be installed,
-- or alternatively to register the package in place in the build tree. The
-- latter is occasionally handy, and will become more important when we try to
-- build multi-package systems.
--
-- This module does not delegate anything to the per-compiler modules but just
-- mixes it all in this module, which is rather unsatisfactory. The script
-- generation and the unregister feature are not well used or tested.
module Distribution.Simple.Register
  ( register
  , unregister
  , internalPackageDBPath
  , initPackageDB
  , doesPackageDBExist
  , createPackageDB
  , deletePackageDB
  , abiHash
  , invokeHcPkg
  , registerPackage
  , HcPkg.RegisterOptions (..)
  , HcPkg.defaultRegisterOptions
  , generateRegistrationInfo
  , inplaceInstalledPackageInfo
  , absoluteInstalledPackageInfo
  , generalInstalledPackageInfo
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Types.ComponentLocalBuildInfo
import Distribution.Types.LocalBuildInfo
import Distribution.Types.TargetInfo

import Distribution.Simple.BuildPaths
import Distribution.Simple.BuildTarget
import Distribution.Simple.LocalBuildInfo

import qualified Distribution.Simple.GHC as GHC
import qualified Distribution.Simple.GHCJS as GHCJS
import qualified Distribution.Simple.HaskellSuite as HaskellSuite
import qualified Distribution.Simple.PackageIndex as Index
import qualified Distribution.Simple.UHC as UHC

import Distribution.Backpack.DescribeUnitId
import Distribution.Compat.Graph (IsNode (nodeKey))
import Distribution.InstalledPackageInfo (InstalledPackageInfo)
import qualified Distribution.InstalledPackageInfo as IPI
import Distribution.License (licenseFromSPDX, licenseToSPDX)
import Distribution.Package
import Distribution.PackageDescription
import Distribution.Pretty
import Distribution.Simple.Compiler
import Distribution.Simple.Errors
import Distribution.Simple.Flag
import Distribution.Simple.Program
import qualified Distribution.Simple.Program.HcPkg as HcPkg
import Distribution.Simple.Program.Script
import Distribution.Simple.Setup.Common
import Distribution.Simple.Setup.Register
import Distribution.Simple.Utils
import Distribution.System
import Distribution.Utils.MapAccum
import Distribution.Utils.Path
import Distribution.Verbosity as Verbosity
import Distribution.Version
import System.Directory
import System.FilePath (isAbsolute)

import qualified Data.ByteString.Lazy.Char8 as BS.Char8

-- -----------------------------------------------------------------------------
-- Registration

register
  :: PackageDescription
  -> LocalBuildInfo
  -> RegisterFlags
  -- ^ Install in the user's database?; verbose
  -> IO ()
register pkg_descr lbi0 flags = do
  -- Duncan originally asked for us to not register/install files
  -- when there was no public library.  But with per-component
  -- configure, we legitimately need to install internal libraries
  -- so that we can get them.  So just unconditionally install.
  let verbosity = fromFlag $ registerVerbosity flags
  targets <- readTargetInfos verbosity pkg_descr lbi0 $ registerTargets flags

  -- It's important to register in build order, because ghc-pkg
  -- will complain if a dependency is not registered.
  let componentsToRegister =
        neededTargetsInBuildOrder' pkg_descr lbi0 (map nodeKey targets)

  (_, ipi_mbs) <-
    mapAccumM `flip` installedPkgs lbi0 `flip` componentsToRegister $ \index tgt ->
      case targetComponent tgt of
        CLib lib -> do
          let clbi = targetCLBI tgt
              lbi = lbi0{installedPkgs = index}
          ipi <- generateOne pkg_descr lib lbi clbi flags
          return (Index.insert ipi index, Just ipi)
        _ -> return (index, Nothing)

  registerAll pkg_descr lbi0 flags (catMaybes ipi_mbs)

generateOne
  :: PackageDescription
  -> Library
  -> LocalBuildInfo
  -> ComponentLocalBuildInfo
  -> RegisterFlags
  -> IO InstalledPackageInfo
generateOne pkg lib lbi clbi regFlags =
  do
    absPackageDBs <- absolutePackageDBPaths mbWorkDir packageDbs
    installedPkgInfo <-
      generateRegistrationInfo
        verbosity
        pkg
        lib
        lbi
        clbi
        inplace
        reloc
        distPref
        (registrationPackageDB absPackageDBs)
    info verbosity (IPI.showInstalledPackageInfo installedPkgInfo)
    return installedPkgInfo
  where
    common = registerCommonFlags regFlags
    inplace = fromFlag (regInPlace regFlags)
    reloc = relocatable lbi
    -- FIXME: there's really no guarantee this will work.
    -- registering into a totally different db stack can
    -- fail if dependencies cannot be satisfied.
    packageDbs =
      nub $
        withPackageDB lbi
          ++ maybeToList (flagToMaybe (regPackageDB regFlags))
    distPref = fromFlag $ setupDistPref common
    verbosity = fromFlag $ setupVerbosity common
    mbWorkDir = flagToMaybe $ setupWorkingDir common

registerAll
  :: PackageDescription
  -> LocalBuildInfo
  -> RegisterFlags
  -> [InstalledPackageInfo]
  -> IO ()
registerAll pkg lbi regFlags ipis =
  do
    when (Just True == flagToMaybe (regPrintId regFlags)) $ do
      for_ ipis $ \installedPkgInfo ->
        -- Only print the public library's IPI
        when
          ( packageId installedPkgInfo == packageId pkg
              && IPI.sourceLibName installedPkgInfo == LMainLibName
          )
          $ putStrLn (prettyShow (IPI.installedUnitId installedPkgInfo))

    -- Three different modes:
    case () of
      _
        | modeGenerateRegFile -> writeRegistrationFileOrDirectory
        | modeGenerateRegScript -> writeRegisterScript
        | otherwise -> do
            for_ ipis $ \ipi -> do
              setupMessage'
                verbosity
                "Registering"
                (packageId pkg)
                (CLibName (IPI.sourceLibName ipi))
                (Just (IPI.instantiatedWith ipi))
              registerPackage
                verbosity
                (compiler lbi)
                (withPrograms lbi)
                (mbWorkDirLBI lbi)
                packageDbs
                ipi
                HcPkg.defaultRegisterOptions
  where
    modeGenerateRegFile = isJust (flagToMaybe (regGenPkgConf regFlags))
    regFile =
      fromMaybe
        (prettyShow (packageId pkg) <.> "conf")
        (fromFlag (regGenPkgConf regFlags))

    modeGenerateRegScript = fromFlag (regGenScript regFlags)

    -- FIXME: there's really no guarantee this will work.
    -- registering into a totally different db stack can
    -- fail if dependencies cannot be satisfied.
    packageDbs =
      nub $
        withPackageDB lbi
          ++ maybeToList (flagToMaybe (regPackageDB regFlags))
    common = registerCommonFlags regFlags
    verbosity = fromFlag (setupVerbosity common)
    mbWorkDir = mbWorkDirLBI lbi

    writeRegistrationFileOrDirectory = do
      -- Handles overwriting both directory and file
      deletePackageDB regFile
      case ipis of
        [installedPkgInfo] -> do
          info verbosity ("Creating package registration file: " ++ regFile)
          writeUTF8File regFile (IPI.showInstalledPackageInfo installedPkgInfo)
        _ -> do
          info verbosity ("Creating package registration directory: " ++ regFile)
          createDirectory regFile
          let num_ipis = length ipis
              lpad m xs = replicate (m - length ys) '0' ++ ys
                where
                  ys = take m xs
              number i = lpad (length (show num_ipis)) (show i)
          for_ (zip ([1 ..] :: [Int]) ipis) $ \(i, installedPkgInfo) ->
            writeUTF8File
              (regFile </> (number i ++ "-" ++ prettyShow (IPI.installedUnitId installedPkgInfo)))
              (IPI.showInstalledPackageInfo installedPkgInfo)

    writeRegisterScript =
      case compilerFlavor (compiler lbi) of
        UHC -> notice verbosity "Registration scripts not needed for uhc"
        _ ->
          withHcPkg
            verbosity
            "Registration scripts are not implemented for this compiler"
            (compiler lbi)
            (withPrograms lbi)
            (writeHcPkgRegisterScript verbosity mbWorkDir ipis packageDbs)

generateRegistrationInfo
  :: Verbosity
  -> PackageDescription
  -> Library
  -> LocalBuildInfo
  -> ComponentLocalBuildInfo
  -> Bool
  -> Bool
  -> SymbolicPath Pkg (Dir Dist)
  -> PackageDB
  -> IO InstalledPackageInfo
generateRegistrationInfo verbosity pkg lib lbi clbi inplace reloc distPref packageDb = do
  inplaceDir <- absoluteWorkingDirLBI lbi
  installedPkgInfo <-
    if inplace
      then -- NB: With an inplace installation, the user may run './Setup
      -- build' to update the library files, without reregistering.
      -- In this case, it is critical that the ABI hash not flip.

        return
          ( inplaceInstalledPackageInfo
              inplaceDir
              distPref
              pkg
              (mkAbiHash "inplace")
              lib
              lbi
              clbi
          )
      else do
        abi_hash <- abiHash verbosity pkg inplaceDir distPref lbi lib clbi
        if reloc
          then
            relocRegistrationInfo
              verbosity
              pkg
              lib
              lbi
              clbi
              abi_hash
              packageDb
          else
            return
              ( absoluteInstalledPackageInfo
                  pkg
                  abi_hash
                  lib
                  lbi
                  clbi
              )

  return installedPkgInfo

-- | Compute the 'AbiHash' of a library that we built inplace.
abiHash
  :: Verbosity
  -> PackageDescription
  -> FilePath
  -> SymbolicPath Pkg (Dir Dist)
  -> LocalBuildInfo
  -> Library
  -> ComponentLocalBuildInfo
  -> IO AbiHash
abiHash verbosity pkg inplaceDir distPref lbi lib clbi =
  case compilerFlavor comp of
    GHC -> do
      fmap mkAbiHash $ GHC.libAbiHash verbosity pkg lbi' lib clbi
    GHCJS -> do
      fmap mkAbiHash $ GHCJS.libAbiHash verbosity pkg lbi' lib clbi
    _ -> return (mkAbiHash "")
  where
    comp = compiler lbi
    lbi' =
      lbi
        { withPackageDB =
            withPackageDB lbi
              ++ [SpecificPackageDB (inplaceDir </> getSymbolicPath (internalPackageDBPath lbi distPref))]
        }

relocRegistrationInfo
  :: Verbosity
  -> PackageDescription
  -> Library
  -> LocalBuildInfo
  -> ComponentLocalBuildInfo
  -> AbiHash
  -> PackageDB
  -> IO InstalledPackageInfo
relocRegistrationInfo verbosity pkg lib lbi clbi abi_hash packageDb =
  case (compilerFlavor (compiler lbi)) of
    GHC -> do
      fs <- GHC.pkgRoot verbosity lbi packageDb
      return
        ( relocatableInstalledPackageInfo
            pkg
            abi_hash
            lib
            lbi
            clbi
            fs
        )
    _ -> dieWithException verbosity RelocRegistrationInfo

initPackageDB :: Verbosity -> Compiler -> ProgramDb -> FilePath -> IO ()
initPackageDB verbosity comp progdb dbPath =
  createPackageDB verbosity comp progdb False dbPath

-- | Create an empty package DB at the specified location.
createPackageDB
  :: Verbosity
  -> Compiler
  -> ProgramDb
  -> Bool
  -> FilePath
  -> IO ()
createPackageDB verbosity comp progdb preferCompat dbPath =
  case compilerFlavor comp of
    GHC -> HcPkg.init (GHC.hcPkgInfo progdb) verbosity preferCompat dbPath
    GHCJS -> HcPkg.init (GHCJS.hcPkgInfo progdb) verbosity False dbPath
    UHC -> return ()
    HaskellSuite _ -> HaskellSuite.initPackageDB verbosity progdb dbPath
    _ -> dieWithException verbosity CreatePackageDB

doesPackageDBExist :: FilePath -> IO Bool
doesPackageDBExist dbPath = do
  -- currently one impl for all compiler flavours, but could change if needed
  dir_exists <- doesDirectoryExist dbPath
  if dir_exists
    then return True
    else doesFileExist dbPath

deletePackageDB :: FilePath -> IO ()
deletePackageDB dbPath = do
  -- currently one impl for all compiler flavours, but could change if needed
  dir_exists <- doesDirectoryExist dbPath
  if dir_exists
    then removeDirectoryRecursive dbPath
    else do
      file_exists <- doesFileExist dbPath
      when file_exists $ removeFile dbPath

-- | Run @hc-pkg@ using a given package DB stack, directly forwarding the
-- provided command-line arguments to it.
invokeHcPkg
  :: Verbosity
  -> Compiler
  -> ProgramDb
  -> Maybe (SymbolicPath CWD (Dir Pkg))
  -> PackageDBStack
  -> [String]
  -> IO ()
invokeHcPkg verbosity comp progdb mbWorkDir dbStack extraArgs =
  withHcPkg
    verbosity
    "invokeHcPkg"
    comp
    progdb
    (\hpi -> HcPkg.invoke hpi verbosity mbWorkDir dbStack extraArgs)

withHcPkg
  :: Verbosity
  -> String
  -> Compiler
  -> ProgramDb
  -> (HcPkg.HcPkgInfo -> IO a)
  -> IO a
withHcPkg verbosity name comp progdb f =
  case compilerFlavor comp of
    GHC -> f (GHC.hcPkgInfo progdb)
    GHCJS -> f (GHCJS.hcPkgInfo progdb)
    _ -> dieWithException verbosity $ WithHcPkg name

registerPackage
  :: Verbosity
  -> Compiler
  -> ProgramDb
  -> Maybe (SymbolicPath CWD (Dir Pkg))
  -> PackageDBStack
  -> InstalledPackageInfo
  -> HcPkg.RegisterOptions
  -> IO ()
registerPackage verbosity comp progdb mbWorkDir packageDbs installedPkgInfo registerOptions =
  case compilerFlavor comp of
    GHC -> GHC.registerPackage verbosity progdb mbWorkDir packageDbs installedPkgInfo registerOptions
    GHCJS -> GHCJS.registerPackage verbosity progdb mbWorkDir packageDbs installedPkgInfo registerOptions
    HaskellSuite{} ->
      HaskellSuite.registerPackage verbosity progdb packageDbs installedPkgInfo
    _
      | HcPkg.registerMultiInstance registerOptions ->
          dieWithException verbosity RegisMultiplePkgNotSupported
    UHC -> UHC.registerPackage verbosity comp progdb packageDbs installedPkgInfo
    _ -> dieWithException verbosity RegisteringNotImplemented

writeHcPkgRegisterScript
  :: Verbosity
  -> Maybe (SymbolicPath CWD (Dir Pkg))
  -> [InstalledPackageInfo]
  -> PackageDBStack
  -> HcPkg.HcPkgInfo
  -> IO ()
writeHcPkgRegisterScript verbosity mbWorkDir ipis packageDbs hpi = do
  let genScript installedPkgInfo =
        let invocation =
              HcPkg.registerInvocation
                hpi
                Verbosity.normal
                mbWorkDir
                packageDbs
                installedPkgInfo
                HcPkg.defaultRegisterOptions
         in invocationAsSystemScript buildOS invocation
      scripts = map genScript ipis
      -- TODO: Do something more robust here
      regScript = unlines scripts

  info verbosity ("Creating package registration script: " ++ regScriptFileName)
  writeUTF8File regScriptFileName regScript
  setFileExecutable regScriptFileName

regScriptFileName :: FilePath
regScriptFileName = case buildOS of
  Windows -> "register.bat"
  _ -> "register.sh"

-- -----------------------------------------------------------------------------
-- Making the InstalledPackageInfo

-- | Construct 'InstalledPackageInfo' for a library in a package, given a set
-- of installation directories.
generalInstalledPackageInfo
  :: ([FilePath] -> [FilePath])
  -- ^ Translate relative include dir paths to
  -- absolute paths.
  -> PackageDescription
  -> AbiHash
  -> Library
  -> LocalBuildInfo
  -> ComponentLocalBuildInfo
  -> InstallDirs FilePath
  -> InstalledPackageInfo
generalInstalledPackageInfo adjustRelIncDirs pkg abi_hash lib lbi clbi installDirs =
  IPI.InstalledPackageInfo
    { IPI.sourcePackageId = packageId pkg
    , IPI.installedUnitId = componentUnitId clbi
    , IPI.installedComponentId_ = componentComponentId clbi
    , IPI.instantiatedWith = componentInstantiatedWith clbi
    , IPI.sourceLibName = libName lib
    , IPI.compatPackageKey = componentCompatPackageKey clbi
    , -- If GHC >= 8.4 we register with SDPX, otherwise with legacy license
      IPI.license =
        if ghc84
          then Left $ either id licenseToSPDX $ licenseRaw pkg
          else Right $ either licenseFromSPDX id $ licenseRaw pkg
    , IPI.copyright = copyright pkg
    , IPI.maintainer = maintainer pkg
    , IPI.author = author pkg
    , IPI.stability = stability pkg
    , IPI.homepage = homepage pkg
    , IPI.pkgUrl = pkgUrl pkg
    , IPI.synopsis = synopsis pkg
    , IPI.description = description pkg
    , IPI.category = category pkg
    , IPI.abiHash = abi_hash
    , IPI.indefinite = componentIsIndefinite clbi
    , IPI.exposed = libExposed lib
    , IPI.exposedModules =
        componentExposedModules clbi
          -- add virtual modules into the list of exposed modules for the
          -- package database as well.
          ++ map (\name -> IPI.ExposedModule name Nothing) (virtualModules bi)
    , IPI.hiddenModules = otherModules bi
    , IPI.trusted = IPI.trusted IPI.emptyInstalledPackageInfo
    , IPI.importDirs = [libdir installDirs | hasModules]
    , IPI.libraryDirs = libdirs
    , IPI.libraryDirsStatic = libdirsStatic
    , IPI.libraryDynDirs = dynlibdirs
    , IPI.dataDir = datadir installDirs
    , IPI.hsLibraries =
        ( if hasLibrary
            then [getHSLibraryName (componentUnitId clbi)]
            else []
        )
          ++ extraBundledLibs bi
    , IPI.extraLibraries = extraLibs bi
    , IPI.extraLibrariesStatic = extraLibsStatic bi
    , IPI.extraGHCiLibraries = extraGHCiLibs bi
    , IPI.includeDirs = absinc ++ adjustRelIncDirs relinc
    , IPI.includes = map getSymbolicPath $ includes bi
    , IPI.depends = depends
    , IPI.abiDepends = [] -- due to #5465
    , IPI.ccOptions = [] -- Note. NOT ccOptions bi!
    -- We don't want cc-options to be propagated
    -- to C compilations in other packages.
    , IPI.cxxOptions = [] -- Also. NOT cxxOptions bi!
    , IPI.ldOptions = ldOptions bi
    , IPI.frameworks = map getSymbolicPath $ frameworks bi
    , IPI.frameworkDirs = map getSymbolicPath $ extraFrameworkDirs bi
    , IPI.haddockInterfaces = [haddockdir installDirs </> haddockName pkg]
    , IPI.haddockHTMLs = [htmldir installDirs]
    , IPI.pkgRoot = Nothing
    , IPI.libVisibility = libVisibility lib
    }
  where
    ghc84 = case compilerId $ compiler lbi of
      CompilerId GHC v -> v >= mkVersion [8, 4]
      _ -> False

    bi = libBuildInfo lib
    -- TODO: unclear what the root cause of the
    -- duplication is, but we nub it here for now:
    depends = ordNub $ map fst (componentPackageDeps clbi)
    (absinc, relinc) = partition isAbsolute (map getSymbolicPath $ includeDirs bi)
    hasModules = not $ null (allLibModules lib clbi)
    comp = compiler lbi
    hasLibrary =
      ( hasModules
          || not (null (cSources bi))
          || not (null (asmSources bi))
          || not (null (cmmSources bi))
          || not (null (cxxSources bi))
          || (not (null (jsSources bi)) && hasJsSupport)
      )
        && not (componentIsIndefinite clbi)
    hasJsSupport = case hostPlatform lbi of
      Platform JavaScript _ -> True
      _ -> False
    extraLibDirs' = map getSymbolicPath $ extraLibDirs bi
    libdirsStatic
      | hasLibrary = libdir installDirs : extraLibDirsStaticOrFallback
      | otherwise = extraLibDirsStaticOrFallback
      where
        -- If no static library dirs were given, the package likely makes no
        -- distinction between fully static linking and otherwise.
        -- Fall back to the normal library dirs in that case.
        extraLibDirsStaticOrFallback = case extraLibDirsStatic bi of
          [] -> extraLibDirs'
          dirs -> map getSymbolicPath dirs
    (libdirs, dynlibdirs)
      | not hasLibrary =
          (extraLibDirs', [])
      -- the dynamic-library-dirs defaults to the library-dirs if not specified,
      -- so this works whether the dynamic-library-dirs field is supported or not

      | libraryDynDirSupported comp =
          ( libdir installDirs : extraLibDirs'
          , dynlibdir installDirs : extraLibDirs'
          )
      | otherwise =
          (libdir installDirs : dynlibdir installDirs : extraLibDirs', [])

-- the compiler doesn't understand the dynamic-library-dirs field so we
-- add the dyn directory to the "normal" list in the library-dirs field

-- | Construct 'InstalledPackageInfo' for a library that is in place in the
-- build tree.
--
-- This function knows about the layout of in place packages.
inplaceInstalledPackageInfo
  :: FilePath
  -- ^ top of the build tree (absolute path)
  -> SymbolicPath Pkg (Dir Dist)
  -- ^ location of the dist tree
  -> PackageDescription
  -> AbiHash
  -> Library
  -> LocalBuildInfo
  -> ComponentLocalBuildInfo
  -> InstalledPackageInfo
inplaceInstalledPackageInfo inplaceDir distPref pkg abi_hash lib lbi clbi =
  generalInstalledPackageInfo
    adjustRelativeIncludeDirs
    pkg
    abi_hash
    lib
    lbi
    clbi
    installDirs
  where
    i = interpretSymbolicPath (Just $ makeSymbolicPath inplaceDir) -- See Note [Symbolic paths] in Distribution.Utils.Path
    adjustRelativeIncludeDirs = concatMap $ \d ->
      [ i $ makeRelativePathEx d -- local include-dir
      , i $ libTargetDir </> makeRelativePathEx d -- autogen include-dir
      ]
    libTargetDir = componentBuildDir lbi clbi
    installDirs =
      (absoluteComponentInstallDirs pkg lbi (componentUnitId clbi) NoCopyDest)
        { libdir = i libTargetDir
        , dynlibdir = i libTargetDir
        , datadir =
            let rawDataDir = dataDir pkg
             in if null $ getSymbolicPath rawDataDir
                  then i sameDirectory
                  else i rawDataDir
        , docdir = i inplaceDocdir
        , htmldir = inplaceHtmldir
        , haddockdir = inplaceHtmldir
        }
    inplaceDocdir = distPref </> makeRelativePathEx "doc"
    inplaceHtmldir = i $ inplaceDocdir </> makeRelativePathEx ("html" </> prettyShow (packageName pkg))

-- | Construct 'InstalledPackageInfo' for the final install location of a
-- library package.
--
-- This function knows about the layout of installed packages.
absoluteInstalledPackageInfo
  :: PackageDescription
  -> AbiHash
  -> Library
  -> LocalBuildInfo
  -> ComponentLocalBuildInfo
  -> InstalledPackageInfo
absoluteInstalledPackageInfo pkg abi_hash lib lbi clbi =
  generalInstalledPackageInfo
    adjustReativeIncludeDirs
    pkg
    abi_hash
    lib
    lbi
    clbi
    installDirs
  where
    -- For installed packages we install all include files into one dir,
    -- whereas in the build tree they may live in multiple local dirs.
    adjustReativeIncludeDirs _
      | null (installIncludes bi) = []
      | otherwise = [includedir installDirs]
    bi = libBuildInfo lib
    installDirs = absoluteComponentInstallDirs pkg lbi (componentUnitId clbi) NoCopyDest

relocatableInstalledPackageInfo
  :: PackageDescription
  -> AbiHash
  -> Library
  -> LocalBuildInfo
  -> ComponentLocalBuildInfo
  -> SymbolicPath CWD ('Dir Pkg)
  -> InstalledPackageInfo
relocatableInstalledPackageInfo pkg abi_hash lib lbi clbi pkgroot =
  generalInstalledPackageInfo
    adjustReativeIncludeDirs
    pkg
    abi_hash
    lib
    lbi
    clbi
    installDirs
  where
    -- For installed packages we install all include files into one dir,
    -- whereas in the build tree they may live in multiple local dirs.
    adjustReativeIncludeDirs _
      | null (installIncludes bi) = []
      | otherwise = [includedir installDirs]
    bi = libBuildInfo lib

    installDirs =
      fmap (("${pkgroot}" </>) . shortRelativePath (getSymbolicPath pkgroot)) $
        absoluteComponentInstallDirs pkg lbi (componentUnitId clbi) NoCopyDest

-- -----------------------------------------------------------------------------
-- Unregistration

unregister :: PackageDescription -> LocalBuildInfo -> RegisterFlags -> IO ()
unregister pkg lbi regFlags = do
  let pkgid = packageId pkg
      common = registerCommonFlags regFlags
      genScript = fromFlag (regGenScript regFlags)
      verbosity = fromFlag (setupVerbosity common)
      packageDb =
        fromFlagOrDefault
          (registrationPackageDB (withPackageDB lbi))
          (regPackageDB regFlags)
      mbWorkDir = mbWorkDirLBI lbi
      unreg hpi =
        let invocation =
              HcPkg.unregisterInvocation
                hpi
                Verbosity.normal
                mbWorkDir
                packageDb
                pkgid
         in if genScript
              then
                writeFileAtomic
                  unregScriptFileName
                  (BS.Char8.pack $ invocationAsSystemScript buildOS invocation)
              else runProgramInvocation verbosity invocation
  setupMessage verbosity "Unregistering" pkgid
  withHcPkg
    verbosity
    "unregistering is only implemented for GHC and GHCJS"
    (compiler lbi)
    (withPrograms lbi)
    unreg

unregScriptFileName :: FilePath
unregScriptFileName = case buildOS of
  Windows -> "unregister.bat"
  _ -> "unregister.sh"

internalPackageDBPath :: LocalBuildInfo -> SymbolicPath Pkg (Dir Dist) -> SymbolicPath Pkg (Dir PkgDB)
internalPackageDBPath lbi distPref =
  case compilerFlavor (compiler lbi) of
    UHC -> UHC.inplacePackageDbPath lbi
    _ -> distPref </> makeRelativePathEx "package.conf.inplace"
