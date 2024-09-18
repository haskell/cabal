{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-----------------------------------------------------------------------------

-- |
-- Module      :  Distribution.Simple.Haddock
-- Copyright   :  Isaac Jones 2003-2005
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This module deals with the @haddock@ and @hscolour@ commands.
-- It uses information about installed packages (from @ghc-pkg@) to find the
-- locations of documentation for dependent packages, so it can create links.
--
-- The @hscolour@ support allows generating HTML versions of the original
-- source, with coloured syntax highlighting.
module Distribution.Simple.Haddock
  ( haddock
  , haddock_setupHooks
  , createHaddockIndex
  , hscolour
  , hscolour_setupHooks
  , haddockPackagePaths
  , Visibility (..)
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import qualified Distribution.Simple.GHC as GHC
import qualified Distribution.Simple.GHCJS as GHCJS

-- local

import Distribution.Backpack (OpenModule)
import Distribution.Backpack.DescribeUnitId
import Distribution.Compat.Semigroup (All (..), Any (..))
import Distribution.InstalledPackageInfo (InstalledPackageInfo)
import qualified Distribution.InstalledPackageInfo as InstalledPackageInfo
import qualified Distribution.ModuleName as ModuleName
import Distribution.Package
import Distribution.PackageDescription
import Distribution.Parsec (simpleParsec)
import Distribution.Pretty
import Distribution.Simple.Build
import Distribution.Simple.BuildPaths
import Distribution.Simple.BuildTarget
import Distribution.Simple.Compiler
import Distribution.Simple.Errors
import Distribution.Simple.Flag
import Distribution.Simple.Glob (matchDirFileGlob)
import Distribution.Simple.InstallDirs
import Distribution.Simple.LocalBuildInfo hiding (substPathTemplate)
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.Simple.PreProcess
import Distribution.Simple.Program
import Distribution.Simple.Program.GHC
import qualified Distribution.Simple.Program.HcPkg as HcPkg
import Distribution.Simple.Program.ResponseFile
import Distribution.Simple.Register
import Distribution.Simple.Setup.Common
import Distribution.Simple.Setup.Haddock
import Distribution.Simple.Setup.Hscolour
import Distribution.Simple.SetupHooks.Internal
  ( BuildHooks (..)
  , BuildingWhat (..)
  , noBuildHooks
  )
import qualified Distribution.Simple.SetupHooks.Internal as SetupHooks
import qualified Distribution.Simple.SetupHooks.Rule as SetupHooks
import Distribution.Simple.Utils
import Distribution.System
import Distribution.Types.ComponentLocalBuildInfo
import Distribution.Types.ExposedModule
import Distribution.Types.LocalBuildInfo
import Distribution.Types.TargetInfo
import Distribution.Utils.Path hiding
  ( Dir
  )
import qualified Distribution.Utils.Path as Path
import qualified Distribution.Utils.ShortText as ShortText
import Distribution.Verbosity
import Distribution.Version

import Control.Monad
import Data.Bool (bool)
import Data.Either (rights)
import System.Directory (doesDirectoryExist, doesFileExist)
import System.FilePath (isAbsolute, normalise)
import System.IO (hClose, hPutStrLn, hSetEncoding, utf8)

-- ------------------------------------------------------------------------------
-- Types

-- | A record that represents the arguments to the haddock executable, a product
-- monoid.
data HaddockArgs = HaddockArgs
  { argInterfaceFile :: Flag FilePath
  -- ^ Path to the interface file, relative to argOutputDir, required.
  , argPackageName :: Flag PackageIdentifier
  -- ^ Package name, required.
  , argComponentName :: Flag String
  -- ^ Optional name used to construct haddock's `--package-name` option for
  -- various components (tests suites, sublibriaries, etc).
  , argHideModules :: (All, [ModuleName.ModuleName])
  -- ^ (Hide modules ?, modules to hide)
  , argIgnoreExports :: Any
  -- ^ Ignore export lists in modules?
  , argLinkSource :: Flag (Template, Template, Template)
  -- ^ (Template for modules, template for symbols, template for lines).
  , argLinkedSource :: Flag Bool
  -- ^ Generate hyperlinked sources
  , argQuickJump :: Flag Bool
  -- ^ Generate quickjump index
  , argCssFile :: Flag FilePath
  -- ^ Optional custom CSS file.
  , argContents :: Flag String
  -- ^ Optional URL to contents page.
  , argGenContents :: Flag Bool
  -- ^ Generate contents
  , argIndex :: Flag String
  -- ^ Optional URL to index page.
  , argGenIndex :: Flag Bool
  -- ^ Generate index
  , argBaseUrl :: Flag String
  -- ^ Optional base url from which static files will be loaded.
  , argVerbose :: Any
  , argOutput :: Flag [Output]
  -- ^ HTML or Hoogle doc or both? Required.
  , argInterfaces :: [(FilePath, Maybe String, Maybe String, Visibility)]
  -- ^ [(Interface file, URL to the HTML docs and hyperlinked-source for links)].
  , argOutputDir :: Directory
  -- ^ Where to generate the documentation.
  , argTitle :: Flag String
  -- ^ Page title, required.
  , argPrologue :: Flag String
  -- ^ Prologue text, required for 'haddock', ignored by 'haddocks'.
  , argPrologueFile :: Flag FilePath
  -- ^ Prologue file name, ignored by 'haddock', optional for 'haddocks'.
  , argGhcOptions :: GhcOptions
  -- ^ Additional flags to pass to GHC.
  , argGhcLibDir :: Flag FilePath
  -- ^ To find the correct GHC, required.
  , argReexports :: [OpenModule]
  -- ^ Re-exported modules
  , argTargets :: [FilePath]
  -- ^ Modules to process.
  , argResourcesDir :: Flag String
  -- ^ haddock's static \/ auxiliary files.
  , argUseUnicode :: Flag Bool
  -- ^ haddock's `--use-unicode` flag
  }
  deriving (Generic)

-- | The FilePath of a directory, it's a monoid under '(</>)'.
newtype Directory = Dir {unDir' :: FilePath} deriving (Read, Show, Eq, Ord)

-- NB: only correct at the top-level, after we have combined monoidally
-- the top-level output directory with the component subdir.
unDir :: Directory -> SymbolicPath Pkg (Path.Dir Artifacts)
unDir = makeSymbolicPath . normalise . unDir'

type Template = String

data Output = Html | Hoogle
  deriving (Eq)

-- ------------------------------------------------------------------------------
-- Haddock support

-- | Get Haddock program and check if it matches the request
getHaddockProg
  :: Verbosity
  -> ProgramDb
  -> Compiler
  -> HaddockArgs
  -> Flag Bool
  -- ^ quickjump feature
  -> IO (ConfiguredProgram, Version)
getHaddockProg verbosity programDb comp args quickJumpFlag = do
  let HaddockArgs
        { argQuickJump
        , argOutput
        } = args
      hoogle = Hoogle `elem` fromFlagOrDefault [] argOutput

  (haddockProg, version, _) <-
    requireProgramVersion
      verbosity
      haddockProgram
      (orLaterVersion (mkVersion [2, 0]))
      programDb

  -- various sanity checks
  when (hoogle && version < mkVersion [2, 2]) $
    dieWithException verbosity NoSupportForHoogle

  when (fromFlag argQuickJump && version < mkVersion [2, 19]) $ do
    let msg = "Haddock prior to 2.19 does not support the --quickjump flag."
        alt = "The generated documentation won't have the QuickJump feature."
    if Flag True == quickJumpFlag
      then dieWithException verbosity NoSupportForQuickJumpFlag
      else warn verbosity (msg ++ "\n" ++ alt)

  haddockGhcVersionStr <-
    getProgramOutput
      verbosity
      haddockProg
      ["--ghc-version"]
  case (simpleParsec haddockGhcVersionStr, compilerCompatVersion GHC comp) of
    (Nothing, _) -> dieWithException verbosity NoGHCVersionFromHaddock
    (_, Nothing) -> dieWithException verbosity NoGHCVersionFromCompiler
    (Just haddockGhcVersion, Just ghcVersion)
      | haddockGhcVersion == ghcVersion -> return ()
      | otherwise -> dieWithException verbosity $ HaddockAndGHCVersionDoesntMatch ghcVersion haddockGhcVersion

  return (haddockProg, version)

haddock
  :: PackageDescription
  -> LocalBuildInfo
  -> [PPSuffixHandler]
  -> HaddockFlags
  -> IO ()
haddock = haddock_setupHooks noBuildHooks

haddock_setupHooks
  :: BuildHooks
  -> PackageDescription
  -> LocalBuildInfo
  -> [PPSuffixHandler]
  -> HaddockFlags
  -> IO ()
haddock_setupHooks
  _
  pkg_descr
  _
  _
  haddockFlags
    | not (hasLibs pkg_descr)
        && not (fromFlag $ haddockExecutables haddockFlags)
        && not (fromFlag $ haddockTestSuites haddockFlags)
        && not (fromFlag $ haddockBenchmarks haddockFlags)
        && not (fromFlag $ haddockForeignLibs haddockFlags) =
        warn (fromFlag $ setupVerbosity $ haddockCommonFlags haddockFlags) $
          "No documentation was generated as this package does not contain "
            ++ "a library. Perhaps you want to use the --executables, --tests,"
            ++ " --benchmarks or --foreign-libraries flags."
haddock_setupHooks
  (BuildHooks{preBuildComponentRules = mbPbcRules})
  pkg_descr
  lbi
  suffixes
  flags' = do
    let verbosity = fromFlag $ haddockVerbosity flags
        mbWorkDir = flagToMaybe $ haddockWorkingDir flags
        comp = compiler lbi
        platform = hostPlatform lbi

        quickJmpFlag = haddockQuickJump flags'
        flags = case haddockTarget of
          ForDevelopment -> flags'
          ForHackage ->
            flags'
              { haddockHoogle = Flag True
              , haddockHtml = Flag True
              , haddockHtmlLocation = Flag (pkg_url ++ "/docs")
              , haddockContents = Flag (toPathTemplate pkg_url)
              , haddockLinkedSource = Flag True
              , haddockQuickJump = Flag True
              }
        pkg_url = "/package/$pkg-$version"
        flag f = fromFlag $ f flags

        tmpFileOpts =
          defaultTempFileOptions
            { optKeepTempFiles = flag haddockKeepTempFiles
            }
        htmlTemplate =
          fmap toPathTemplate . flagToMaybe . haddockHtmlLocation $
            flags
        haddockTarget =
          fromFlagOrDefault ForDevelopment (haddockForHackage flags')

    libdirArgs <- getGhcLibDir verbosity lbi
    -- The haddock-output-dir flag overrides any other documentation placement concerns.
    -- The point is to give the user full freedom over the location if they need it.
    let overrideWithOutputDir args = case haddockOutputDir flags of
          NoFlag -> args
          Flag dir -> args{argOutputDir = Dir dir}
    let commonArgs =
          overrideWithOutputDir $
            mconcat
              [ libdirArgs
              , fromFlags (haddockTemplateEnv lbi (packageId pkg_descr)) flags
              , fromPackageDescription haddockTarget pkg_descr
              ]

    (haddockProg, version) <-
      getHaddockProg verbosity (withPrograms lbi) comp commonArgs quickJmpFlag

    -- We fall back to using HsColour only for versions of Haddock which don't
    -- support '--hyperlinked-sources'.
    let using_hscolour = flag haddockLinkedSource && version < mkVersion [2, 17]
    when using_hscolour $
      hscolour'
        noBuildHooks
        -- NB: we are not passing the user BuildHooks here,
        -- because we are already running the pre/post build hooks
        -- for Haddock.
        (warn verbosity)
        haddockTarget
        pkg_descr
        lbi
        suffixes
        (defaultHscolourFlags `mappend` haddockToHscolour flags)

    targets <- readTargetInfos verbosity pkg_descr lbi (haddockTargets flags)

    let
      targets' =
        case targets of
          [] -> allTargetsInBuildOrder' pkg_descr lbi
          _ -> targets

    internalPackageDB <-
      createInternalPackageDB verbosity lbi (flag $ setupDistPref . haddockCommonFlags)

    (\f -> foldM_ f (installedPkgs lbi) targets') $ \index target -> do
      let
        component = targetComponent target
        clbi = targetCLBI target
        bi = componentBuildInfo component
        -- Include any build-tool-depends on build tools internal to the current package.
        progs' = addInternalBuildTools pkg_descr lbi bi (withPrograms lbi)
        lbi' =
          lbi
            { withPrograms = progs'
            , withPackageDB = withPackageDB lbi ++ [internalPackageDB]
            , installedPkgs = index
            }

        runPreBuildHooks :: LocalBuildInfo -> TargetInfo -> IO ()
        runPreBuildHooks lbi2 tgt =
          let inputs =
                SetupHooks.PreBuildComponentInputs
                  { SetupHooks.buildingWhat = BuildHaddock flags
                  , SetupHooks.localBuildInfo = lbi2
                  , SetupHooks.targetInfo = tgt
                  }
           in for_ mbPbcRules $ \pbcRules -> do
                (ruleFromId, _mons) <- SetupHooks.computeRules verbosity inputs pbcRules
                SetupHooks.executeRules verbosity lbi2 tgt ruleFromId

      -- See Note [Hi Haddock Recompilation Avoidance]
      reusingGHCCompilationArtifacts verbosity tmpFileOpts mbWorkDir lbi bi clbi version $ \haddockArtifactsDirs -> do
        preBuildComponent runPreBuildHooks verbosity lbi' target
        preprocessComponent pkg_descr component lbi' clbi False verbosity suffixes
        let
          doExe com = case (compToExe com) of
            Just exe -> do
              exeArgs <-
                fromExecutable
                  verbosity
                  haddockArtifactsDirs
                  lbi'
                  clbi
                  htmlTemplate
                  haddockTarget
                  pkg_descr
                  exe
                  commonArgs
              runHaddock
                verbosity
                mbWorkDir
                tmpFileOpts
                comp
                platform
                haddockProg
                True
                exeArgs
            Nothing -> do
              warn
                verbosity
                "Unsupported component, skipping..."
              return ()
          -- We define 'smsg' once and then reuse it inside the case, so that
          -- we don't say we are running Haddock when we actually aren't
          -- (e.g., Haddock is not run on non-libraries)
          smsg :: IO ()
          smsg =
            setupMessage'
              verbosity
              "Running Haddock on"
              (packageId pkg_descr)
              (componentLocalName clbi)
              (maybeComponentInstantiatedWith clbi)
        ipi <- case component of
          CLib lib -> do
            smsg
            libArgs <-
              fromLibrary
                verbosity
                haddockArtifactsDirs
                lbi'
                clbi
                htmlTemplate
                haddockTarget
                pkg_descr
                lib
                commonArgs
            runHaddock
              verbosity
              mbWorkDir
              tmpFileOpts
              comp
              platform
              haddockProg
              True
              libArgs
            inplaceDir <- absoluteWorkingDirLBI lbi

            let
              ipi =
                inplaceInstalledPackageInfo
                  inplaceDir
                  (flag $ setupDistPref . haddockCommonFlags)
                  pkg_descr
                  (mkAbiHash "inplace")
                  lib
                  lbi'
                  clbi

            debug verbosity $
              "Registering inplace:\n"
                ++ (InstalledPackageInfo.showInstalledPackageInfo ipi)

            registerPackage
              verbosity
              (compiler lbi')
              (withPrograms lbi')
              mbWorkDir
              (withPackageDB lbi')
              ipi
              HcPkg.defaultRegisterOptions
                { HcPkg.registerMultiInstance = True
                }

            return $ PackageIndex.insert ipi index
          CFLib flib ->
            when
              (flag haddockForeignLibs)
              ( do
                  smsg
                  flibArgs <-
                    fromForeignLib
                      verbosity
                      haddockArtifactsDirs
                      lbi'
                      clbi
                      htmlTemplate
                      haddockTarget
                      pkg_descr
                      flib
                      commonArgs
                  runHaddock
                    verbosity
                    mbWorkDir
                    tmpFileOpts
                    comp
                    platform
                    haddockProg
                    True
                    flibArgs
              )
              >> return index
          CExe _ -> when (flag haddockExecutables) (smsg >> doExe component) >> return index
          CTest test -> do
            when (flag haddockTestSuites) $ do
              smsg
              testArgs <-
                fromTest
                  verbosity
                  haddockArtifactsDirs
                  lbi'
                  clbi
                  htmlTemplate
                  haddockTarget
                  pkg_descr
                  test
                  commonArgs
              runHaddock
                verbosity
                mbWorkDir
                tmpFileOpts
                comp
                platform
                haddockProg
                True
                testArgs
            return index
          CBench bench -> do
            when (flag haddockBenchmarks) $ do
              smsg
              benchArgs <-
                fromBenchmark
                  verbosity
                  haddockArtifactsDirs
                  lbi'
                  clbi
                  htmlTemplate
                  haddockTarget
                  pkg_descr
                  bench
                  commonArgs
              runHaddock
                verbosity
                mbWorkDir
                tmpFileOpts
                comp
                platform
                haddockProg
                True
                benchArgs
            return index

        return ipi

    for_ (extraDocFiles pkg_descr) $ \fpath -> do
      files <- matchDirFileGlob verbosity (specVersion pkg_descr) mbWorkDir fpath
      let targetDir = Dir $ unDir' (argOutputDir commonArgs) </> haddockDirName haddockTarget pkg_descr
      for_ files $
        copyFileToCwd verbosity mbWorkDir (unDir targetDir)

-- | Execute 'Haddock' configured with 'HaddocksFlags'.  It is used to build
-- index and contents for documentation of multiple packages.
createHaddockIndex
  :: Verbosity
  -> ProgramDb
  -> Compiler
  -> Platform
  -> Maybe (SymbolicPath CWD (Path.Dir Pkg))
  -> HaddockProjectFlags
  -> IO ()
createHaddockIndex verbosity programDb comp platform mbWorkDir flags = do
  let args = fromHaddockProjectFlags flags
  (haddockProg, _version) <-
    getHaddockProg verbosity programDb comp args (Flag True)
  runHaddock verbosity mbWorkDir defaultTempFileOptions comp platform haddockProg False args

-- ------------------------------------------------------------------------------
-- Contributions to HaddockArgs (see also Doctest.hs for very similar code).

fromFlags :: PathTemplateEnv -> HaddockFlags -> HaddockArgs
fromFlags env flags =
  mempty
    { argHideModules =
        ( maybe mempty (All . not) $
            flagToMaybe (haddockInternal flags)
        , mempty
        )
    , argLinkSource =
        if fromFlag (haddockLinkedSource flags)
          then
            Flag
              ( "src/%{MODULE/./-}.html"
              , "src/%{MODULE/./-}.html#%{NAME}"
              , "src/%{MODULE/./-}.html#line-%{LINE}"
              )
          else NoFlag
    , argLinkedSource = haddockLinkedSource flags
    , argQuickJump = haddockQuickJump flags
    , argCssFile = haddockCss flags
    , argContents =
        fmap
          (fromPathTemplate . substPathTemplate env)
          (haddockContents flags)
    , argGenContents = Flag False
    , argIndex =
        fmap
          (fromPathTemplate . substPathTemplate env)
          (haddockIndex flags)
    , argGenIndex = Flag False
    , argBaseUrl = haddockBaseUrl flags
    , argResourcesDir = haddockResourcesDir flags
    , argVerbose =
        maybe mempty (Any . (>= deafening))
          . flagToMaybe
          $ setupVerbosity commonFlags
    , argOutput =
        Flag $ case [Html | Flag True <- [haddockHtml flags]]
          ++ [Hoogle | Flag True <- [haddockHoogle flags]] of
          [] -> [Html]
          os -> os
    , argOutputDir = maybe mempty (Dir . getSymbolicPath) . flagToMaybe $ setupDistPref commonFlags
    , argGhcOptions = mempty{ghcOptExtra = ghcArgs}
    , argUseUnicode = haddockUseUnicode flags
    }
  where
    ghcArgs = fromMaybe [] . lookup "ghc" . haddockProgramArgs $ flags
    commonFlags = haddockCommonFlags flags

fromHaddockProjectFlags :: HaddockProjectFlags -> HaddockArgs
fromHaddockProjectFlags flags =
  mempty
    { argOutputDir = Dir (fromFlag $ haddockProjectDir flags)
    , argQuickJump = Flag True
    , argGenContents = Flag True
    , argGenIndex = Flag True
    , argPrologueFile = haddockProjectPrologue flags
    , argInterfaces = fromFlagOrDefault [] (haddockProjectInterfaces flags)
    , argLinkedSource = Flag True
    , argResourcesDir = haddockProjectResourcesDir flags
    }

fromPackageDescription :: HaddockTarget -> PackageDescription -> HaddockArgs
fromPackageDescription _haddockTarget pkg_descr =
  mempty
    { argInterfaceFile = Flag $ haddockPath pkg_descr
    , argPackageName = Flag $ packageId $ pkg_descr
    , argOutputDir = Dir $ "doc" </> "html"
    , argPrologue =
        Flag $
          ShortText.fromShortText $
            if ShortText.null desc
              then synopsis pkg_descr
              else desc
    , argTitle = Flag $ showPkg ++ subtitle
    }
  where
    desc = description pkg_descr
    showPkg = prettyShow (packageId pkg_descr)
    subtitle
      | ShortText.null (synopsis pkg_descr) = ""
      | otherwise = ": " ++ ShortText.fromShortText (synopsis pkg_descr)

componentGhcOptions
  :: Verbosity
  -> LocalBuildInfo
  -> BuildInfo
  -> ComponentLocalBuildInfo
  -> SymbolicPath Pkg (Path.Dir build)
  -> GhcOptions
componentGhcOptions verbosity lbi bi clbi odir =
  let f = case compilerFlavor (compiler lbi) of
        GHC -> GHC.componentGhcOptions
        GHCJS -> GHCJS.componentGhcOptions
        _ ->
          error $
            "Distribution.Simple.Haddock.componentGhcOptions:"
              ++ "haddock only supports GHC and GHCJS"
   in f verbosity lbi bi clbi odir

{-
Note [Hi Haddock Recompilation Avoidance]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Starting with Haddock 2.28, we no longer want to run Haddock's GHC session in
an arbitrary temporary directory. Doing so always causes recompilation during
documentation generation, which can now be avoided thanks to Hi Haddock.

Instead, we want to re-use the interface and object files produced by GHC.
We copy these intermediate files produced by GHC to temporary directories and
point haddock to them.

The reason why we can't use the GHC files /inplace/ is that haddock may have to
recompile (e.g. because of `haddock-options`). In that case, we want to be sure
the files produced by GHC do not get overwritten.

See https://github.com/haskell/cabal/pull/9177 for discussion.

(W.1) As it turns out, -stubdir is included in GHC's recompilation fingerprint.
This means that if we use a temporary directory for stubfiles produced by GHC
for the haddock invocation, haddock will trigger full recompilation since the
stubdir would be different.

So we don't use a temporary stubdir, despite the tmp o-dir and hi-dir:

We want to avoid at all costs haddock accidentally overwriting o-files and
hi-files (e.g. if a user specified haddock-option triggers recompilation), and
thus copy them to a temporary directory to pass them on to haddock. However,
stub files are much less problematic since ABI-incompatibility isn't at play
here, that is, there doesn't seem to be a GHC flag that could accidentally make
a stub file incompatible with the one produced by GHC from the same module.
-}

mkHaddockArgs
  :: Verbosity
  -> (SymbolicPath Pkg (Path.Dir Artifacts), SymbolicPath Pkg (Path.Dir Artifacts), SymbolicPath Pkg (Path.Dir Artifacts))
  -- ^ Directories for -hidir, -odir, and -stubdir to GHC through Haddock.
  -- See Note [Hi Haddock Recompilation Avoidance]
  -> LocalBuildInfo
  -> ComponentLocalBuildInfo
  -> Maybe PathTemplate
  -- ^ template for HTML location
  -> [SymbolicPath Pkg File]
  -> BuildInfo
  -> IO HaddockArgs
mkHaddockArgs verbosity (tmpObjDir, tmpHiDir, tmpStubDir) lbi clbi htmlTemplate inFiles bi = do
  let
    vanillaOpts' =
      componentGhcOptions normal lbi bi clbi (buildDir lbi)
    vanillaOpts =
      vanillaOpts'
        { -- See Note [Hi Haddock Recompilation Avoidance]
          ghcOptObjDir = toFlag tmpObjDir
        , ghcOptHiDir = toFlag tmpHiDir
        , ghcOptStubDir = toFlag tmpStubDir
        }
    sharedOpts =
      vanillaOpts
        { ghcOptDynLinkMode = toFlag GhcDynamicOnly
        , ghcOptFPic = toFlag True
        , ghcOptHiSuffix = toFlag "dyn_hi"
        , ghcOptObjSuffix = toFlag "dyn_o"
        , ghcOptExtra = hcSharedOptions GHC bi
        }
  ifaceArgs <- getInterfaces verbosity lbi clbi htmlTemplate
  opts <-
    if withVanillaLib lbi
      then return vanillaOpts
      else
        if withSharedLib lbi
          then return sharedOpts
          else dieWithException verbosity MustHaveSharedLibraries

  return
    ifaceArgs
      { argGhcOptions = opts
      , argTargets = map getSymbolicPath inFiles
      , argReexports = getReexports clbi
      }

fromLibrary
  :: Verbosity
  -> (SymbolicPath Pkg (Path.Dir Artifacts), SymbolicPath Pkg (Path.Dir Artifacts), SymbolicPath Pkg (Path.Dir Artifacts))
  -- ^ Directories for -hidir, -odir, and -stubdir to GHC through Haddock.
  -- See Note [Hi Haddock Recompilation Avoidance]
  -> LocalBuildInfo
  -> ComponentLocalBuildInfo
  -> Maybe PathTemplate
  -- ^ template for HTML location
  -> HaddockTarget
  -> PackageDescription
  -> Library
  -> HaddockArgs
  -- ^ common args
  -> IO HaddockArgs
fromLibrary verbosity haddockArtifactsDirs lbi clbi htmlTemplate haddockTarget pkg_descr lib commonArgs = do
  inFiles <- map snd `fmap` getLibSourceFiles verbosity lbi lib clbi
  args <-
    mkHaddockArgs
      verbosity
      haddockArtifactsDirs
      lbi
      clbi
      htmlTemplate
      inFiles
      (libBuildInfo lib)
  let args' =
        commonArgs
          <> args
            { argOutputDir =
                Dir $ haddockLibraryDirPath haddockTarget pkg_descr lib
            , argInterfaceFile = Flag $ haddockLibraryPath pkg_descr lib
            }
      args'' =
        args'
          { argHideModules = (mempty, otherModules (libBuildInfo lib))
          , argTitle = Flag $ haddockPackageLibraryName pkg_descr lib
          , argComponentName = toFlag (haddockPackageLibraryName' (pkgName (package pkg_descr)) (libName lib))
          , -- we need to accommodate for `argOutputDir`, see `haddockLibraryPath`
            argBaseUrl = case (libName lib, argBaseUrl args') of
              (LSubLibName _, Flag url) -> Flag $ ".." </> url
              (_, a) -> a
          , argContents = case (libName lib, argContents args') of
              (LSubLibName _, Flag url) -> Flag $ ".." </> url
              (_, a) -> a
          , argIndex = case (libName lib, argIndex args') of
              (LSubLibName _, Flag url) -> Flag $ ".." </> url
              (_, a) -> a
          }
  return args''

fromExecutable
  :: Verbosity
  -> (SymbolicPath Pkg (Path.Dir Artifacts), SymbolicPath Pkg (Path.Dir Artifacts), SymbolicPath Pkg (Path.Dir Artifacts))
  -- ^ Directories for -hidir, -odir, and -stubdir to GHC through Haddock.
  -- See Note [Hi Haddock Recompilation Avoidance]
  -> LocalBuildInfo
  -> ComponentLocalBuildInfo
  -> Maybe PathTemplate
  -- ^ template for HTML location
  -> HaddockTarget
  -> PackageDescription
  -> Executable
  -> HaddockArgs
  -- ^ common args
  -> IO HaddockArgs
fromExecutable verbosity haddockArtifactsDirs lbi clbi htmlTemplate haddockTarget pkg_descr exe commonArgs = do
  inFiles <- map snd `fmap` getExeSourceFiles verbosity lbi exe clbi
  args <-
    mkHaddockArgs
      verbosity
      haddockArtifactsDirs
      lbi
      clbi
      htmlTemplate
      inFiles
      (buildInfo exe)
  let args' =
        commonArgs
          <> args
            { argOutputDir =
                Dir $
                  haddockDirName haddockTarget pkg_descr
                    </> unUnqualComponentName (exeName exe)
            }
  return
    args'
      { argTitle = Flag $ unUnqualComponentName $ exeName exe
      , -- we need to accommodate `argOutputDir`
        argBaseUrl = case argBaseUrl args' of
          Flag url -> Flag $ ".." </> url
          NoFlag -> NoFlag
      , argContents = case argContents args' of
          Flag url -> Flag $ ".." </> url
          NoFlag -> NoFlag
      , argIndex = case argIndex args' of
          Flag url -> Flag $ ".." </> url
          NoFlag -> NoFlag
      }

fromTest
  :: Verbosity
  -> (SymbolicPath Pkg (Path.Dir Artifacts), SymbolicPath Pkg (Path.Dir Artifacts), SymbolicPath Pkg (Path.Dir Artifacts))
  -- ^ Directories for -hidir, -odir, and -stubdir to GHC through Haddock.
  -- See Note [Hi Haddock Recompilation Avoidance]
  -> LocalBuildInfo
  -> ComponentLocalBuildInfo
  -> Maybe PathTemplate
  -- ^ template for HTML location
  -> HaddockTarget
  -> PackageDescription
  -> TestSuite
  -> HaddockArgs
  -- ^ common args
  -> IO HaddockArgs
fromTest verbosity haddockArtifactsDirs lbi clbi htmlTemplate haddockTarget pkg_descr test commonArgs = do
  inFiles <- map snd `fmap` getTestSourceFiles verbosity lbi test clbi
  args <-
    mkHaddockArgs
      verbosity
      haddockArtifactsDirs
      lbi
      clbi
      htmlTemplate
      inFiles
      (testBuildInfo test)
  let args' =
        commonArgs
          <> args
            { argOutputDir =
                Dir $
                  haddockDirName haddockTarget pkg_descr
                    </> unUnqualComponentName (testName test)
            }
  return
    args'
      { argTitle = Flag $ prettyShow (packageName pkg_descr)
      , argComponentName = Flag $ prettyShow (packageName pkg_descr) ++ ":" ++ unUnqualComponentName (testName test)
      , -- we need to accommodate `argOutputDir`
        argBaseUrl = case argBaseUrl args' of
          Flag url -> Flag $ ".." </> url
          NoFlag -> NoFlag
      , argContents = case argContents args' of
          Flag url -> Flag $ ".." </> url
          NoFlag -> NoFlag
      , argIndex = case argIndex args' of
          Flag url -> Flag $ ".." </> url
          NoFlag -> NoFlag
      }

fromBenchmark
  :: Verbosity
  -> (SymbolicPath Pkg (Path.Dir Artifacts), SymbolicPath Pkg (Path.Dir Artifacts), SymbolicPath Pkg (Path.Dir Artifacts))
  -- ^ Directories for -hidir, -odir, and -stubdir to GHC through Haddock.
  -- See Note [Hi Haddock Recompilation Avoidance]
  -> LocalBuildInfo
  -> ComponentLocalBuildInfo
  -> Maybe PathTemplate
  -- ^ template for HTML location
  -> HaddockTarget
  -> PackageDescription
  -> Benchmark
  -> HaddockArgs
  -- ^ common args
  -> IO HaddockArgs
fromBenchmark verbosity haddockArtifactsDirs lbi clbi htmlTemplate haddockTarget pkg_descr bench commonArgs = do
  inFiles <- map snd `fmap` getBenchmarkSourceFiles verbosity lbi bench clbi
  args <-
    mkHaddockArgs
      verbosity
      haddockArtifactsDirs
      lbi
      clbi
      htmlTemplate
      inFiles
      (benchmarkBuildInfo bench)
  let args' =
        commonArgs
          <> args
            { argOutputDir =
                Dir $
                  haddockDirName haddockTarget pkg_descr
                    </> unUnqualComponentName (benchmarkName bench)
            }
  return
    args'
      { argTitle = Flag $ prettyShow (packageName pkg_descr)
      , argComponentName = Flag $ prettyShow (packageName pkg_descr) ++ ":" ++ unUnqualComponentName (benchmarkName bench)
      , -- we need to accommodate `argOutputDir`
        argBaseUrl = case argBaseUrl args' of
          Flag url -> Flag $ ".." </> url
          NoFlag -> NoFlag
      , argContents = case argContents args' of
          Flag url -> Flag $ ".." </> url
          NoFlag -> NoFlag
      , argIndex = case argIndex args' of
          Flag url -> Flag $ ".." </> url
          NoFlag -> NoFlag
      }

fromForeignLib
  :: Verbosity
  -> (SymbolicPath Pkg (Path.Dir Artifacts), SymbolicPath Pkg (Path.Dir Artifacts), SymbolicPath Pkg (Path.Dir Artifacts))
  -- ^ Directories for -hidir, -odir, and -stubdir to GHC through Haddock.
  -- See Note [Hi Haddock Recompilation Avoidance]
  -> LocalBuildInfo
  -> ComponentLocalBuildInfo
  -> Maybe PathTemplate
  -- ^ template for HTML location
  -> HaddockTarget
  -> PackageDescription
  -> ForeignLib
  -> HaddockArgs
  -- ^ common args
  -> IO HaddockArgs
fromForeignLib verbosity haddockArtifactsDirs lbi clbi htmlTemplate haddockTarget pkg_descr flib commonArgs = do
  inFiles <- map snd `fmap` getFLibSourceFiles verbosity lbi flib clbi
  args <-
    mkHaddockArgs
      verbosity
      haddockArtifactsDirs
      lbi
      clbi
      htmlTemplate
      inFiles
      (foreignLibBuildInfo flib)
  let args' =
        commonArgs
          <> args
            { argOutputDir =
                Dir $
                  haddockDirName haddockTarget pkg_descr
                    </> unUnqualComponentName (foreignLibName flib)
            }
  return
    args'
      { argTitle = Flag $ unUnqualComponentName $ foreignLibName flib
      , -- we need to accommodate `argOutputDir`
        argBaseUrl = case argBaseUrl args' of
          Flag url -> Flag $ ".." </> url
          NoFlag -> NoFlag
      , argContents = case argContents args' of
          Flag url -> Flag $ ".." </> url
          NoFlag -> NoFlag
      , argIndex = case argIndex args' of
          Flag url -> Flag $ ".." </> url
          NoFlag -> NoFlag
      }

compToExe :: Component -> Maybe Executable
compToExe comp =
  case comp of
    CTest test@TestSuite{testInterface = TestSuiteExeV10 _ f} ->
      Just
        Executable
          { exeName = testName test
          , modulePath = f
          , exeScope = ExecutablePublic
          , buildInfo = testBuildInfo test
          }
    CBench bench@Benchmark{benchmarkInterface = BenchmarkExeV10 _ f} ->
      Just
        Executable
          { exeName = benchmarkName bench
          , modulePath = f
          , exeScope = ExecutablePublic
          , buildInfo = benchmarkBuildInfo bench
          }
    CExe exe -> Just exe
    _ -> Nothing

getInterfaces
  :: Verbosity
  -> LocalBuildInfo
  -> ComponentLocalBuildInfo
  -> Maybe PathTemplate
  -- ^ template for HTML location
  -> IO HaddockArgs
getInterfaces verbosity lbi clbi htmlTemplate = do
  (packageFlags, warnings) <- haddockPackageFlags verbosity lbi clbi htmlTemplate
  traverse_ (warn (verboseUnmarkOutput verbosity)) warnings
  return $
    mempty
      { argInterfaces = packageFlags
      }

getReexports :: ComponentLocalBuildInfo -> [OpenModule]
getReexports LibComponentLocalBuildInfo{componentExposedModules = mods} =
  mapMaybe exposedReexport mods
getReexports _ = []

getGhcLibDir
  :: Verbosity
  -> LocalBuildInfo
  -> IO HaddockArgs
getGhcLibDir verbosity lbi = do
  l <- case compilerFlavor (compiler lbi) of
    GHC -> GHC.getLibDir verbosity lbi
    GHCJS -> GHCJS.getLibDir verbosity lbi
    _ -> error "haddock only supports GHC and GHCJS"
  return $ mempty{argGhcLibDir = Flag l}

-- | If Hi Haddock is supported, this function creates temporary directories
-- and copies existing interface and object files produced by GHC into them,
-- then passes them off to the given continuation.
--
-- If Hi Haddock is _not_ supported, we can't re-use GHC's compilation files.
-- Instead, we use a clean temporary directory to the continuation,
-- with no hope for recompilation avoidance.
--
-- See Note [Hi Haddock Recompilation Avoidance]
reusingGHCCompilationArtifacts
  :: Verbosity
  -> TempFileOptions
  -> Maybe (SymbolicPath CWD (Path.Dir Pkg))
  -- ^ Working directory
  -> LocalBuildInfo
  -> BuildInfo
  -> ComponentLocalBuildInfo
  -> Version
  -- ^ Haddock's version
  -> ((SymbolicPath Pkg (Path.Dir Artifacts), SymbolicPath Pkg (Path.Dir Artifacts), SymbolicPath Pkg (Path.Dir Artifacts)) -> IO r)
  -- ^ Continuation
  -> IO r
reusingGHCCompilationArtifacts verbosity tmpFileOpts mbWorkDir lbi bi clbi version act
  | version >= mkVersion [2, 28, 0] = do
      withTempDirectoryCwdEx verbosity tmpFileOpts mbWorkDir (distPrefLBI lbi) "haddock-objs" $ \tmpObjDir ->
        withTempDirectoryCwdEx verbosity tmpFileOpts mbWorkDir (distPrefLBI lbi) "haddock-his" $ \tmpHiDir -> do
          -- Re-use ghc's interface and obj files, but first copy them to
          -- somewhere where it is safe if haddock overwrites them
          let
            vanillaOpts = componentGhcOptions normal lbi bi clbi (buildDir lbi)
            i = interpretSymbolicPath mbWorkDir
            copyDir ghcDir tmpDir = copyDirectoryRecursive verbosity (i $ fromFlag $ ghcDir vanillaOpts) (i tmpDir)
          copyDir ghcOptObjDir tmpObjDir
          copyDir ghcOptHiDir tmpHiDir
          -- copyDir ghcOptStubDir tmpStubDir -- (see W.1 in Note [Hi Haddock Recompilation Avoidance])

          act (tmpObjDir, tmpHiDir, fromFlag $ ghcOptHiDir vanillaOpts)
  | otherwise = do
      withTempDirectoryCwdEx verbosity tmpFileOpts mbWorkDir (distPrefLBI lbi) "tmp" $
        \tmpFallback -> act (tmpFallback, tmpFallback, tmpFallback)

-- ------------------------------------------------------------------------------

-- | Call haddock with the specified arguments.
runHaddock
  :: Verbosity
  -> Maybe (SymbolicPath CWD (Path.Dir Pkg))
  -> TempFileOptions
  -> Compiler
  -> Platform
  -> ConfiguredProgram
  -> Bool
  -- ^ require targets
  -> HaddockArgs
  -> IO ()
runHaddock verbosity mbWorkDir tmpFileOpts comp platform haddockProg requireTargets args
  | requireTargets && null (argTargets args) =
      warn verbosity $
        "Haddocks are being requested, but there aren't any modules given "
          ++ "to create documentation for."
  | otherwise = do
      let haddockVersion =
            fromMaybe
              (error "unable to determine haddock version")
              (programVersion haddockProg)
      renderArgs verbosity mbWorkDir tmpFileOpts haddockVersion comp platform args $
        \flags result -> do
          runProgramCwd verbosity mbWorkDir haddockProg flags
          notice verbosity $ "Documentation created: " ++ result

renderArgs
  :: forall a
   . Verbosity
  -> Maybe (SymbolicPath CWD (Path.Dir Pkg))
  -> TempFileOptions
  -> Version
  -> Compiler
  -> Platform
  -> HaddockArgs
  -> ([String] -> FilePath -> IO a)
  -> IO a
renderArgs verbosity mbWorkDir tmpFileOpts version comp platform args k = do
  let haddockSupportsUTF8 = version >= mkVersion [2, 14, 4]
      haddockSupportsResponseFiles = version > mkVersion [2, 16, 2]
  createDirectoryIfMissingVerbose verbosity True (i outputDir)
  let withPrologueArgs prologueArgs =
        let renderedArgs = prologueArgs <> renderPureArgs version comp platform args
         in if haddockSupportsResponseFiles
              then
                withResponseFile
                  verbosity
                  tmpFileOpts
                  mbWorkDir
                  outputDir
                  "haddock-response.txt"
                  (if haddockSupportsUTF8 then Just utf8 else Nothing)
                  renderedArgs
                  (\responseFileName -> k ["@" ++ responseFileName] result)
              else k renderedArgs result
  case (argPrologueFile args, argPrologue args) of
    (Flag pfile, _) ->
      withPrologueArgs ["--prologue=" ++ pfile]
    (_, Flag prologueText) ->
      withTempFileEx tmpFileOpts mbWorkDir outputDir "haddock-prologue.txt" $
        \prologueFileName h -> do
          when haddockSupportsUTF8 (hSetEncoding h utf8)
          hPutStrLn h prologueText
          hClose h
          withPrologueArgs ["--prologue=" ++ u prologueFileName]
    (NoFlag, NoFlag) ->
      withPrologueArgs []
  where
    -- See Note [Symbolic paths] in Distribution.Utils.Path
    i = interpretSymbolicPath mbWorkDir
    u :: SymbolicPath Pkg to -> FilePath
    u = interpretSymbolicPathCWD

    outputDir = coerceSymbolicPath $ unDir $ argOutputDir args
    isNotArgContents = isNothing (flagToMaybe $ argContents args)
    isNotArgIndex = isNothing (flagToMaybe $ argIndex args)
    isArgGenIndex = fromFlagOrDefault False (argGenIndex args)
    -- Haddock, when generating HTML, does not generate an index if the options
    -- --use-contents or --use-index are passed to it. See
    -- https://haskell-haddock.readthedocs.io/en/latest/invoking.html#cmdoption-use-contents
    isIndexGenerated = isArgGenIndex && isNotArgContents && isNotArgIndex
    result =
      intercalate ", "
        . map
          ( \o ->
              i outputDir
                </> case o of
                  Html
                    | isIndexGenerated ->
                        "index.html"
                  Html
                    | otherwise ->
                        mempty
                  Hoogle -> pkgstr <.> "txt"
          )
        . fromFlagOrDefault [Html]
        . argOutput
        $ args
      where
        pkgstr = prettyShow $ packageName pkgid
        pkgid = arg argPackageName
    arg f = fromFlag $ f args

renderPureArgs :: Version -> Compiler -> Platform -> HaddockArgs -> [String]
renderPureArgs version comp platform args =
  concat
    [ map (\f -> "--dump-interface=" ++ u (unDir (argOutputDir args)) </> f)
        . flagToList
        . argInterfaceFile
        $ args
    , if haddockSupportsPackageName
        then
          maybe
            []
            ( \pkg ->
                [ "--package-name="
                    ++ case argComponentName args of
                      Flag name -> name
                      _ -> prettyShow (pkgName pkg)
                , "--package-version=" ++ prettyShow (pkgVersion pkg)
                ]
            )
            . flagToMaybe
            . argPackageName
            $ args
        else []
    , ["--since-qual=external" | isVersion 2 20]
    , [ "--quickjump" | isVersion 2 19, True <- flagToList . argQuickJump $ args
      ]
    , ["--hyperlinked-source" | isHyperlinkedSource]
    , (\(All b, xs) -> bool [] (map (("--hide=" ++) . prettyShow) xs) b)
        . argHideModules
        $ args
    , bool [] ["--ignore-all-exports"] . getAny . argIgnoreExports $ args
    , -- Haddock's --source-* options are ignored once --hyperlinked-source is
      -- set.
      -- See https://haskell-haddock.readthedocs.io/en/latest/invoking.html#cmdoption-hyperlinked-source
      -- To avoid Haddock's warning, we only set --source-* options if
      -- --hyperlinked-source is not set.
      if isHyperlinkedSource
        then []
        else
          maybe
            []
            ( \(m, e, l) ->
                [ "--source-module=" ++ m
                , "--source-entity=" ++ e
                ]
                  ++ if isVersion 2 14
                    then ["--source-entity-line=" ++ l]
                    else []
            )
            . flagToMaybe
            . argLinkSource
            $ args
    , maybe [] ((: []) . ("--css=" ++)) . flagToMaybe . argCssFile $ args
    , maybe [] ((: []) . ("--use-contents=" ++)) . flagToMaybe . argContents $ args
    , bool [] ["--gen-contents"] . fromFlagOrDefault False . argGenContents $ args
    , maybe [] ((: []) . ("--use-index=" ++)) . flagToMaybe . argIndex $ args
    , bool [] ["--gen-index"] . fromFlagOrDefault False . argGenIndex $ args
    , maybe [] ((: []) . ("--base-url=" ++)) . flagToMaybe . argBaseUrl $ args
    , bool [verbosityFlag] [] . getAny . argVerbose $ args
    , map (\o -> case o of Hoogle -> "--hoogle"; Html -> "--html")
        . fromFlagOrDefault []
        . argOutput
        $ args
    , renderInterfaces . argInterfaces $ args
    , (: []) . ("--odir=" ++) . u . unDir . argOutputDir $ args
    , maybe
        []
        ( (: [])
            . ("--title=" ++)
            . ( bool
                  id
                  (++ " (internal documentation)")
                  (getAny $ argIgnoreExports args)
              )
        )
        . flagToMaybe
        . argTitle
        $ args
    , [ "--optghc=" ++ opt | let opts = argGhcOptions args, opt <- renderGhcOptions comp platform opts
      ]
    , maybe [] (\l -> ["-B" ++ l]) $
        flagToMaybe (argGhcLibDir args) -- error if Nothing?
    , -- https://github.com/haskell/haddock/pull/547
      [ "--reexport=" ++ prettyShow r
      | r <- argReexports args
      , isVersion 2 19
      ]
    , argTargets $ args
    , maybe [] ((: []) . (resourcesDirFlag ++)) . flagToMaybe . argResourcesDir $ args
    , -- Do not re-direct compilation output to a temporary directory (--no-tmp-comp-dir)
      -- We pass this option by default to haddock to avoid recompilation
      -- See Note [Hi Haddock Recompilation Avoidance]
      ["--no-tmp-comp-dir" | version >= mkVersion [2, 28, 0]]
    , bool [] ["--use-unicode"] . fromFlagOrDefault False . argUseUnicode $ args
    ]
  where
    -- See Note [Symbolic paths] in Distribution.Utils.Path
    u = interpretSymbolicPathCWD
    renderInterfaces = map renderInterface

    renderInterface :: (FilePath, Maybe FilePath, Maybe FilePath, Visibility) -> String
    renderInterface (i, html, hypsrc, visibility) =
      "--read-interface="
        ++ intercalate
          ","
          ( concat
              [ [fromMaybe "" html]
              , -- only render hypsrc path if html path
                -- is given and hyperlinked-source is
                -- enabled

                [ case (html, hypsrc) of
                    (Nothing, _) -> ""
                    (_, Nothing) -> ""
                    (_, Just x)
                      | isVersion 2 17
                      , fromFlagOrDefault False . argLinkedSource $ args ->
                          x
                      | otherwise ->
                          ""
                ]
              , if haddockSupportsVisibility
                  then
                    [ case visibility of
                        Visible -> "visible"
                        Hidden -> "hidden"
                    ]
                  else []
              , [i]
              ]
          )

    isVersion major minor = version >= mkVersion [major, minor]
    verbosityFlag
      | isVersion 2 5 = "--verbosity=1"
      | otherwise = "--verbose"
    resourcesDirFlag
      | isVersion 2 29 = "--resources-dir="
      | otherwise = "--lib="
    haddockSupportsVisibility = version >= mkVersion [2, 26, 1]
    haddockSupportsPackageName = version > mkVersion [2, 16]
    haddockSupportsHyperlinkedSource = isVersion 2 17
    isHyperlinkedSource =
      haddockSupportsHyperlinkedSource
        && fromFlagOrDefault False (argLinkedSource args)

---------------------------------------------------------------------------------

-- | Given a list of 'InstalledPackageInfo's, return a list of interfaces and
-- HTML paths, and an optional warning for packages with missing documentation.
haddockPackagePaths
  :: [InstalledPackageInfo]
  -> Maybe (InstalledPackageInfo -> FilePath)
  -> IO
      ( [ ( FilePath -- path to interface
      -- file
          , Maybe FilePath -- url to html
          -- documentation
          , Maybe FilePath -- url to hyperlinked
          -- source
          , Visibility
          )
        ]
      , Maybe String -- warning about
      -- missing documentation
      )
haddockPackagePaths ipkgs mkHtmlPath = do
  interfaces <-
    sequenceA
      [ case interfaceAndHtmlPath ipkg of
        Nothing -> do
          return (Left (packageId ipkg))
        Just (interface, html) -> do
          (html', hypsrc') <-
            case html of
              Just htmlPath -> do
                let hypSrcPath = htmlPath </> defaultHyperlinkedSourceDirectory
                hypSrcExists <- doesDirectoryExist hypSrcPath
                return $
                  ( Just (fixFileUrl htmlPath)
                  , if hypSrcExists
                      then Just (fixFileUrl hypSrcPath)
                      else Nothing
                  )
              Nothing -> return (Nothing, Nothing)

          exists <- doesFileExist interface
          if exists
            then return (Right (interface, html', hypsrc', Visible))
            else return (Left pkgid)
      | ipkg <- ipkgs
      , let pkgid = packageId ipkg
      , pkgName pkgid `notElem` noHaddockWhitelist
      ]

  let missing = [pkgid | Left pkgid <- interfaces]
      warning =
        "The documentation for the following packages are not "
          ++ "installed. No links will be generated to these packages: "
          ++ intercalate ", " (map prettyShow missing)
      flags = rights interfaces

  return (flags, if null missing then Nothing else Just warning)
  where
    -- Don't warn about missing documentation for these packages. See #1231.
    noHaddockWhitelist = map mkPackageName ["rts"]

    -- Actually extract interface and HTML paths from an 'InstalledPackageInfo'.
    interfaceAndHtmlPath
      :: InstalledPackageInfo
      -> Maybe (FilePath, Maybe FilePath)
    interfaceAndHtmlPath pkg = do
      interface <- listToMaybe (InstalledPackageInfo.haddockInterfaces pkg)
      html <- case mkHtmlPath of
        Nothing -> listToMaybe (InstalledPackageInfo.haddockHTMLs pkg)
        Just mkPath -> Just (mkPath pkg)
      return (interface, if null html then Nothing else Just html)

    -- The 'haddock-html' field in the hc-pkg output is often set as a
    -- native path, but we need it as a URL. See #1064. Also don't "fix"
    -- the path if it is an interpolated one.
    fixFileUrl f
      | Nothing <- mkHtmlPath
      , isAbsolute f =
          "file://" ++ f
      | otherwise = f

    -- 'src' is the default hyperlinked source directory ever since. It is
    -- not possible to configure that directory in any way in haddock.
    defaultHyperlinkedSourceDirectory = "src"

haddockPackageFlags
  :: Verbosity
  -> LocalBuildInfo
  -> ComponentLocalBuildInfo
  -> Maybe PathTemplate
  -> IO
      ( [ ( FilePath -- path to interface
      -- file
          , Maybe FilePath -- url to html
          -- documentation
          , Maybe FilePath -- url to hyperlinked
          -- source
          , Visibility
          )
        ]
      , Maybe String -- warning about
      -- missing documentation
      )
haddockPackageFlags verbosity lbi clbi htmlTemplate = do
  let allPkgs = installedPkgs lbi
      directDeps = map fst (componentPackageDeps clbi)
  transitiveDeps <- case PackageIndex.dependencyClosure allPkgs directDeps of
    Left x -> return x
    Right inf ->
      dieWithException verbosity $ HaddockPackageFlags inf

  haddockPackagePaths (PackageIndex.allPackages transitiveDeps) mkHtmlPath
  where
    mkHtmlPath = fmap expandTemplateVars htmlTemplate
    expandTemplateVars tmpl pkg =
      fromPathTemplate . substPathTemplate (env pkg) $ tmpl
    env pkg = haddockTemplateEnv lbi (packageId pkg)

haddockTemplateEnv :: LocalBuildInfo -> PackageIdentifier -> PathTemplateEnv
haddockTemplateEnv lbi pkg_id =
  (PrefixVar, prefix (installDirTemplates lbi))
    -- We want the legacy unit ID here, because it gives us nice paths
    -- (Haddock people don't care about the dependencies)
    : initialPathTemplateEnv
      pkg_id
      (mkLegacyUnitId pkg_id)
      (compilerInfo (compiler lbi))
      (hostPlatform lbi)

-- ------------------------------------------------------------------------------
-- hscolour support.

hscolour
  :: PackageDescription
  -> LocalBuildInfo
  -> [PPSuffixHandler]
  -> HscolourFlags
  -> IO ()
hscolour = hscolour_setupHooks noBuildHooks

hscolour_setupHooks
  :: BuildHooks
  -> PackageDescription
  -> LocalBuildInfo
  -> [PPSuffixHandler]
  -> HscolourFlags
  -> IO ()
hscolour_setupHooks setupHooks =
  hscolour' setupHooks dieNoVerbosity ForDevelopment

hscolour'
  :: BuildHooks
  -> (String -> IO ())
  -- ^ Called when the 'hscolour' exe is not found.
  -> HaddockTarget
  -> PackageDescription
  -> LocalBuildInfo
  -> [PPSuffixHandler]
  -> HscolourFlags
  -> IO ()
hscolour'
  (BuildHooks{preBuildComponentRules = mbPbcRules})
  onNoHsColour
  haddockTarget
  pkg_descr
  lbi
  suffixes
  flags =
    either (\excep -> onNoHsColour $ exceptionMessage excep) (\(hscolourProg, _, _) -> go hscolourProg)
      =<< lookupProgramVersion
        verbosity
        hscolourProgram
        (orLaterVersion (mkVersion [1, 8]))
        (withPrograms lbi)
    where
      common = hscolourCommonFlags flags
      verbosity = fromFlag $ setupVerbosity common
      distPref = fromFlag $ setupDistPref common
      mbWorkDir = mbWorkDirLBI lbi
      i = interpretSymbolicPathLBI lbi -- See Note [Symbolic paths] in Distribution.Utils.Path
      u :: SymbolicPath Pkg to -> FilePath
      u = interpretSymbolicPathCWD

      go :: ConfiguredProgram -> IO ()
      go hscolourProg = do
        warn verbosity $
          "the 'cabal hscolour' command is deprecated in favour of 'cabal "
            ++ "haddock --hyperlink-source' and will be removed in the next major "
            ++ "release."

        setupMessage verbosity "Running hscolour for" (packageId pkg_descr)
        createDirectoryIfMissingVerbose verbosity True $
          i $
            hscolourPref haddockTarget distPref pkg_descr

        withAllComponentsInBuildOrder pkg_descr lbi $ \comp clbi -> do
          let tgt = TargetInfo clbi comp
              runPreBuildHooks :: LocalBuildInfo -> TargetInfo -> IO ()
              runPreBuildHooks lbi2 target =
                let inputs =
                      SetupHooks.PreBuildComponentInputs
                        { SetupHooks.buildingWhat = BuildHscolour flags
                        , SetupHooks.localBuildInfo = lbi2
                        , SetupHooks.targetInfo = target
                        }
                 in for_ mbPbcRules $ \pbcRules -> do
                      (ruleFromId, _mons) <- SetupHooks.computeRules verbosity inputs pbcRules
                      SetupHooks.executeRules verbosity lbi2 tgt ruleFromId
          preBuildComponent runPreBuildHooks verbosity lbi tgt
          preprocessComponent pkg_descr comp lbi clbi False verbosity suffixes
          let
            doExe com = case (compToExe com) of
              Just exe -> do
                let outputDir =
                      hscolourPref haddockTarget distPref pkg_descr
                        </> makeRelativePathEx (unUnqualComponentName (exeName exe) </> "src")
                runHsColour hscolourProg outputDir =<< getExeSourceFiles verbosity lbi exe clbi
              Nothing -> do
                warn verbosity "Unsupported component, skipping..."
                return ()
          case comp of
            CLib lib -> do
              let outputDir = hscolourPref haddockTarget distPref pkg_descr </> makeRelativePathEx "src"
              runHsColour hscolourProg outputDir =<< getLibSourceFiles verbosity lbi lib clbi
            CFLib flib -> do
              let outputDir =
                    hscolourPref haddockTarget distPref pkg_descr
                      </> makeRelativePathEx
                        ( unUnqualComponentName (foreignLibName flib)
                            </> "src"
                        )
              runHsColour hscolourProg outputDir =<< getFLibSourceFiles verbosity lbi flib clbi
            CExe _ -> when (fromFlag (hscolourExecutables flags)) $ doExe comp
            CTest _ -> when (fromFlag (hscolourTestSuites flags)) $ doExe comp
            CBench _ -> when (fromFlag (hscolourBenchmarks flags)) $ doExe comp

      stylesheet = flagToMaybe (hscolourCSS flags)

      runHsColour
        :: ConfiguredProgram
        -> SymbolicPath Pkg to
        -> [(ModuleName.ModuleName, SymbolicPath Pkg to1)]
        -> IO ()
      runHsColour prog outputDir moduleFiles = do
        createDirectoryIfMissingVerbose verbosity True (i outputDir)

        case stylesheet of -- copy the CSS file
          Nothing
            | programVersion prog >= Just (mkVersion [1, 9]) ->
                runProgramCwd
                  verbosity
                  mbWorkDir
                  prog
                  ["-print-css", "-o" ++ u outputDir </> "hscolour.css"]
            | otherwise -> return ()
          Just s -> copyFileVerbose verbosity s (i outputDir </> "hscolour.css")

        for_ moduleFiles $ \(m, inFile) ->
          runProgramCwd
            verbosity
            mbWorkDir
            prog
            ["-css", "-anchor", "-o" ++ outFile m, u inFile]
        where
          outFile m =
            i outputDir
              </> intercalate "-" (ModuleName.components m)
                <.> "html"

haddockToHscolour :: HaddockFlags -> HscolourFlags
haddockToHscolour flags =
  HscolourFlags
    { hscolourCommonFlags = haddockCommonFlags flags
    , hscolourCSS = haddockHscolourCss flags
    , hscolourExecutables = haddockExecutables flags
    , hscolourTestSuites = haddockTestSuites flags
    , hscolourBenchmarks = haddockBenchmarks flags
    , hscolourForeignLibs = haddockForeignLibs flags
    }

-- ------------------------------------------------------------------------------
-- Boilerplate Monoid instance.
instance Monoid HaddockArgs where
  mempty = gmempty
  mappend = (<>)

instance Semigroup HaddockArgs where
  (<>) = gmappend

instance Monoid Directory where
  mempty = Dir "."
  mappend = (<>)

instance Semigroup Directory where
  Dir m <> Dir n = Dir $ m </> n
