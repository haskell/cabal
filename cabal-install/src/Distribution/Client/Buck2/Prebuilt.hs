-- | Turns the already-resolved, already-built dependency closure of the
-- project (everything @cabal buck2@ just built via @--only-dependencies@)
-- into @third-party\/haskell@: a @haskell_prebuilt_library()@ per package,
-- a filtered\/recached package db holding just the @.conf@ files those
-- rules reference, and repo-relative symlinks to GHC and the cabal store
-- so the generated paths aren't host-specific.
--
-- This is an in-process rewrite of what used to be the external
-- @buck2\/gen-haskell-prebuilt.py@ script. Doing it here instead means:
--
--   * no re-deriving GHC's version\/paths from @dist-newstyle\/cache\/
--     plan.json@ - they're already sitting in the 'ElaboratedSharedConfig'
--     \/'CabalDirLayout' this command already elaborated.
--   * no shelling out to a (possibly different-version) @cabal@ on
--     @$PATH@ - which used to cause index-cache parse errors when it
--     didn't match the @cabal@ actually running.
--   * no globbing for the store root's ABI-tag suffix - 'storeDirectory'
--     computes it exactly.
--   * the dependency closure comes from 'elaboratedPlanToExecute' - the
--     exact, already test\/benchmark-flag-pruned plan @cabal buck2@ just
--     built - rather than re-walking every local component's
--     @build-depends@ unconditionally (which used to pull in test-only
--     dependencies that were never actually built when
--     @--enable-tests@ wasn't passed, producing spurious "not found"
--     warnings).
module Distribution.Client.Buck2.Prebuilt
  ( generatePrebuilt
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import qualified Data.ByteString as BS
import Data.Char (isHexDigit)
import Data.List (stripPrefix)
import qualified Data.Map as Map
import qualified Data.Set as Set

import System.Directory
  ( createDirectoryIfMissing
  , createFileLink
  , doesFileExist
  , doesPathExist
  , removeDirectoryRecursive
  , removeFile
  )
import System.FilePath
  ( isAbsolute
  , isPathSeparator
  , joinPath
  , makeRelative
  , pathSeparator
  , splitDirectories
  , splitPath
  , takeDirectory
  , (<.>)
  , (</>)
  )

import qualified Distribution.Client.InstallPlan as InstallPlan

import Distribution.Client.DistDirLayout
  ( CabalDirLayout (cabalStoreDirLayout)
  , DistDirLayout (distBuildDirectory, distDirectory)
  , StoreDirLayout (storeDirectory, storePackageDBPath)
  )
import Distribution.Client.ProjectPlanning
  ( BuildStyle (BuildAndInstall)
  , ElaboratedConfiguredPackage (elabBuildStyle, elabInstallDirs, elabLocalToProject)
  , ElaboratedInstallPlan
  , ElaboratedSharedConfig (pkgConfigCompiler, pkgConfigCompilerProgs, pkgConfigPlatform)
  )
import Distribution.Client.ProjectPlanning.Types (elabComponentName, elabDistDirParams)
import Distribution.Types.ComponentName (ComponentName (CExeName))

import Distribution.InstalledPackageInfo (parseInstalledPackageInfo)
import Distribution.Package (HasUnitId (installedUnitId), packageName, packageVersion)
import Distribution.Simple.BuildPaths (exeExtension)
import Distribution.Types.PackageName (PackageName)
import Distribution.Types.UnqualComponentName (UnqualComponentName, unUnqualComponentName)
import Distribution.Simple.Compiler
  ( Compiler (compilerId, compilerProperties)
  , compilerVersion
  )
import Distribution.Simple.GHC (getGlobalPackageDB)
import qualified Distribution.Simple.InstallDirs as InstallDirs
import Distribution.Simple.Program.Builtin (ghcPkgProgram, ghcProgram)
import Distribution.Simple.Program.Db (lookupProgram)
import Distribution.Simple.Program.Types (ConfiguredProgram, programPath)
import Distribution.Simple.Utils (dieWithException, notice, ordNub, rawSystemExit, warn)
import Distribution.Types.InstalledPackageInfo
  ( InstalledPackageInfo
      ( depends
      , extraLibraries
      , hsLibraries
      , includeDirs
      , libraryDirs
      , libraryDynDirs
      , sourceLibName
      )
  )
import Distribution.Types.UnitId (UnitId, unUnitId)

import Distribution.Client.Errors
  ( CabalInstallException (Buck2NoGhcPkgProgram, Buck2NoGhcProgram)
  )

import Distribution.Client.Buck2.CabalToBuck (libTargetName)
import Distribution.Client.Buck2.Starlark

-- | Generate\/refresh @third-party\/haskell@ from the dependency closure
-- of an already-built, already-pruned install plan. Returns:
--
--   * The name of every @wantedBuildTools@ pair this project's own
--     dependency closure resolved a real binary for - each one also
--     gets a real @export_file()@ target in the generated
--     @third-party\/haskell\/BUCK@ (@\/\/third-party\/haskell:\<name\>-exe@),
--     for "Distribution.Client.Buck2.CabalToBuck" to reference from a
--     component's own @build_tool_depends@ (see that module's own
--     @buildToolDependsArg@) - a real buck2 dependency edge, not a
--     host-filesystem symlink farm resolved once at generation time (the
--     wanted pair's own *local*-package half, if any, is a completely
--     separate case handled entirely by 'CabalToBuck' itself, referencing
--     that package's own real, already-generated @haskell_binary()@
--     target directly - this function only ever sees, and only needs to
--     handle, already-installed *external* dependencies).
--   * The real, already-parsed 'InstalledPackageInfo' for every resolved
--     dependency (local\/quasi-local packages included, per
--     'localUnitIds's own haddock) - callers that need a real
--     'Distribution.Simple.PackageIndex.InstalledPackageIndex' (e.g. to
--     build a genuine 'LocalBuildInfo' via "Distribution.Client.InLibrary",
--     the way "Distribution.Client.CmdBuck2" does) can build one directly
--     from this via 'PackageIndex.fromList' without a second, independent
--     walk of the same @.conf@ files.
generatePrebuilt
  :: Verbosity
  -> FilePath
  -- ^ project root (the buck2 cell root)
  -> CabalDirLayout
  -> DistDirLayout
  -> ElaboratedSharedConfig
  -> ElaboratedInstallPlan
  -- ^ 'elaboratedPlanToExecute': already pruned to exactly the (test\/
  -- benchmark-flag-aware) dependency closure that was just built. Local
  -- packages are *usually* not in this plan - except when
  -- 'CmdBuck2.pruneToDependenciesNeeded' had to keep one in because
  -- something else (e.g. another local package's Custom Setup.hs) needed
  -- it - which is filtered back out below, since a kept-in local package
  -- already gets a real 'haskell_library()' from
  -- "Distribution.Client.Buck2.Generate", not a prebuilt one here.
  -> [(PackageName, UnqualComponentName)]
  -- ^ Every @pkg:exe@ named in any local component's own
  -- @build-tool-depends:@, across the whole project - see this
  -- function's own return-value haddock above.
  -> IO (Set String, [InstalledPackageInfo])
generatePrebuilt verbosity projectRoot cabalDirLayout distDirLayout shared depsPlan wantedBuildTools = do
  ghcProg <-
    maybe (dieWithException verbosity Buck2NoGhcProgram) return $
      lookupProgram ghcProgram (pkgConfigCompilerProgs shared)
  ghcPkgProg <-
    maybe (dieWithException verbosity Buck2NoGhcPkgProgram) return $
      lookupProgram ghcPkgProgram (pkgConfigCompilerProgs shared)

  let compiler = pkgConfigCompiler shared
      ghcVersionStr = prettyShow (compilerVersion compiler)
      ghcDynamic = Map.lookup "GHC Dynamic" (compilerProperties compiler) == Just "YES"
      storeLayout = cabalStoreDirLayout cabalDirLayout
      storeDB = storePackageDBPath storeLayout compiler
      storeRootAbs = storeDirectory storeLayout compiler
      -- Mirrors DistDirLayout's own (unexported) distPackageDBPath: the
      -- package db every *inplace*-built package - every local package,
      -- plus any non-local one that itself ends up depending on a local
      -- package (see 'pruneToDependenciesNeeded's own haddock) - gets
      -- registered into.
      inplaceDB = distDirectory distDirLayout </> "packagedb" </> prettyShow (compilerId compiler)
      targetDir = projectRoot </> "third-party" </> "haskell"
      targetStoreDB = targetDir </> "store-db"
      ghcBinAbs = takeDirectory (programPath ghcProg)

  globalDB <- getGlobalPackageDB verbosity ghcProg
  let globalRootAbs = takeDirectory globalDB

      -- Every non-local package's elaborated node, keyed by unit id -
      -- needed both to tell local packages apart from non-local ones
      -- (below) and, for a non-local-but-inplace package, to derive its
      -- real build directory (see 'inplaceExtraRoots').
      elabByUnit =
        Map.fromList
          [ (installedUnitId elab, elab)
          | pkg <- InstallPlan.toList depsPlan
          , Just elab <- [configuredOrInstalled pkg]
          ]
      allUnitIds = ordNub [installedUnitId pkg | pkg <- InstallPlan.toList depsPlan]
      -- A *local* unit id can genuinely turn up in 'depsPlan' (see this
      -- function's own haddock on 'pruneToDependenciesNeeded' kicking
      -- in) - its own real @haskell_library()@ already comes from
      -- "Distribution.Client.Buck2.Generate", so it must never also get
      -- a @haskell_prebuilt_library()@ rule here. But its @.conf@ still
      -- needs to be *registered* (just not exposed as a rule): a non-
      -- local package that itself depends on it (e.g. hackage-security,
      -- via its own @cabal-syntax@ flag, on the local in-tree Cabal-
      -- syntax) has a real compiled interface whose own @depends:@
      -- names that local package's unit id directly, and ghc-pkg's
      -- dependency-closure check for *that* package fails outright
      -- ("cannot satisfy ...: unusable due to missing dependencies") if
      -- nothing registers it anywhere - independent of whether anything
      -- ever actually exposes or links against this registration
      -- directly (nothing does: every real consumer depends on the
      -- local package via its own buck2 target instead).
      --
      -- Also excludes any non-local package whose own build is forced
      -- inplace by depending on a local one (e.g. hackage-security
      -- itself) - "Distribution.Client.CmdBuck2" gives these a real
      -- buck2 rule from their own source too (the same reasoning as for
      -- a genuinely local package: an inplace package is compiled
      -- directly against whatever its dependencies actually were at
      -- that build, so reusing its already-compiled interface here -
      -- built against the *original*, non-buck2 copy of whatever local
      -- package it depends on - would leave GHC with two nominally
      -- distinct, incompatible copies of that local package's types in
      -- the one build). Must match 'CmdBuck2.buck2Action's own widened
      -- "is this local-ish" predicate exactly, or a package would get
      -- both a prebuilt rule here *and* a real one from Generate.hs -
      -- buck2 rejects the resulting duplicate target outright.
      localUnitIds =
        Set.fromList
          [ installedUnitId pkg
          | pkg <- InstallPlan.toList depsPlan
          , Just elab <- [configuredOrInstalled pkg]
          , elabLocalToProject elab || elabBuildStyle elab /= BuildAndInstall
          ]
      -- A package built inplace (see 'inplaceDB' above) - local or not -
      -- has no stable install location the way a store package does -
      -- its library-dirs, read from its own minimal in-tree .conf, come
      -- back empty. Its real build output lives under a per-package
      -- directory 'distBuildDirectory' can compute exactly (from the
      -- same elaborated node), so that's symlinked in individually
      -- instead of trying to find one shared anchor for every such
      -- package, and used directly (see 'readPackage') in place of the
      -- .conf's own (empty) library-dirs. Not that it matters for a
      -- *local* unit id's own libraries specifically - nothing here ever
      -- links against them, only against its real buck2 target - but
      -- computing this uniformly over every inplace id (rather than
      -- special-casing local ones out) costs nothing and stays correct
      -- if that ever changes.
      inplaceBuildDirs =
        Map.fromList
          [ (uid, distBuildDirectory distDirLayout (elabDistDirParams shared elab) </> "build")
          | uid <- allUnitIds
          , classifyUnitId uid == InplaceDb
          , Just elab <- [Map.lookup uid elabByUnit]
          ]
      inplaceExtraRoots = [(dir, "inplace" </> unUnitId uid) | (uid, dir) <- Map.toList inplaceBuildDirs]
      paths =
        RepoPaths
          { rpGhcVersion = ghcVersionStr
          , rpGlobalRootAbs = globalRootAbs
          , rpStoreRootAbs = storeRootAbs
          , rpExtraRoots = inplaceExtraRoots
          }

  createDirectoryIfMissing True targetDir
  ensureSymlink (targetDir </> ("ghc-" ++ ghcVersionStr)) globalRootAbs
  ensureSymlink (targetDir </> "ghc-bin") ghcBinAbs
  for_ inplaceExtraRoots $ \(absDir, relName) -> do
    createDirectoryIfMissing True (takeDirectory (targetDir </> relName))
    ensureSymlink (targetDir </> relName) absDir

  notice verbosity "cabal buck2: resolving prebuilt dependency closure"
  allResolved <- catMaybes <$> traverse (readPackage verbosity paths storeDB inplaceDB inplaceBuildDirs) allUnitIds
  -- Registered (for ghc-pkg's own dependency-closure check - see
  -- 'localUnitIds's haddock) but never turned into a rule: a local
  -- package already gets a real one from "Distribution.Client.Buck2.Generate".
  let packages = filter (\p -> rpUnitId p `Set.notMember` localUnitIds) allResolved
      alexPath = findToolBinary paths shared depsPlan "alex"
      happyPath = findToolBinary paths shared depsPlan "happy"
      -- Every @build-tool-depends: pkg:exe@ pair the *external*
      -- (already-installed) dependency closure can resolve a real
      -- binary for - a local-package pair is never in `depsPlan` at all
      -- (see this function's own haddock on that parameter), so this
      -- silently, correctly resolves to nothing for one; entirely
      -- 'CabalToBuck's own job to notice that case and reference the
      -- local package's real target directly instead.
      buildToolPaths =
        Map.fromList
          [ (unUnqualComponentName exeName, path)
          | (pn, exeName) <- ordNub wantedBuildTools
          , Just path <- [findExeBinaryAbs shared depsPlan pn exeName]
          ]

  -- Needed by any package's own library files, *and* independently by
  -- alex/happy's/any build-tool-depends executable's own binary path -
  -- a build-tool-only dependency (an executable, no library)
  -- contributes no ResolvedPackage at all (see readPackage), so
  -- checking `packages` alone would miss a project that needs alex/
  -- happy/a preprocessor tool but nothing else store-installed.
  when
    ( any ((== StoreDb) . rpDbKind) packages
        || any inStore (alexPath : happyPath : map (toRepoRelative paths) (Map.elems buildToolPaths))
    )
    $ ensureSymlink (targetDir </> "cabal-store") storeRootAbs

  notice verbosity "cabal buck2: building filtered store package db"
  setupStoreDB verbosity ghcPkgProg targetStoreDB allResolved

  notice verbosity "cabal buck2: generating third-party/haskell/BUCK"
  writeBuckFile targetDir paths packages buildToolPaths

  writeToolsFile targetDir ghcVersionStr ghcDynamic alexPath happyPath

  return (Map.keysSet buildToolPaths, map rpInfo allResolved)

-- | The repo-relative anchors every generated path is expressed against:
-- the symlinks 'generatePrebuilt' just created, plus the GHC version
-- string (needed for both the global db's own relative path and
-- shared-library sonames). 'rpExtraRoots' holds one more anchor per
-- non-local-but-inplace package (see 'generatePrebuilt's own haddock on
-- 'inplaceExtraRoots') - empty for the overwhelming majority of projects,
-- which have none of those.
data RepoPaths = RepoPaths
  { rpGhcVersion :: String
  , rpGlobalRootAbs :: FilePath
  , rpStoreRootAbs :: FilePath
  , rpExtraRoots :: [(FilePath, FilePath)]
  }

-- | Which of the (now three) package dbs a unit id's @.conf@ lives in -
-- decided entirely by the unit id's own shape, no searching required
-- (unlike the Python predecessor, which had to glob for the store root,
-- not knowing its ABI-tag suffix in advance): GHC's global\/boot packages
-- have a plain @name-version@ id; a package installed to the store gets a
-- hash-suffixed one; a package built @inplace@ - every local package,
-- plus any non-local one that itself ends up depending on a local
-- package (see 'pruneToDependenciesNeeded's own haddock in CmdBuck2) -
-- gets an @-inplace@-suffixed one.
data PkgDbKind = GlobalDb | StoreDb | InplaceDb
  deriving (Eq)

classifyUnitId :: UnitId -> PkgDbKind
classifyUnitId uid
  | "-inplace" `isSuffixOf` s = InplaceDb
  | hasStoreHashSuffix s = StoreDb
  | otherwise = GlobalDb
  where
    s = unUnitId uid
    hasStoreHashSuffix rs = case break (== '-') (reverse rs) of
      (revSuffix, '-' : _) -> length revSuffix >= 20 && all isHexDigit revSuffix
      _ -> False

-- | A resolved package: its metadata plus which package db it came from
-- (needed to pick the right @db =@ value in the generated rule, and to
-- know where to find its @.conf@ file to symlink into the filtered store
-- db).
data ResolvedPackage = ResolvedPackage
  { rpInfo :: InstalledPackageInfo
  , rpUnitId :: UnitId
  , rpDbKind :: PkgDbKind
  , rpConfPath :: FilePath
  , rpStaticLibs :: [FilePath]
  , rpProfiledLibs :: [FilePath]
  , rpSharedLibs :: [(String, FilePath)]
  }

confPath :: FilePath -> FilePath -> FilePath -> UnitId -> FilePath
confPath globalRootAbs storeDB inplaceDB uid = case classifyUnitId uid of
  GlobalDb -> globalRootAbs </> "package.conf.d" </> unUnitId uid <.> "conf"
  StoreDb -> storeDB </> unUnitId uid <.> "conf"
  InplaceDb -> inplaceDB </> unUnitId uid <.> "conf"

readPackage :: Verbosity -> RepoPaths -> FilePath -> FilePath -> Map UnitId FilePath -> UnitId -> IO (Maybe ResolvedPackage)
readPackage verbosity paths storeDB inplaceDB inplaceBuildDirs uid = do
  let dbKind = classifyUnitId uid
      path = confPath (rpGlobalRootAbs paths) storeDB inplaceDB uid
  exists <- doesFileExist path
  if not exists
    then do
      warn verbosity $ "cabal buck2: no .conf file for " ++ unUnitId uid ++ " (expected at " ++ path ++ ")"
      return Nothing
    else do
      contents <- BS.readFile path
      case parseInstalledPackageInfo contents of
        Left errs -> do
          warn verbosity $ "cabal buck2: couldn't parse " ++ path ++ ": " ++ intercalate "; " (toList errs)
          return Nothing
        Right (_warnings, ipi0) -> do
          -- '${pkgroot}' is the directory *containing* the package db (one
          -- level above the .conf file's own db directory, e.g.
          -- ".../lib" for a ".../lib/package.conf.d/<uid>.conf" file) -
          -- not the db directory itself.
          let munged = mungePkgroot (takeDirectory (takeDirectory path)) ipi0
              -- An inplace package's own .conf (unlike a store/global
              -- one) never has real library-dirs at all - Cabal derives
              -- its actual build output location separately, computed
              -- from the elaborated plan into 'inplaceBuildDirs' up in
              -- 'generatePrebuilt' (the same directory just symlinked in
              -- as this unit's own 'rpExtraRoots' entry), not from
              -- anything baked into the .conf file itself.
              ipi = case Map.lookup uid inplaceBuildDirs of
                Just dir -> munged{libraryDirs = [dir], libraryDynDirs = [dir]}
                Nothing -> munged
              isRts = prettyShow (packageName ipi) == "rts"
          staticLibs <- findLibs paths (libraryDirs ipi) ["lib" ++ stem <.> "a" | stem <- hsLibraries ipi]
          -- GHC doesn't build profiled RTS libraries the normal way - see
          -- gen-haskell-prebuilt.py's own note on this, which this
          -- inherits without fully understanding why either.
          profiledLibs <-
            if isRts
              then return []
              else findLibs paths (libraryDirs ipi) ["lib" ++ stem ++ "_p" <.> "a" | stem <- hsLibraries ipi]
          sharedLibs <- findSharedLibs paths ipi
          return $ Just (ResolvedPackage ipi uid dbKind path staticLibs profiledLibs sharedLibs)

-- | Resolve each candidate filename against @dirs@ in turn, keeping only
-- the ones that actually exist on disk (unlike gen-haskell-prebuilt.py's
-- Python predecessor, this doesn't assume every @hs-libraries@ stem has a
-- library of every flavour - e.g. rts's second stem, \"Cffi\", only ever
-- ships as a static archive, never as a @.so@).
findLibs :: RepoPaths -> [FilePath] -> [String] -> IO [FilePath]
findLibs paths dirs fnames = catMaybes <$> traverse (findOne paths dirs) fnames

findOne :: RepoPaths -> [FilePath] -> String -> IO (Maybe FilePath)
findOne paths dirs fname = go dirs
  where
    go [] = return Nothing
    go (d : ds) = do
      let absPath = d </> fname
      exists <- doesFileExist absPath
      if exists
        then return (toRepoRelative paths absPath)
        else go ds

findSharedLibs :: RepoPaths -> InstalledPackageInfo -> IO [(String, FilePath)]
findSharedLibs paths ipi = catMaybes <$> traverse oneStem (hsLibraries ipi)
  where
    dirs = libraryDynDirs ipi ++ libraryDirs ipi
    oneStem stem = do
      let soname = "lib" ++ stem ++ "-ghc" ++ rpGhcVersion paths ++ ".so"
      mpath <- findOne paths dirs soname
      return ((,) soname <$> mpath)

-- | @.conf@ files use @${pkgroot}@ (the directory containing the package
-- db) as a portable stand-in for their own absolute location - GHC's own
-- @ghc-pkg@ expands this when it serves package info, but reading the
-- file directly (as 'parseInstalledPackageInfo' does here) doesn't, so
-- library-dirs\/include-dirs come back as literal, unusable
-- @"${pkgroot}/..."@ strings unless expanded by hand. Mirrors
-- 'Distribution.Simple.Program.HcPkg.mungePackagePaths', which isn't
-- exported.
mungePkgroot :: FilePath -> InstalledPackageInfo -> InstalledPackageInfo
mungePkgroot pkgroot ipi =
  ipi
    { libraryDirs = map munge (libraryDirs ipi)
    , libraryDynDirs = map munge (libraryDynDirs ipi)
    , includeDirs = map munge (includeDirs ipi)
    }
  where
    munge p = maybe p collapseDotDot (stripPkgroot p)
    stripPkgroot p = case splitPath p of
      (root : rest) -> case stripPrefix "${pkgroot}" root of
        Just [sep] | isPathSeparator sep -> Just (pkgroot </> joinPath rest)
        _ -> Nothing
      _ -> Nothing

-- | @${pkgroot}@ substitution routinely produces a @.../package.conf.d/
-- ../lib/...@ path (since @${pkgroot}@ is the db directory, and the real
-- libraries live next to it, not under it) - harmless as a real filesystem
-- path, but buck2 rejects any @attrs.source()@ containing a literal
-- @".."@ component ("expected a normalized path"), so it has to be
-- collapsed before it ever reaches a generated rule. Unlike
-- 'System.FilePath.normalise' (which only tidies separators\/dots, not
-- @".."@ segments - not safe in general with symlinks in play, but safe
-- here since every path this is applied to is already fully resolved:
-- real, non-symlink directories under GHC's own libdir or the cabal
-- store).
collapseDotDot :: FilePath -> FilePath
collapseDotDot = joinPath . reverse . foldl' step [] . splitDirectories
  where
    step (top : rest) ".." | top /= ".." && top /= [pathSeparator] = rest
    step stack "." = stack
    step stack seg = seg : stack

-- | Point @link@ at @target@, creating or repointing it as needed.
ensureSymlink :: FilePath -> FilePath -> IO ()
ensureSymlink link target = do
  exists <- doesPathExist link
  when exists $ removeFile link
  createFileLink target link

-- | The filtered, recached db every non-global package's generated rule
-- points its @db =@ at - holding both store packages' @.conf@s and (see
-- 'generatePrebuilt's own haddock) any non-local-but-inplace package's,
-- since @ghc-pkg@'s recache doesn't care which original db a symlinked
-- @.conf@ came from, only that it's present here.
setupStoreDB :: Verbosity -> ConfiguredProgram -> FilePath -> [ResolvedPackage] -> IO ()
setupStoreDB verbosity ghcPkgProg targetStoreDB packages = do
  exists <- doesPathExist targetStoreDB
  when exists $ removeDirectoryRecursive targetStoreDB
  createDirectoryIfMissing True targetStoreDB
  traverse_
    (\p -> createFileLink (rpConfPath p) (targetStoreDB </> unUnitId (rpUnitId p) <.> "conf"))
    (filter ((/= GlobalDb) . rpDbKind) packages)
  rawSystemExit verbosity Nothing (programPath ghcPkgProg) ["--package-db", targetStoreDB, "recache"]

writeBuckFile :: FilePath -> RepoPaths -> [ResolvedPackage] -> Map String FilePath -> IO ()
writeBuckFile targetDir paths packages buildToolPaths =
  writeFile (targetDir </> "BUCK") (renderFile header [] calls)
  where
    header = "@generated by `cabal buck2` - do not edit by hand.\nRe-run `cabal buck2` to update."
    uidToTarget = Map.fromList [(rpUnitId p, targetName p) | p <- packages]
    calls =
      [prebuiltCall paths uidToTarget p | p <- packages]
        ++ [buildToolExportCall name relPath | (name, relPath) <- Map.toList (Map.mapMaybe (toRepoRelative paths) buildToolPaths)]

-- | One @export_file()@ per resolved @build-tool-depends@ executable -
-- gives it a real buck2 target ('DefaultInfo' with a single default
-- output, basename forced to the tool's own bare name, executable bit
-- forced on regardless of whatever @copy_file@'s own default happens to
-- preserve) that any component's own @build_tool_depends@ can reference
-- directly, the same @export_file()@ primitive this project already
-- uses for autogen files - see "Distribution.Client.Buck2.CabalToBuck"'s
-- own @buildToolDependsArg@ for the consuming side, and
-- @buck2\/prelude\/decls\/haskell_common.bzl@'s own comment on the attr
-- for why this needs to be a real target at all (not a host-filesystem
-- symlink, superseded by this - see buck2.md's own DONE entry).
buildToolExportCall :: String -> FilePath -> Call
buildToolExportCall name relPath =
  call
    "export_file"
    [ ("name", str (name ++ "-exe"))
    , ("src", str relPath)
    , ("out", str name)
    , ("mode", str "copy")
    , ("executable_bit_override", VBool True)
    , ("visibility", strList ["PUBLIC"])
    ]

-- | Unlike a local package (one @haskell_library()@ per library, main or
-- named sub-library alike - see 'libTargetName'), a *prebuilt* one used
-- to get exactly one @haskell_prebuilt_library()@ per package name,
-- regardless of how many of its libraries were actually in the
-- dependency closure - a real bug, not just a theoretical gap: a package
-- with an internal sub-library (e.g. @attoparsec@'s own
-- @attoparsec-internal@) resolves to *two* units here, and buck2
-- rejected the second @haskell_prebuilt_library()@ outright as a
-- duplicate target the first time this was tried against a real,
-- large project. 'sourceLibName' (parsed straight from the @.conf@,
-- the exact same 'LibraryName' a local package's own 'libName' would
-- give) is what 'libTargetName' needs to tell them apart, the same way
-- it already does for local packages.
targetName :: ResolvedPackage -> String
targetName p = libTargetName (packageName (rpInfo p)) (sourceLibName (rpInfo p))

prebuiltCall :: RepoPaths -> Map UnitId String -> ResolvedPackage -> Call
prebuiltCall paths uidToTarget p =
  call
    "haskell_prebuilt_library"
    ( [ ("name", str (targetName p))
      , ("version", str (prettyShow (packageVersion (rpInfo p))))
      , ("id", str (unUnitId (rpUnitId p)))
      , ("db", str (if rpDbKind p == GlobalDb then globalDbRel else "store-db"))
      , ("static_libs", strList (rpStaticLibs p))
      ]
        ++ [("profiled_static_libs", strList (rpProfiledLibs p)) | not (null (rpProfiledLibs p))]
        ++ [("pic_profiled_static_libs", strList (rpProfiledLibs p)) | not (null (rpProfiledLibs p))]
        ++ [("shared_libs", VDict [(soname, str sopath) | (soname, sopath) <- rpSharedLibs p])]
        ++ [("cxx_header_dirs", strList headerDirs) | not (null headerDirs)]
        ++ [("exported_linker_flags", strList extraLinkerFlags) | not (null extraLinkerFlags)]
        ++ [("deps", strList depTargets) | not (null depTargets)]
        ++ [("visibility", strList ["PUBLIC"])]
    )
  where
    info = rpInfo p
    globalDbRel = ("ghc-" ++ rpGhcVersion paths) </> "package.conf.d"
    headerDirs = mapMaybe (toRepoRelative paths) (includeDirs info)
    extraLinkerFlags = ["-l" ++ lib | lib <- extraLibraries info]
    depTargets = ordNub [":" ++ t | d <- depends info, Just t <- [Map.lookup d uidToTarget]]

-- | Convert an absolute path under GHC's libdir, the cabal store, or one
-- of the per-package inplace-build anchors, into one relative to
-- @third-party\/haskell@, via whichever symlink 'generatePrebuilt' pointed
-- there actually contains it - the same translation
-- gen-haskell-prebuilt.py's own @abs_to_rel@ did (extended here with the
-- inplace anchors it never needed).
toRepoRelative :: RepoPaths -> FilePath -> Maybe FilePath
toRepoRelative paths path
  | not (isAbsolute path) = Nothing
  | otherwise =
      relTo (rpGlobalRootAbs paths) ("ghc-" ++ rpGhcVersion paths) path
        <|> relTo (rpStoreRootAbs paths) "cabal-store" path
        <|> foldr (\(root, prefix) acc -> relTo root prefix path <|> acc) Nothing (rpExtraRoots paths)

relTo :: FilePath -> FilePath -> FilePath -> Maybe FilePath
relTo root repoRelPrefix path =
  let r = makeRelative root path
   in if r /= path then Just (repoRelPrefix </> r) else Nothing

-- | The store-installed binary path (repo-relative, like everything else
-- 'toRepoRelative' produces) for a build-tool dependency package - e.g.
-- @alex@\/@happy@, needed by buck2/alex_happy.bzl to preprocess @.x@\/@.y@
-- sources - or 'Nothing' if the project doesn't need it at all. A
-- simplified 'CmdListBin.elaboratedPackage'\/@bin_file'@: alex\/happy are
-- always external Hackage dependencies, never a local package, so the
-- inplace-build-style branch that logic also has to handle never applies
-- here, and (being plain, single-executable packages) their own
-- executable is always named after the package itself, with no need to
-- resolve a target selector to find out which component that is.
findToolBinary :: RepoPaths -> ElaboratedSharedConfig -> ElaboratedInstallPlan -> String -> Maybe FilePath
findToolBinary paths shared plan toolName =
  listToMaybe
    [ rel
    | pkg <- InstallPlan.toList plan
    , Just elab <- [configuredOrInstalled pkg]
    , prettyShow (packageName elab) == toolName
    , let absPath = InstallDirs.bindir (elabInstallDirs elab) </> toolName <.> exeExtension (pkgConfigPlatform shared)
    , Just rel <- [toRepoRelative paths absPath]
    ]

-- | The real, absolute binary path for one @pkg:exe@ pair - 'Nothing' if
-- that package isn't in the resolved dependency closure at all, or has
-- no matching executable
-- component. Unlike 'findToolBinary' (which assumes the executable is
-- named after its own package - true for alex\/happy, both self-named
-- single-executable packages), this matches the package and executable
-- names independently, since a @build-tool-depends: pkg:exe@ entry can
-- legitimately name them differently.
--
-- A package brought in purely as a @build-tool-depends@ needs commonly
-- elaborate to *two* distinct plan nodes for the same package name - a
-- whole-package one (@elabComponentName@ @Nothing@ or some other
-- component) and a real per-executable one (@elabComponentName ==
-- Just (CExeName exeName)@) - and only the latter's own
-- 'elabInstallDirs' has a real, populated @bindir@ (confirmed the hard
-- way: the former's own @bindir@ computes to a real-looking but
-- entirely empty directory, no error, just @createProcess: posix_spawnp:
-- does not exist@ at the point of actually trying to run it). Preferring
-- an exact component-name match - falling back to any match sharing the
-- package name only if none exists, the same as 'findToolBinary' always
-- did - fixes this without needing to know in advance which shape a
-- given package's own plan happens to take.
findExeBinaryAbs :: ElaboratedSharedConfig -> ElaboratedInstallPlan -> PackageName -> UnqualComponentName -> Maybe FilePath
findExeBinaryAbs shared plan pn exeName =
  listToMaybe (exact ++ fallback)
  where
    candidates =
      [ elab
      | pkg <- InstallPlan.toList plan
      , Just elab <- [configuredOrInstalled pkg]
      , packageName elab == pn
      ]
    binPathFor elab = InstallDirs.bindir (elabInstallDirs elab) </> unUnqualComponentName exeName <.> exeExtension (pkgConfigPlatform shared)
    exact = [binPathFor elab | elab <- candidates, elabComponentName elab == Just (CExeName exeName)]
    fallback = [binPathFor elab | elab <- candidates]

-- | Same package, in either of the two states a *non-local* dependency
-- that's actually going to be used can be in: 'Configured' (needs
-- building this run) or 'Installed' (already built and installed from a
-- previous run, nothing to do - which is what alex/happy settle into on
-- any @cabal buck2@ after the first, once their build is cached). Unlike
-- 'installedUnitId' (a 'HasUnitId' method, already defined uniformly
-- across all three 'GenericPlanPackage' constructors), there's no
-- existing helper for this, since most other call sites here only need
-- the unit id, not the full 'ElaboratedConfiguredPackage'.
configuredOrInstalled :: InstallPlan.GenericPlanPackage ipkg srcpkg -> Maybe srcpkg
configuredOrInstalled (InstallPlan.Configured spkg) = Just spkg
configuredOrInstalled (InstallPlan.Installed spkg) = Just spkg
configuredOrInstalled InstallPlan.PreExisting{} = Nothing

inStore :: Maybe FilePath -> Bool
inStore = maybe False ("cabal-store" `isPrefixOf`)

writeToolsFile :: FilePath -> String -> Bool -> Maybe FilePath -> Maybe FilePath -> IO ()
writeToolsFile targetDir ghcVersionStr ghcDynamic alexPath happyPath =
  writeFile (targetDir </> "tools.bzl") $
    unlines
      [ "# @generated by `cabal buck2` - do not edit by hand."
      , "# Re-run `cabal buck2` to update."
      , ""
      , "GHC_VERSION = " ++ show ghcVersionStr
      , "GHC_BIN_DIR = \"third-party/haskell/ghc-bin\""
      , "GHC_DYNAMIC = " ++ (if ghcDynamic then "True" else "False")
      , ""
      , "ALEX = " ++ show ("third-party/haskell/" ++ fromMaybe "missing" alexPath)
      , "HAPPY = " ++ show ("third-party/haskell/" ++ fromMaybe "missing" happyPath)
      ]
