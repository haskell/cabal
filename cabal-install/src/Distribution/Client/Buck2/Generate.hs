-- | Writes the per-package @BUCK.cabal.bzl@\/@BUCK@\/@cabal-buck2\/autogen\/
-- BUCK@ files for every local project package.
--
-- Three files, not one, per package directory:
--
--   * @BUCK.cabal.bzl@ is fully regenerated on every run (it's marked
--     @\@generated@ and never hand-edited) and defines a single
--     @generated_targets()@ macro with one rule call per buildable
--     component. Deliberately kept at the package root (*not* under
--     @cabal-buck2\/@ alongside the generated @Paths_\<pkg\>@ module and
--     each component's own @cabal_macros.h@ - see
--     "Distribution.Client.Buck2.CabalToBuck") - buck2's @load()@ only
--     accepts a bare same-package filename for a same-package path (no
--     @\/@ allowed - see 'renderBuckWrapper's own haddock), and a fully
--     cell-qualified path instead would make every hand-maintained
--     @BUCK@ depend on its own package's location in the project, which
--     defeats the point of it being freely hand-editable\/relocatable.
--   * @BUCK@ is created only if it doesn't already exist, as a two-line
--     file that loads and calls that macro. This is the file a user is
--     free to hand-edit - to add extra targets, or stop calling
--     @generated_targets()@ altogether for a package that needs fully
--     custom rules - without a re-run of @cabal buck2@ ever touching it.
--   * @cabal-buck2\/autogen\/BUCK@ (see 'generateAutogenBuck') is fully
--     regenerated on every run too, and gives every autogen file its own
--     real, addressable buck2 target (via @export_file()@) - both so a
--     generated rule's own @cabal_component@ kwarg picks up
--     @cabal_macros.h@ as a real, buck2-tracked dependency edge instead
--     of an untracked path string folded into @compiler_flags@, and so
--     that other, hand-written @BUCK@ files anywhere in the project (not
--     just this package's own) can reference e.g. @Paths_\<pkg\>@
--     directly, instead of hand-rolling a stand-in for it.
module Distribution.Client.Buck2.Generate
  ( generateAllPackages
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (makeRelative, takeFileName, (</>))

import qualified Data.Map as Map

import qualified Distribution.ModuleName as ModuleName
import Distribution.Package (packageName)
import Distribution.PackageDescription
  ( Library (exposedModules, reexportedModules)
  , PackageDescription
  , library
  )
import Distribution.Types.ComponentName (ComponentName)
import Distribution.Types.LocalBuildInfo (LocalBuildInfo)
import Distribution.Types.ModuleReexport
  ( ModuleReexport (moduleReexportOriginalName, moduleReexportOriginalPackage)
  )
import Distribution.Simple.InstallDirs (PathTemplate)
import Distribution.Types.PackageName (PackageName)

import Distribution.Simple.Utils (notice, ordNub, warn)

import Distribution.Client.Buck2.CabalToBuck
import Distribution.Client.Buck2.Starlark

-- | Generate\/refresh @BUCK.cabal.bzl@ (and @BUCK@, where missing) for
-- every local package. @projectRoot@ is the buck2 cell root (the
-- directory containing @.buckconfig@), used to turn each package's
-- absolute directory into the cell-relative one buck2 target labels need.
-- @componentLBIs@ is a real, Cabal-computed 'LocalBuildInfo' for every
-- local (or quasi-local) *component* - see "Distribution.Client.CmdBuck2"
-- - used to generate each component's own @cabal_macros.h@\/
-- @Paths_\<pkg\>@\/@PackageInfo_\<pkg\>@ via Cabal's own real generators
-- (see "Distribution.Client.Buck2.CabalToBuck") instead of reimplementing
-- pieces of them by hand. @externalBuildTools@ is every
-- @build-tool-depends:@ executable name "Distribution.Client.Buck2.
-- Prebuilt" resolved a real external binary (and generated an
-- @export_file()@ target) for - see 'CabalToBuck.buildToolDependsArg'.
generateAllPackages :: Verbosity -> FilePath -> Map (PackageName, ComponentName) LocalBuildInfo -> Set String -> Map (PackageName, ComponentName) [PathTemplate] -> [(FilePath, PackageDescription)] -> IO ()
generateAllPackages verbosity projectRoot componentLBIs externalBuildTools projectTestOptions pkgs = do
  traverse_ (generateOnePackage verbosity localIndex projectRoot componentLBIs externalBuildTools projectTestOptions) pkgs
  where
    localIndex :: LocalPackageIndex
    localIndex =
      Map.fromList
        [ (packageName pkgDesc, (rootRelativeDir projectRoot pkgDir, reexportOrigins pkgDesc))
        | (pkgDir, pkgDesc) <- pkgs
        ]
    -- Every module exposed by any local package's main library, to
    -- resolve a `reexported-modules:` entry that (as is typical - see
    -- Cabal.cabal's own reexport of Cabal-syntax) names only the bare
    -- module, not an explicit `origin-package:Module` - Cabal itself
    -- resolves that form by searching the reexporting package's own
    -- build-depends for whichever one actually defines it, which for a
    -- *local* origin this index can do too (an external origin doesn't
    -- need this: its real .conf file already declares the reexport
    -- directly to ghc-pkg).
    moduleOwners :: Map.Map ModuleName.ModuleName PackageName
    moduleOwners =
      Map.fromList
        [ (m, packageName pkgDesc)
        | (_, pkgDesc) <- pkgs
        , Just lib <- [library pkgDesc]
        , m <- exposedModules lib
        ]
    reexportOrigins pkgDesc =
      ordNub
        [ pn
        | Just lib <- [library pkgDesc]
        , reexport <- reexportedModules lib
        , Just pn <- [originPackage reexport]
        , pn /= packageName pkgDesc
        ]
    originPackage reexport = case moduleReexportOriginalPackage reexport of
      Just pn -> Just pn
      Nothing -> Map.lookup (moduleReexportOriginalName reexport) moduleOwners

rootRelativeDir :: FilePath -> FilePath -> FilePath
rootRelativeDir projectRoot pkgDir = case makeRelative projectRoot pkgDir of
  "" -> "."
  rel -> rel

generateOnePackage :: Verbosity -> LocalPackageIndex -> FilePath -> Map (PackageName, ComponentName) LocalBuildInfo -> Set String -> Map (PackageName, ComponentName) [PathTemplate] -> (FilePath, PackageDescription) -> IO ()
generateOnePackage verbosity localIndex projectRoot componentLBIs externalBuildTools projectTestOptions (pkgDir, pkgDesc) = do
  targets <- generatePackageTargets verbosity localIndex (rootRelativeDir projectRoot pkgDir) componentLBIs externalBuildTools projectTestOptions pkgDir pkgDesc
  let pkgName = packageName pkgDesc
  if null (ptCalls targets)
    then warn verbosity $ "cabal buck2: no buck2 targets generated for package " ++ show pkgName
    else do
      let bzlPath = pkgDir </> "BUCK.cabal.bzl"
          buckPath = pkgDir </> "BUCK"
      writeFile bzlPath (renderGeneratedBzl pkgName targets)
      buckExists <- doesFileExist buckPath
      unless buckExists $ writeFile buckPath renderBuckWrapper
      generateAutogenBuck pkgDir pkgName targets
      notice verbosity $
        "cabal buck2: generated "
          ++ (rootRelativeDir projectRoot pkgDir </> "BUCK.cabal.bzl")
          ++ " ("
          ++ show (length (ptCalls targets))
          ++ " target(s))"
          ++ (if buckExists then "" else ", created " ++ (rootRelativeDir projectRoot pkgDir </> "BUCK"))

-- | @cabal-buck2\/autogen\/BUCK@: one @export_file()@ per generated
-- autogen file (a component's own @cabal_macros.h@, or the package's
-- @Paths_\<pkg\>@ module - see 'PackageTargets'' own haddock), so it's a
-- real, addressable buck2 target - referenced by this same package's own
-- generated rules via their @cabal_component@ kwarg (see
-- "Distribution.Client.Buck2.CabalToBuck"'s @cabalComponentArg@), and
-- just as easily by a hand-written rule anywhere else in the project
-- (the point of this file existing at all - see buck2.md's DONE entry on
-- this). Fully regenerated on every run, like @BUCK.cabal.bzl@ itself -
-- never hand-edited, so no separate wrapper file is needed here the way
-- @BUCK@ is for it. @export_file@ is a builtin buck2\/prelude rule, so
-- this needs no @load()@ statement at all.
generateAutogenBuck :: FilePath -> PackageName -> PackageTargets -> IO ()
generateAutogenBuck pkgDir pkgName targets
  | null exports = return ()
  | otherwise = do
      createDirectoryIfMissing True autogenDir
      writeFile (autogenDir </> "BUCK") (renderFile header [] exportCalls)
  where
    exports = ptAutogenExports targets
    autogenDir = pkgDir </> "cabal-buck2" </> "autogen"
    header =
      "@generated by `cabal buck2` from "
        ++ prettyShow pkgName
        ++ ".cabal - do not edit by hand.\nRe-run `cabal buck2` after editing the .cabal file to refresh this file."
    exportCalls =
      [ call
        "export_file"
        [ ("name", str exportName)
        , ("src", str exportRelPath)
        , -- export_file()'s own `out` defaults to the *rule's* name, not
          -- to `src`'s basename (see buck2/prelude/export_file.bzl) - so
          -- without this, the materialised artifact would be named e.g.
          -- `exe-pkg-detailed-test-stub-main` instead of `Main.hs`,
          -- losing the source file's real extension. Harmless for a
          -- consumer that only ever references the file opaquely (e.g.
          -- cabal_component's own `$(location ...)` include-path use),
          -- but a real, silent bug for one referenced from `srcs`:
          -- buck2/haskell.bzl's own `is_haskell_src()` check (and GHC's
          -- own module-name-from-extension logic) both key off the
          -- artifact's own filename, not the target label - an artifact
          -- missing its `.hs` extension is silently treated as a
          -- non-Haskell "hidden" input instead of a compiled module,
          -- with no error from buck2 itself, just GHC's own unhelpful
          -- "Could not find module" (found the hard way against a real
          -- `buck2 build` - see buck2.md's own entry on this).
          ("out", str (takeFileName exportRelPath))
        , ("visibility", strList ["PUBLIC"])
        ]
      | (exportName, exportRelPath) <- exports
      ]

renderGeneratedBzl :: PackageName -> PackageTargets -> String
renderGeneratedBzl pkgName targets =
  unlines
    [ "# @generated by `cabal buck2` from " ++ prettyShow pkgName ++ ".cabal - do not edit by hand."
    , "# Re-run `cabal buck2` after editing the .cabal file to refresh this file."
    ]
    ++ "\n"
    ++ concatMap (uncurry renderLoad) (ptLoads targets)
    ++ "\n"
    ++ concatMap (\(name, v) -> renderBinding name v ++ "\n") (ptConstants targets)
    ++ "def generated_targets():\n"
    ++ indentBlock (intercalate "\n" (map renderCall (ptCalls targets)))

indentBlock :: String -> String
indentBlock = unlines . map indentLine . lines
  where
    indentLine "" = ""
    indentLine l = "    " ++ l

-- | buck2's @load()@ doesn't accept a same-package path containing a
-- @\/@ (@:cabal-buck2\/targets.bzl@ fails outright: "Unable to parse
-- import spec ... but got a path") - only a bare same-package filename
-- (@:filename.bzl@) or a fully cell-qualified one
-- (@\/\/package\/path:filename.bzl@) work, and the latter would make
-- this hand-maintained file depend on its own package's location in the
-- project (confirmed empirically against the real buck2 binary while
-- trying @BUCK.cabal.bzl@ living under @cabal-buck2\/@ instead - reverted
-- for exactly this reason). Keeping @BUCK.cabal.bzl@ at the package root
-- avoids the whole issue: it's a same-package, no-slash filename either
-- way.
renderBuckWrapper :: String
renderBuckWrapper =
  unlines
    [ "# Hand-maintained: add extra targets below, or stop calling"
    , "# generated_targets() to fully take over this package's BUCK rules."
    , "load(\":BUCK.cabal.bzl\", \"generated_targets\")"
    , ""
    , "generated_targets()"
    ]
