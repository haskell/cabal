{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

module Distribution.Simple.GHC.Build.ExtraSources where

import Control.Monad
import Data.Foldable
import Distribution.Compiler (CompilerFlavor (GHC))
import Distribution.Simple.Compiler (compilerCompatVersion)
import Distribution.Simple.Flag
import qualified Distribution.Simple.GHC.Internal as Internal
import Distribution.Simple.Program
import Distribution.Simple.Program.GHC
import Distribution.Simple.Utils
import Distribution.Utils.NubList

import Distribution.Types.BuildInfo
import Distribution.Types.Component
import Distribution.Types.TargetInfo

import Distribution.Simple.Build.Inputs
import Distribution.Simple.BuildWay
import Distribution.Simple.GHC.Build.Utils
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup.Common (commonSetupTempFileOptions)
import Distribution.System (Arch (JavaScript), Platform (..))
import Distribution.Types.ComponentLocalBuildInfo
import Distribution.Types.ExtraSource (ExtraSource (..), extraSourceFromPath)
import Distribution.Utils.Path
import Distribution.Verbosity (VerbosityHandles, mkVerbosity, verbosityLevel)
import Distribution.Version (mkVersion)

-- | An action that builds all the extra build sources of a component, i.e. C,
-- C++, Js, Asm, C-- sources.
buildAllExtraSources
  :: Maybe (SymbolicPath Pkg File)
  -- ^ An optional non-Haskell Main file
  -> ConfiguredProgram
  -- ^ The GHC configured program
  -> SymbolicPath Pkg (Dir Artifacts)
  -- ^ The build directory for this target
  -> (Bool -> [BuildWay], Bool -> BuildWay, BuildWay)
  -- ^ Needed build ways
  -> VerbosityHandles
  -- ^ Logging handles
  -> PreBuildComponentInputs
  -- ^ The context and component being built in it.
  -> IO (NubListR (SymbolicPath Pkg File))
  -- ^ Returns the (nubbed) list of extra sources that were built
buildAllExtraSources =
  mconcat
    [ buildCSources
    , buildCxxSources
    , buildJsSources
    , buildAsmSources
    , buildCmmSources
    ]

type ExtraSourceBuilder =
  Maybe (SymbolicPath Pkg File)
  -- ^ An optional non-Haskell Main file
  -> ConfiguredProgram
  -- ^ The GHC configured program
  -> SymbolicPath Pkg (Dir Artifacts)
  -- ^ The build directory for this target
  -> (Bool -> [BuildWay], Bool -> BuildWay, BuildWay)
  -- ^ Needed build ways
  -> VerbosityHandles
  -- ^ Logging handles
  -> PreBuildComponentInputs
  -- ^ The context and component being built in it.
  -> IO (NubListR (SymbolicPath Pkg File))
  -- ^ Returns the list of extra sources that were built

buildCSources :: ExtraSourceBuilder
buildCSources mbMainFile =
  buildExtraSources
    "C Sources"
    Internal.CSourceKind
    ( \c -> do
        let cFiles = cSources (componentBuildInfo c)
        case c of
          CExe{}
            | Just main <- mbMainFile
            , isC $ getSymbolicPath main ->
                cFiles ++ [extraSourceFromPath main]
          _otherwise -> cFiles
    )

buildCxxSources :: ExtraSourceBuilder
buildCxxSources mbMainFile =
  buildExtraSources
    "C++ Sources"
    Internal.CxxSourceKind
    ( \c -> do
        let cxxFiles = cxxSources (componentBuildInfo c)
        case c of
          CExe{}
            | Just main <- mbMainFile
            , isCxx $ getSymbolicPath main ->
                cxxFiles ++ [extraSourceFromPath main]
          _otherwise -> cxxFiles
    )

buildJsSources :: ExtraSourceBuilder
buildJsSources _mbMainFile ghcProg buildTargetDir neededWays verbHandles = do
  Platform hostArch _ <- hostPlatform <$> localBuildInfo
  let hasJsSupport = hostArch == JavaScript
  buildExtraSources
    "JS Sources"
    Internal.JsSourceKind
    ( \c ->
        if hasJsSupport
          then -- JS files are C-like with GHC's JS backend: they are
          -- "compiled" into `.o` files (renamed with a header).
          -- This is a difference from GHCJS, for which we only
          -- pass the JS files at link time.
            jsSources (componentBuildInfo c)
          else mempty
    )
    ghcProg
    buildTargetDir
    neededWays
    verbHandles

buildAsmSources :: ExtraSourceBuilder
buildAsmSources _mbMainFile =
  buildExtraSources
    "Assembler Sources"
    Internal.AsmSourceKind
    (asmSources . componentBuildInfo)

buildCmmSources :: ExtraSourceBuilder
buildCmmSources _mbMainFile =
  buildExtraSources
    "C-- Sources"
    Internal.CmmSourceKind
    (cmmSources . componentBuildInfo)

-- | Create 'PreBuildComponentRules' for a given type of extra build sources
-- which are compiled via a GHC invocation with the given options. Used to
-- define built-in extra sources, such as, C, Cxx, Js, Asm, and Cmm sources.
buildExtraSources
  :: String
  -- ^ String describing the extra sources being built, for printing.
  -> Internal.ExtraSourceKind
  -- ^ The kind of these extra sources, which determines the @'GhcOptions'@ for
  -- the invocation of GHC when compiling them, and where their per-file
  -- options go.
  -> (Component -> [ExtraSource])
  -- ^ View the extra sources of a component, typically from
  -- the build info (e.g. @'asmSources'@, @'cSources'@).
  -- @'Executable'@ components might additionally add the
  -- program entry point (@main-is@ file) to the extra sources,
  -- if it should be compiled as the rest of them.
  -> ConfiguredProgram
  -- ^ The GHC configured program
  -> SymbolicPath Pkg (Dir Artifacts)
  -- ^ The build directory for this target
  -> (Bool -> [BuildWay], Bool -> BuildWay, BuildWay)
  -- ^ Needed build ways
  -> VerbosityHandles
  -- ^ Handles for logging
  -> PreBuildComponentInputs
  -- ^ The context and component being built in it.
  -> IO (NubListR (SymbolicPath Pkg File))
  -- ^ Returns the list of extra sources that were built
buildExtraSources
  description
  kind
  viewSources
  ghcProg
  buildTargetDir
  (neededLibWays, neededFLibWay, neededExeWay)
  verbHandles
  PreBuildComponentInputs{buildingWhat, localBuildInfo = lbi, targetInfo} = do
    let
      bi = componentBuildInfo (targetComponent targetInfo)
      verbosity = mkVerbosity verbHandles $ buildingWhatVerbosity buildingWhat
      clbi = targetCLBI targetInfo
      isIndef = componentIsIndefinite clbi
      mbWorkDir = mbWorkDirLBI lbi
      i = interpretSymbolicPath mbWorkDir
      sources = viewSources (targetComponent targetInfo)
      comp = compiler lbi
      platform = hostPlatform lbi
      tempFileOptions = commonSetupTempFileOptions $ buildingWhatCommonFlags buildingWhat
      runGhcProg =
        runGHCWithResponseFile
          "ghc.rsp"
          Nothing
          tempFileOptions
          verbosity
          ghcProg
          comp
          platform
          mbWorkDir

      buildAction :: ExtraSource -> IO ()
      buildAction extraSource = do
        let baseSrcOpts =
              Internal.extraSourceGhcOptions
                kind
                (verbosityLevel verbosity)
                lbi
                bi
                clbi
                buildTargetDir
                extraSource
            vanillaSrcOpts =
              -- -fPIC is used in case you are using the repl
              -- of a dynamically linked GHC
              baseSrcOpts{ghcOptFPic = toFlag True}
            profSrcOpts =
              vanillaSrcOpts
                <> mempty
                  { ghcOptProfilingMode = toFlag True
                  }
            sharedSrcOpts =
              vanillaSrcOpts
                <> mempty
                  { ghcOptFPic = toFlag True
                  , ghcOptDynLinkMode = toFlag GhcDynamicOnly
                  }
            profSharedSrcOpts =
              vanillaSrcOpts
                <> mempty
                  { ghcOptProfilingMode = toFlag True
                  , ghcOptFPic = toFlag True
                  , ghcOptDynLinkMode = toFlag GhcDynamicOnly
                  }
            -- TODO: Placing all Haskell, C, & C++ objects in a single directory
            --       Has the potential for file collisions. In general we would
            --       consider this a user error. However, we should strive to
            --       add a warning if this occurs.
            odir = fromFlag (ghcOptObjDir vanillaSrcOpts)

            compileIfNeeded :: GhcOptions -> IO ()
            compileIfNeeded opts' = do
              needsRecomp <- checkNeedsRecompilation mbWorkDir (extraSourceFile extraSource) opts'
              when needsRecomp $ runGhcProg opts'

            -- Per-file options on JavaScript sources are routed to -optJSP,
            -- which only exists since GHC 9.12 and is dropped by
            -- 'renderGhcOptions' before that. Say so rather than silently
            -- building without them.
            optionsAreDropped =
              not (null (extraSourceOpts extraSource))
                && kind == Internal.JsSourceKind
                && maybe True (< mkVersion [9, 12]) (compilerCompatVersion GHC comp)

        when optionsAreDropped $
          warn verbosity $
            "Ignoring the per-file options on "
              ++ getSymbolicPath (extraSourceFile extraSource)
              ++ ": passing options to the JavaScript preprocessor requires GHC 9.12 or later."

        createDirectoryIfMissingVerbose verbosity True (i odir)
        case targetComponent targetInfo of
          -- For libraries, we compile extra objects in the four ways: vanilla, shared, profiled and profiled shared.
          -- We suffix shared objects with `.dyn_o`, profiled ones with `.p_o` and profiled shared ones with `.p_dyn_o`.
          CLib _lib
            -- Unless for repl, in which case we only need the vanilla way
            | BuildRepl _ <- buildingWhat ->
                compileIfNeeded vanillaSrcOpts
            | otherwise ->
                do
                  forM_ (neededLibWays isIndef) $ \case
                    StaticWay -> compileIfNeeded vanillaSrcOpts
                    DynWay -> compileIfNeeded sharedSrcOpts{ghcOptObjSuffix = toFlag "dyn_o"}
                    ProfWay -> compileIfNeeded profSrcOpts{ghcOptObjSuffix = toFlag "p_o"}
                    ProfDynWay -> compileIfNeeded profSharedSrcOpts{ghcOptObjSuffix = toFlag "p_dyn_o"}
          CFLib flib ->
            case neededFLibWay (withDynFLib flib) of
              StaticWay -> compileIfNeeded vanillaSrcOpts
              DynWay -> compileIfNeeded sharedSrcOpts
              ProfWay -> compileIfNeeded profSrcOpts
              ProfDynWay -> compileIfNeeded profSharedSrcOpts
          -- For the remaining component types (Exec, Test, Bench), we also
          -- determine with which options to build the objects (vanilla vs shared vs
          -- profiled), but predicate is the same for the three kinds.
          _exeLike ->
            case neededExeWay of
              StaticWay -> compileIfNeeded vanillaSrcOpts
              DynWay -> compileIfNeeded sharedSrcOpts
              ProfWay -> compileIfNeeded profSrcOpts
              ProfDynWay -> compileIfNeeded profSharedSrcOpts

    -- build any sources
    if null sources || componentIsIndefinite clbi
      then return mempty
      else do
        info verbosity ("Building " ++ description ++ "...")
        traverse_ buildAction sources
        return (toNubListR (map extraSourceFile sources))
