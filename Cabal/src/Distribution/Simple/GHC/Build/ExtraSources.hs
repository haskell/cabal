{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module Distribution.Simple.GHC.Build.ExtraSources where

import Control.Monad
import Data.Foldable
import Distribution.Simple.Flag
import qualified Distribution.Simple.GHC.Internal as Internal
import Distribution.Simple.Program.GHC
import Distribution.Simple.Utils
import Distribution.Utils.NubList

import Distribution.Types.BuildInfo
import Distribution.Types.Component
import Distribution.Types.TargetInfo

import Distribution.Simple.Build.Inputs
import Distribution.Simple.GHC.Build.Modules
import Distribution.Simple.GHC.Build.Utils
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program.Types
import Distribution.Simple.Setup.Common (commonSetupTempFileOptions)
import Distribution.System (Arch (JavaScript), Platform (..))
import Distribution.Types.ComponentLocalBuildInfo
import Distribution.Utils.Path
import Distribution.Verbosity (VerbosityHandles, VerbosityLevel, mkVerbosity, verbosityLevel)

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

buildCSources
  , buildCxxSources
  , buildJsSources
  , buildAsmSources
  , buildCmmSources
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
    -- ^ Returns the list of extra sources that were built
buildCSources mbMainFile =
  buildExtraSources
    "C Sources"
    Internal.componentCcGhcOptions
    ( \c -> do
        let cFiles = cSources (componentBuildInfo c)
        case c of
          CExe{}
            | Just main <- mbMainFile
            , isC $ getSymbolicPath main ->
                cFiles ++ [main]
          _otherwise -> cFiles
    )
buildCxxSources mbMainFile =
  buildExtraSources
    "C++ Sources"
    Internal.componentCxxGhcOptions
    ( \c -> do
        let cxxFiles = cxxSources (componentBuildInfo c)
        case c of
          CExe{}
            | Just main <- mbMainFile
            , isCxx $ getSymbolicPath main ->
                cxxFiles ++ [main]
          _otherwise -> cxxFiles
    )
buildJsSources _mbMainFile ghcProg buildTargetDir neededWays verbHandles = do
  Platform hostArch _ <- hostPlatform <$> localBuildInfo
  let hasJsSupport = hostArch == JavaScript
  buildExtraSources
    "JS Sources"
    Internal.componentJsGhcOptions
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
buildAsmSources _mbMainFile =
  buildExtraSources
    "Assembler Sources"
    Internal.componentAsmGhcOptions
    (asmSources . componentBuildInfo)
buildCmmSources _mbMainFile =
  buildExtraSources
    "C-- Sources"
    Internal.componentCmmGhcOptions
    (cmmSources . componentBuildInfo)

-- | Create 'PreBuildComponentRules' for a given type of extra build sources
-- which are compiled via a GHC invocation with the given options. Used to
-- define built-in extra sources, such as, C, Cxx, Js, Asm, and Cmm sources.
buildExtraSources
  :: String
  -- ^ String describing the extra sources being built, for printing.
  -> ( VerbosityLevel
       -> LocalBuildInfo
       -> BuildInfo
       -> ComponentLocalBuildInfo
       -> SymbolicPath Pkg (Dir Artifacts)
       -> SymbolicPath Pkg File
       -> GhcOptions
     )
  -- ^ Function to determine the @'GhcOptions'@ for the
  -- invocation of GHC when compiling these extra sources (e.g.
  -- @'Internal.componentCxxGhcOptions'@,
  -- @'Internal.componentCmmGhcOptions'@)
  -> (Component -> [SymbolicPath Pkg File])
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
  componentSourceGhcOptions
  viewSources
  ghcProg
  buildTargetDir
  (neededLibWays, neededFLibWay, neededExeWay)
  verbHandles =
    \PreBuildComponentInputs{buildingWhat, localBuildInfo = lbi, targetInfo} -> do
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

        buildAction :: SymbolicPath Pkg File -> IO ()
        buildAction sourceFile = do
          let baseSrcOpts =
                componentSourceGhcOptions
                  (verbosityLevel verbosity)
                  lbi
                  bi
                  clbi
                  buildTargetDir
                  sourceFile
              vanillaSrcOpts =
                -- -fPIC is used in case you are using the repl
                -- of a dynamically linked GHC
                baseSrcOpts{ghcOptFPic = toFlag True}
              profSrcOpts =
                vanillaSrcOpts
                  `mappend` mempty
                    { ghcOptProfilingMode = toFlag True
                    }
              sharedSrcOpts =
                vanillaSrcOpts
                  `mappend` mempty
                    { ghcOptFPic = toFlag True
                    , ghcOptDynLinkMode = toFlag GhcDynamicOnly
                    }
              profSharedSrcOpts =
                vanillaSrcOpts
                  `mappend` mempty
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
              compileIfNeeded opts = do
                needsRecomp <- checkNeedsRecompilation mbWorkDir sourceFile opts
                when needsRecomp $ runGhcProg opts

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
      if (null sources || componentIsIndefinite clbi)
        then return mempty
        else do
          info verbosity ("Building " ++ description ++ "...")
          traverse_ buildAction sources
          return (toNubListR sources)
