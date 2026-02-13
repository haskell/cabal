{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module Distribution.Simple.GHC.Build.ExtraSources where

import Control.Monad (forM, when)
import Data.Foldable
import Distribution.Simple.Flag
import qualified Distribution.Simple.GHC.Internal as Internal
import Distribution.Simple.Program.GHC
import Distribution.Simple.Utils
import Distribution.Utils.NubList

import Distribution.Types.BuildInfo
import Distribution.Types.Component
import Distribution.Types.ParStrat
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
import Distribution.Verbosity (Verbosity)

-- | An action that builds all the extra build sources of a component, i.e. C,
-- C++, Js, Asm, C-- sources.
buildAllExtraSources
  :: Flag ParStrat
  -> Maybe (SymbolicPath Pkg File)
  -- ^ An optional non-Haskell Main file
  -> ConfiguredProgram
  -- ^ The GHC configured program
  -> SymbolicPath Pkg (Dir Artifacts)
  -- ^ The build directory for this target
  -> (Bool -> [BuildWay], Bool -> BuildWay, BuildWay)
  -- ^ Needed build ways
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
    :: Flag ParStrat
    -> Maybe (SymbolicPath Pkg File)
    -- ^ An optional non-Haskell Main file
    -> ConfiguredProgram
    -- ^ The GHC configured program
    -> SymbolicPath Pkg (Dir Artifacts)
    -- ^ The build directory for this target
    -> (Bool -> [BuildWay], Bool -> BuildWay, BuildWay)
    -- ^ Needed build ways
    -> PreBuildComponentInputs
    -- ^ The context and component being built in it.
    -> IO (NubListR (SymbolicPath Pkg File))
    -- ^ Returns the list of extra sources that were built
buildCSources parStrat mbMainFile =
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
    parStrat
buildCxxSources parStrat mbMainFile =
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
    parStrat
buildJsSources parStrat _mbMainFile ghcProg buildTargetDir neededWays = do
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
    parStrat
    ghcProg
    buildTargetDir
    neededWays
buildAsmSources parStrat _mbMainFile =
  buildExtraSources
    "Assembler Sources"
    Internal.componentAsmGhcOptions
    (asmSources . componentBuildInfo)
    parStrat
buildCmmSources parStrat _mbMainFile =
  buildExtraSources
    "C-- Sources"
    Internal.componentCmmGhcOptions
    (cmmSources . componentBuildInfo)
    parStrat

-- | Create 'PreBuildComponentRules' for a given type of extra build sources
-- which are compiled via a GHC invocation with the given options. Used to
-- define built-in extra sources, such as, C, Cxx, Js, Asm, and Cmm sources.
buildExtraSources
  :: String
  -- ^ String describing the extra sources being built, for printing.
  -> ( Verbosity
       -> LocalBuildInfo
       -> BuildInfo
       -> ComponentLocalBuildInfo
       -> SymbolicPath Pkg (Dir Artifacts)
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
  -> Flag ParStrat
  -> ConfiguredProgram
  -- ^ The GHC configured program
  -> SymbolicPath Pkg (Dir Artifacts)
  -- ^ The build directory for this target
  -> (Bool -> [BuildWay], Bool -> BuildWay, BuildWay)
  -- ^ Needed build ways
  -> PreBuildComponentInputs
  -- ^ The context and component being built in it.
  -> IO (NubListR (SymbolicPath Pkg File))
  -- ^ Returns the list of extra sources that were built
buildExtraSources
  description
  componentSourceGhcOptions
  viewSources
  parStrat
  ghcProg
  buildTargetDir
  (neededLibWays, neededFLibWay, neededExeWay) =
    \PreBuildComponentInputs{buildingWhat, localBuildInfo = lbi, targetInfo} -> do
      let
        bi = componentBuildInfo (targetComponent targetInfo)
        verbosity = buildingWhatVerbosity buildingWhat
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

        buildAction :: [SymbolicPath Pkg File] -> IO ()
        buildAction sourceFiles = do
          let baseSrcOpts =
                componentSourceGhcOptions
                  verbosity
                  lbi
                  bi
                  clbi
                  buildTargetDir
              vanillaSrcOpts =
                baseSrcOpts
                  { ghcOptFPic = toFlag True
                  , -- -fPIC is always used in case you are using the repl of a
                    -- dynamically linked GHC
                    ghcOptNumJobs = parStrat
                  }
              profSrcOpts =
                vanillaSrcOpts
                  { ghcOptProfilingMode = toFlag True
                  , ghcOptObjSuffix = toFlag "p_o"
                  }
              sharedSrcOpts =
                vanillaSrcOpts
                  { ghcOptDynLinkMode = toFlag GhcDynamicOnly
                  , ghcOptObjSuffix = toFlag "dyn_o"
                  }
              profSharedSrcOpts =
                vanillaSrcOpts
                  { ghcOptProfilingMode = toFlag True
                  , ghcOptDynLinkMode = toFlag GhcDynamicOnly
                  , ghcOptObjSuffix = toFlag "p_dyn_o"
                  }
              -- TODO: Placing all Haskell, C, & C++ objects in a single directory
              --       Has the potential for file collisions. In general we would
              --       consider this a user error. However, we should strive to
              --       add a warning if this occurs.
              odir = fromFlag (ghcOptObjDir vanillaSrcOpts)

          createDirectoryIfMissingVerbose verbosity True (i odir)
          let optsForWay = \case
                StaticWay -> vanillaSrcOpts
                DynWay -> sharedSrcOpts
                ProfWay -> profSrcOpts
                ProfDynWay -> profSharedSrcOpts
          -- we don't tell GHC to suffix the filenames of objects produced from
          -- extra-sources for executables or foreign libraries
          let stripObjectSuffix opts =
                opts{ghcOptObjSuffix = ghcOptObjSuffix baseSrcOpts}

          let optsForNeededWays =
                case targetComponent targetInfo of
                  -- For libraries (foreign or not), we compile extra objects in the four ways: vanilla, shared,
                  -- profiled and profiled shared. We suffix shared objects with `.dyn_o`, profiled ones with `.p_o`
                  -- and profiled shared ones with `.p_dyn_o`.
                  CLib _lib
                    -- Unless for repl, in which case we only need the vanilla way
                    | BuildRepl _ <- buildingWhat ->
                        [vanillaSrcOpts]
                    | otherwise ->
                        optsForWay <$> neededLibWays isIndef
                  CFLib flib ->
                    [stripObjectSuffix (optsForWay (neededFLibWay (withDynFLib flib)))]
                  -- For the remaining component types (Exec, Test, Bench) we
                  -- only need to build them with one set of options.
                  _exeLike ->
                    [stripObjectSuffix (optsForWay neededExeWay)]
          forM_ optsForNeededWays $ \opts -> do
            files <- fmap concat $ forM sourceFiles $ \sourceFile -> do
              needsRecomp <- checkNeedsRecompilation mbWorkDir sourceFile opts
              return [sourceFile | needsRecomp]
            when (not (null files)) $
              runGhcProg opts{ghcOptInputFiles = toNubListR files}
      -- build any sources
      if (null sources || componentIsIndefinite clbi)
        then return mempty
        else do
          info verbosity ("Building " ++ description ++ "...")
          buildAction sources
          return (toNubListR sources)
