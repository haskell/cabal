{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Distribution.Simple.GHC.Build.ExtraSources where

import Control.Monad
import Data.Foldable
import Distribution.Simple.Flag
import qualified Distribution.Simple.GHC.Internal as Internal
import Distribution.Simple.Program.GHC
import Distribution.Simple.Utils

import Distribution.Types.BuildInfo
import Distribution.Types.Component
import Distribution.Types.TargetInfo

import Distribution.Simple.GHC.Build.Utils
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program.Types
import Distribution.System (Arch (JavaScript), Platform (..))
import Distribution.Types.ComponentLocalBuildInfo
import Distribution.Types.Executable
import Distribution.Verbosity (Verbosity)

import Distribution.Simple.Build.Monad

-- | An action that builds all the extra build sources of a component, i.e. C,
-- C++, Js, Asm, C-- sources.
buildAllExtraSources
  :: ConfiguredProgram
  -- ^ The GHC configured program
  -> FilePath
  -- ^ The build directory for this target
  -> BuildM [FilePath]
  -- ^ Returns the list of extra sources that were built
buildAllExtraSources ghcProg buildTargetDir =
  concat
    <$> traverse
      (($ buildTargetDir) . ($ ghcProg))
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
    :: ConfiguredProgram
    -- ^ The GHC configured program
    -> FilePath
    -- ^ The build directory for this target
    -> BuildM [FilePath]
    -- ^ Returns the list of extra sources that were built
buildCSources =
  buildExtraSources
    "C Sources"
    Internal.componentCcGhcOptions
    True
    ( \c ->
        cSources (componentBuildInfo c)
          ++ case c of
            CExe exe | isC (modulePath exe) -> [modulePath exe]
            _otherwise -> []
    )
buildCxxSources =
  buildExtraSources
    "C++ Sources"
    Internal.componentCxxGhcOptions
    True
    ( \c ->
        cxxSources (componentBuildInfo c)
          ++ case c of
            CExe exe | isCxx (modulePath exe) -> [modulePath exe]
            _otherwise -> []
    )
buildJsSources ghcProg buildTargetDir = do
  Platform hostArch _ <- hostPlatform <$> buildLBI
  let hasJsSupport = hostArch == JavaScript
  buildExtraSources
    "JS Sources"
    Internal.componentJsGhcOptions
    False
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
buildAsmSources =
  buildExtraSources
    "Assembler Sources"
    Internal.componentAsmGhcOptions
    True
    (asmSources . componentBuildInfo)
buildCmmSources =
  buildExtraSources
    "C-- Sources"
    Internal.componentCmmGhcOptions
    True
    (cmmSources . componentBuildInfo)

-- | Create 'PreBuildComponentRules' for a given type of extra build sources
-- which are compiled via a GHC invocation with the given options. Used to
-- define built-in extra sources, such as, C, Cxx, Js, Asm, and Cmm sources.
buildExtraSources
  :: String
  -- ^ String describing the extra sources being built, for printing.
  -> (Verbosity -> LocalBuildInfo -> BuildInfo -> ComponentLocalBuildInfo -> FilePath -> FilePath -> GhcOptions)
  -- ^ Function to determine the @'GhcOptions'@ for the
  -- invocation of GHC when compiling these extra sources (e.g.
  -- @'Internal.componentCxxGhcOptions'@,
  -- @'Internal.componentCmmGhcOptions'@)
  -> Bool
  -- ^ Want dynamic?
  -> (Component -> [FilePath])
  -- ^ View the extra sources of a component, typically from
  -- the build info (e.g. @'asmSources'@, @'cSources'@).
  -- @'Executable'@ components might additionally add the
  -- program entry point (@main-is@ file) to the extra sources,
  -- if it should be compiled as the rest of them.
  -> ConfiguredProgram
  -- ^ The GHC configured program
  -> FilePath
  -- ^ The build directory for this target
  -> BuildM [FilePath]
  -- ^ Returns the list of extra sources that were built
buildExtraSources description componentSourceGhcOptions wantDyn viewSources ghcProg buildTargetDir =
  BuildM $ \PreBuildComponentInputs{buildingWhat, localBuildInfo = lbi, targetInfo} ->
    let
      bi = componentBuildInfo (targetComponent targetInfo)
      verbosity = buildingWhatVerbosity buildingWhat
      clbi = targetCLBI targetInfo

      sources = viewSources (targetComponent targetInfo)

      comp = compiler lbi
      platform = hostPlatform lbi
      -- ROMES:TODO: Instead of keeping this logic here, we really just want to
      -- receive as an input the `neededWays` and build accordingly
      isGhcDynamic = isDynamic comp
      doingTH = usesTemplateHaskellOrQQ bi
      forceSharedLib = doingTH && isGhcDynamic
      runGhcProg = runGHC verbosity ghcProg comp platform

      buildAction sourceFile = do
        let baseSrcOpts =
              componentSourceGhcOptions
                verbosity
                lbi
                bi
                clbi
                buildTargetDir
                sourceFile
            vanillaSrcOpts
              -- Dynamic GHC requires C sources to be built
              -- with -fPIC for REPL to work. See #2207.
              | isGhcDynamic && wantDyn = baseSrcOpts{ghcOptFPic = toFlag True}
              | otherwise = baseSrcOpts
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
            -- TODO: Placing all Haskell, C, & C++ objects in a single directory
            --       Has the potential for file collisions. In general we would
            --       consider this a user error. However, we should strive to
            --       add a warning if this occurs.
            odir = fromFlag (ghcOptObjDir vanillaSrcOpts)
            compileIfNeeded opts = do
              needsRecomp <- checkNeedsRecompilation sourceFile opts
              when needsRecomp $ runGhcProg opts

        createDirectoryIfMissingVerbose verbosity True odir
        case targetComponent targetInfo of
          -- For libraries, we compile extra objects in the three ways: vanilla, shared, and profiled.
          -- We suffix shared objects with .dyn_o and profiled ones with .p_o.
          --
          -- ROMES:TODO: Should we use those suffixes for extra sources for
          -- executables too? We use those suffixes for haskell objects for
          -- executables ... (see gbuild)
          CLib _lib
            -- Unless for repl, in which case we only need the vanilla way
            | BuildRepl _ <- buildingWhat ->
                compileIfNeeded vanillaSrcOpts
            | otherwise ->
                do
                  compileIfNeeded vanillaSrcOpts
                  when (wantDyn && (forceSharedLib || withSharedLib lbi)) $
                    compileIfNeeded sharedSrcOpts{ghcOptObjSuffix = toFlag "dyn_o"}
                  when (withProfLib lbi) $
                    compileIfNeeded profSrcOpts{ghcOptObjSuffix = toFlag "p_o"}

          -- For foreign libraries, we determine with which options to build the
          -- objects (vanilla vs shared vs profiled)
          CFLib flib
            | withProfExe lbi -> -- ROMES:TODO: doesn't sound right "ProfExe" for FLib...
                compileIfNeeded profSrcOpts
            | withDynFLib flib && wantDyn ->
                compileIfNeeded sharedSrcOpts
            | otherwise ->
                compileIfNeeded vanillaSrcOpts
          -- For the remaining component types (Exec, Test, Bench), we also
          -- determine with which options to build the objects (vanilla vs shared vs
          -- profiled), but predicate is the same for the three kinds.
          _exeLike
            | withProfExe lbi ->
                compileIfNeeded profSrcOpts
            | withDynExe lbi && wantDyn ->
                compileIfNeeded sharedSrcOpts
            | otherwise ->
                compileIfNeeded vanillaSrcOpts
     in
      -- build any sources
      if (null sources || componentIsIndefinite clbi)
        then return []
        else do
          info verbosity ("Building " ++ description ++ "...")
          traverse_ buildAction sources
          return sources
