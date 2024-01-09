{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Distribution.Simple.Build.ExtraSources where

import Control.Monad
import Data.Foldable
import Distribution.Simple.Flag
import qualified Distribution.Simple.GHC.Internal as Internal
import Distribution.Simple.Program.GHC
import Distribution.Simple.Utils

import Distribution.Simple.Program.Builtin (ghcProgram)
import Distribution.Types.BuildInfo
import Distribution.Types.Component
import Distribution.Types.TargetInfo

import Distribution.Simple.GHC.Build
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program (requireProgram)
import Distribution.Types.ComponentName (componentNameRaw)
import Distribution.Types.Executable
import Distribution.Verbosity (Verbosity)
import System.FilePath

import Distribution.Simple.Build.Monad

buildAllExtraSources :: BuildM ()
buildAllExtraSources =
  sequence_
    [ buildCSources
    , buildCxxSources
    , buildJsSources
    , buildAsmSources
    , buildCmmSources
    ]

-- ROMES:TODO:
-- unless (not hasJsSupport || null jsSrcs) $ ... and (not has_code)
-- where has_code = not (componentIsIndefinite clbi)

-- ROMES:PATCH:NOTE: Worry about mimicking the current behaviour first, and only
-- later worry about dependency tracking and ghc -M, gcc -M, or ghc -optc-MD ...

-- ROMES:TODO:
-- How should we handle a C source depending on a stub generated from a foreign export?

buildCSources
  , buildCxxSources
  , buildJsSources
  , buildAsmSources
  , buildCmmSources
    :: BuildM ()
-- Currently, an executable main file may be a C++ or C file, in which case we want to
-- compile it alongside other C/C++ sources. Eventually, we may be able to
-- compile other main files as build sources (e.g. ObjC...). This functionality
-- may also be provided in standalone packages, since nothing precludes users
-- from writing their own build rules for declared foreign modules in main-is
-- and eventually custom stanzas.
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
buildJsSources =
  buildExtraSources
    "JS Sources"
    Internal.componentJsGhcOptions
    False
    (jsSources . componentBuildInfo)
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
  -> BuildM ()
buildExtraSources description componentSourceGhcOptions wantDyn viewSources =
  BuildM \PreBuildComponentInputs{buildingWhat, localBuildInfo = lbi, targetInfo} ->
    let
      bi = componentBuildInfo (targetComponent targetInfo)
      verbosity = buildingWhatVerbosity buildingWhat
      clbi = targetCLBI targetInfo

      sources = viewSources (targetComponent targetInfo)

      comp = compiler lbi
      platform = hostPlatform lbi
      isGhcDynamic = isDynamic comp
      doingTH = usesTemplateHaskellOrQQ bi
      forceSharedLib = doingTH && isGhcDynamic

      buildAction sourceFile = do
        (ghcProg, _) <- requireProgram verbosity ghcProgram (withPrograms lbi)
        let runGhcProg = runGHC verbosity ghcProg comp platform

        let baseSrcOpts =
              componentSourceGhcOptions
                verbosity
                lbi
                bi
                clbi
                buildDir'
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
            | withProfExe lbi -> -- ROMES: hmm... doesn't sound right.
                compileIfNeeded profSrcOpts
            | flibIsDynamic flib ->
                compileIfNeeded sharedSrcOpts
            | otherwise ->
                compileIfNeeded vanillaSrcOpts
          -- For the remaining component types (Exec, Test, Bench), we also
          -- determine with which options to build the objects (vanilla vs shared vs
          -- profiled), but predicate is the same for the three kinds.
          _exeLike
            | withProfExe lbi ->
                compileIfNeeded profSrcOpts
            | withDynExe lbi ->
                compileIfNeeded sharedSrcOpts
            | otherwise ->
                compileIfNeeded vanillaSrcOpts

      -- Until we get rid of the "exename-tmp" directory within the executable
      -- build dir, we need to accommodate that fact (see eg @tmpDir@ in @gbuild@)
      -- This is a workaround for #9498 until it is fixed.
      cname = componentName (targetComponent targetInfo)
      buildDir'
        | CLibName{} <- cname =
            componentBuildDir lbi clbi
        | CNotLibName{} <- cname =
            componentBuildDir lbi clbi
              </> componentNameRaw cname <> "-tmp"
     in
      do
        -- build any sources
        unless (null sources) $ do
          info verbosity ("Building " ++ description ++ "...")
          traverse_ buildAction sources
