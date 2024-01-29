module Distribution.Simple.GHC.Build.Utils where

import Distribution.Compat.Prelude
import Prelude ()

import Control.Monad (msum)
import Data.Char (isLower)
import Distribution.ModuleName (ModuleName)
import qualified Distribution.ModuleName as ModuleName
import Distribution.PackageDescription as PD
import Distribution.PackageDescription.Utils (cabalBug)
import Distribution.Simple.BuildPaths
import Distribution.Simple.Compiler
import qualified Distribution.Simple.GHC.Internal as Internal
import Distribution.Simple.Program.GHC
import Distribution.Simple.Setup.Common
import Distribution.Simple.Utils
import Distribution.System
import Distribution.Types.LocalBuildInfo
import Distribution.Utils.Path (getSymbolicPath)
import Distribution.Verbosity
import System.FilePath
  ( replaceExtension
  , takeExtension
  , (<.>)
  , (</>)
  )

-- | Find the path to the entry point of an executable (typically specified in
-- @main-is@, and found in @hs-source-dirs@).
findExecutableMain
  :: Verbosity
  -> FilePath
  -- ^ Build directory
  -> (BuildInfo, FilePath)
  -- ^ The build info and module path of an executable-like component (Exe, Test, Bench)
  -> IO FilePath
  -- ^ The path to the main source file.
findExecutableMain verbosity bdir (bnfo, modPath) =
  findFileEx verbosity (bdir : map getSymbolicPath (hsSourceDirs bnfo)) modPath

-- | Does this compiler support the @-dynamic-too@ option
supportsDynamicToo :: Compiler -> Bool
supportsDynamicToo = Internal.ghcLookupProperty "Support dynamic-too"

-- | Is this compiler's RTS dynamically linked?
isDynamic :: Compiler -> Bool
isDynamic = Internal.ghcLookupProperty "GHC Dynamic"

-- | Should we dynamically link the foreign library, based on its 'foreignLibType'?
withDynFLib :: ForeignLib -> Bool
withDynFLib flib =
  case foreignLibType flib of
    ForeignLibNativeShared ->
      ForeignLibStandalone `notElem` foreignLibOptions flib
    ForeignLibNativeStatic ->
      False
    ForeignLibTypeUnknown ->
      cabalBug "unknown foreign lib type"

-- | Is this file a C++ source file, i.e. ends with .cpp, .cxx, or .c++?
isCxx :: FilePath -> Bool
isCxx fp = elem (takeExtension fp) [".cpp", ".cxx", ".c++"]

-- | Is this a C source file, i.e. ends with .c?
isC :: FilePath -> Bool
isC fp = elem (takeExtension fp) [".c"]

-- | FilePath has a Haskell extension: .hs or .lhs
isHaskell :: FilePath -> Bool
isHaskell fp = elem (takeExtension fp) [".hs", ".lhs"]

-- | Returns True if the modification date of the given source file is newer than
-- the object file we last compiled for it, or if no object file exists yet.
checkNeedsRecompilation :: FilePath -> GhcOptions -> IO Bool
checkNeedsRecompilation filename opts = filename `moreRecentFile` oname
  where
    oname = getObjectFileName filename opts

-- | Finds the object file name of the given source file
getObjectFileName :: FilePath -> GhcOptions -> FilePath
getObjectFileName filename opts = oname
  where
    odir = fromFlag (ghcOptObjDir opts)
    oext = fromFlagOrDefault "o" (ghcOptObjSuffix opts)
    oname = odir </> replaceExtension filename oext

-- | Target name for a foreign library (the actual file name)
--
-- We do not use mkLibName and co here because the naming for foreign libraries
-- is slightly different (we don't use "_p" or compiler version suffices, and we
-- don't want the "lib" prefix on Windows).
--
-- TODO: We do use `dllExtension` and co here, but really that's wrong: they
-- use the OS used to build cabal to determine which extension to use, rather
-- than the target OS (but this is wrong elsewhere in Cabal as well).
flibTargetName :: LocalBuildInfo -> ForeignLib -> String
flibTargetName lbi flib =
  case (os, foreignLibType flib) of
    (Windows, ForeignLibNativeShared) -> nm <.> "dll"
    (Windows, ForeignLibNativeStatic) -> nm <.> "lib"
    (Linux, ForeignLibNativeShared) -> "lib" ++ nm <.> versionedExt
    (_other, ForeignLibNativeShared) ->
      "lib" ++ nm <.> dllExtension (hostPlatform lbi)
    (_other, ForeignLibNativeStatic) ->
      "lib" ++ nm <.> staticLibExtension (hostPlatform lbi)
    (_any, ForeignLibTypeUnknown) -> cabalBug "unknown foreign lib type"
  where
    nm :: String
    nm = unUnqualComponentName $ foreignLibName flib

    os :: OS
    Platform _ os = hostPlatform lbi

    -- If a foreign lib foo has lib-version-info 5:1:2 or
    -- lib-version-linux 3.2.1, it should be built as libfoo.so.3.2.1
    -- Libtool's version-info data is translated into library versions in a
    -- nontrivial way: so refer to libtool documentation.
    versionedExt :: String
    versionedExt =
      let nums = foreignLibVersion flib os
       in foldl (<.>) "so" (map show nums)

-- | Name for the library when building.
--
-- If the `lib-version-info` field or the `lib-version-linux` field of
-- a foreign library target is set, we need to incorporate that
-- version into the SONAME field.
--
-- If a foreign library foo has lib-version-info 5:1:2, it should be
-- built as libfoo.so.3.2.1.  We want it to get soname libfoo.so.3.
-- However, GHC does not allow overriding soname by setting linker
-- options, as it sets a soname of its own (namely the output
-- filename), after the user-supplied linker options.  Hence, we have
-- to compile the library with the soname as its filename.  We rename
-- the compiled binary afterwards.
--
-- This method allows to adjust the name of the library at build time
-- such that the correct soname can be set.
flibBuildName :: LocalBuildInfo -> ForeignLib -> String
flibBuildName lbi flib
  -- On linux, if a foreign-library has version data, the first digit is used
  -- to produce the SONAME.
  | (os, foreignLibType flib)
      == (Linux, ForeignLibNativeShared) =
      let nums = foreignLibVersion flib os
       in "lib" ++ nm <.> foldl (<.>) "so" (map show (take 1 nums))
  | otherwise = flibTargetName lbi flib
  where
    os :: OS
    Platform _ os = hostPlatform lbi

    nm :: String
    nm = unUnqualComponentName $ foreignLibName flib

-- | Gets the target name (name of actual executable file) from the name of an
-- executable-like component ('Executable', 'TestSuite', 'Benchmark').
exeTargetName :: Platform -> UnqualComponentName -> String
exeTargetName platform name = unUnqualComponentName name `withExt` exeExtension platform
  where
    withExt :: FilePath -> String -> FilePath
    withExt fp ext = fp <.> if takeExtension fp /= ('.' : ext) then ext else ""

-- | "Main" module name when overridden by @ghc-options: -main-is ...@
-- or 'Nothing' if no @-main-is@ flag could be found.
--
-- In case of 'Nothing', 'Distribution.ModuleName.main' can be assumed.
exeMainModuleName
  :: BuildInfo
  -- ^ The build info of the executable-like component (Exe, Test, Bench)
  -> ModuleName
exeMainModuleName bnfo =
  -- GHC honors the last occurrence of a module name updated via -main-is
  --
  -- Moreover, -main-is when parsed left-to-right can update either
  -- the "Main" module name, or the "main" function name, or both,
  -- see also 'decodeMainIsArg'.
  fromMaybe ModuleName.main $ msum $ reverse $ map decodeMainIsArg $ findIsMainArgs ghcopts
  where
    ghcopts = hcOptions GHC bnfo

    findIsMainArgs [] = []
    findIsMainArgs ("-main-is" : arg : rest) = arg : findIsMainArgs rest
    findIsMainArgs (_ : rest) = findIsMainArgs rest

-- | Decode argument to '-main-is'
--
-- Returns 'Nothing' if argument set only the function name.
--
-- This code has been stolen/refactored from GHC's DynFlags.setMainIs
-- function. The logic here is deliberately imperfect as it is
-- intended to be bug-compatible with GHC's parser. See discussion in
-- https://github.com/haskell/cabal/pull/4539#discussion_r118981753.
decodeMainIsArg :: String -> Maybe ModuleName
decodeMainIsArg arg
  | headOf main_fn isLower =
      -- The arg looked like "Foo.Bar.baz"
      Just (ModuleName.fromString main_mod)
  | headOf arg isUpper -- The arg looked like "Foo" or "Foo.Bar"
    =
      Just (ModuleName.fromString arg)
  | otherwise -- The arg looked like "baz"
    =
      Nothing
  where
    headOf :: String -> (Char -> Bool) -> Bool
    headOf str pred' = any pred' (safeHead str)

    (main_mod, main_fn) = splitLongestPrefix arg (== '.')

    splitLongestPrefix :: String -> (Char -> Bool) -> (String, String)
    splitLongestPrefix str pred'
      | null r_pre = (str, [])
      | otherwise = (reverse (safeTail r_pre), reverse r_suf)
      where
        -- 'safeTail' drops the char satisfying 'pred'
        (r_suf, r_pre) = break pred' (reverse str)
