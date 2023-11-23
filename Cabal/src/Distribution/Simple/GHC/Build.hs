module Distribution.Simple.GHC.Build
  ( getRPaths
  , runReplOrWriteFlags
  , checkNeedsRecompilation
  , replNoLoad
  , componentGhcOptions
  , supportsDynamicToo
  , isDynamic
  , flibBuildName
  , flibTargetName
  , exeTargetName
  )
where

import Distribution.Compat.Prelude
import Prelude ()

import qualified Data.ByteString.Lazy.Char8 as BS
import Distribution.Compat.Binary (encode)
import Distribution.Compat.ResponseFile (escapeArgs)
import qualified Distribution.InstalledPackageInfo as IPI
import Distribution.Package
import Distribution.PackageDescription as PD
import Distribution.PackageDescription.Utils (cabalBug)
import Distribution.Pretty
import Distribution.Simple.BuildPaths
import Distribution.Simple.Compiler
import Distribution.Simple.Flag (Flag (..), fromFlag, fromFlagOrDefault)
import Distribution.Simple.GHC.ImplInfo
import qualified Distribution.Simple.GHC.Internal as Internal
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program
import Distribution.Simple.Program.GHC
import Distribution.Simple.Setup.Repl
import Distribution.Simple.Utils
import Distribution.System
import Distribution.Utils.NubList
import Distribution.Verbosity
import Distribution.Version
import System.Directory
  ( createDirectoryIfMissing
  , getCurrentDirectory
  )
import System.FilePath
  ( isRelative
  , replaceExtension
  , takeExtension
  , (<.>)
  , (</>)
  )

exeTargetName :: Platform -> Executable -> String
exeTargetName platform exe = unUnqualComponentName (exeName exe) `withExt` exeExtension platform

withExt :: FilePath -> String -> FilePath
withExt fp ext = fp <.> if takeExtension fp /= ('.' : ext) then ext else ""

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
    os =
      let (Platform _ os') = hostPlatform lbi
       in os'

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
    os =
      let (Platform _ os') = hostPlatform lbi
       in os'

    nm :: String
    nm = unUnqualComponentName $ foreignLibName flib

supportsDynamicToo :: Compiler -> Bool
supportsDynamicToo = Internal.ghcLookupProperty "Support dynamic-too"

isDynamic :: Compiler -> Bool
isDynamic = Internal.ghcLookupProperty "GHC Dynamic"

componentGhcOptions
  :: Verbosity
  -> LocalBuildInfo
  -> BuildInfo
  -> ComponentLocalBuildInfo
  -> FilePath
  -> GhcOptions
componentGhcOptions verbosity lbi =
  Internal.componentGhcOptions verbosity implInfo lbi
  where
    comp = compiler lbi
    implInfo = getImplInfo comp

replNoLoad :: Ord a => ReplOptions -> NubListR a -> NubListR a
replNoLoad replFlags l
  | replOptionsNoLoad replFlags == Flag True = mempty
  | otherwise = l

-- | Finds the object file name of the given source file
getObjectFileName :: FilePath -> GhcOptions -> FilePath
getObjectFileName filename opts = oname
  where
    odir = fromFlag (ghcOptObjDir opts)
    oext = fromFlagOrDefault "o" (ghcOptObjSuffix opts)
    oname = odir </> replaceExtension filename oext

-- | Returns True if the modification date of the given source file is newer than
-- the object file we last compiled for it, or if no object file exists yet.
checkNeedsRecompilation :: FilePath -> GhcOptions -> IO Bool
checkNeedsRecompilation filename opts = filename `moreRecentFile` oname
  where
    oname = getObjectFileName filename opts

-- | Calculate the RPATHs for the component we are building.
--
-- Calculates relative RPATHs when 'relocatable' is set.
getRPaths
  :: LocalBuildInfo
  -> ComponentLocalBuildInfo
  -- ^ Component we are building
  -> IO (NubListR FilePath)
getRPaths lbi clbi | supportRPaths hostOS = do
  libraryPaths <- depLibraryPaths False (relocatable lbi) lbi clbi
  let hostPref = case hostOS of
        OSX -> "@loader_path"
        _ -> "$ORIGIN"
      relPath p = if isRelative p then hostPref </> p else p
      rpaths = toNubListR (map relPath libraryPaths)
  return rpaths
  where
    (Platform _ hostOS) = hostPlatform lbi
    compid = compilerId . compiler $ lbi

    -- The list of RPath-supported operating systems below reflects the
    -- platforms on which Cabal's RPATH handling is tested. It does _NOT_
    -- reflect whether the OS supports RPATH.

    -- E.g. when this comment was written, the *BSD operating systems were
    -- untested with regards to Cabal RPATH handling, and were hence set to
    -- 'False', while those operating systems themselves do support RPATH.
    supportRPaths Linux = True
    supportRPaths Windows = False
    supportRPaths OSX = True
    supportRPaths FreeBSD =
      case compid of
        CompilerId GHC ver | ver >= mkVersion [7, 10, 2] -> True
        _ -> False
    supportRPaths OpenBSD = False
    supportRPaths NetBSD = False
    supportRPaths DragonFly = False
    supportRPaths Solaris = False
    supportRPaths AIX = False
    supportRPaths HPUX = False
    supportRPaths IRIX = False
    supportRPaths HaLVM = False
    supportRPaths IOS = False
    supportRPaths Android = False
    supportRPaths Ghcjs = False
    supportRPaths Wasi = False
    supportRPaths Hurd = True
    supportRPaths Haiku = False
    supportRPaths (OtherOS _) = False
-- Do _not_ add a default case so that we get a warning here when a new OS
-- is added.

getRPaths _ _ = return mempty

runReplOrWriteFlags
  :: Verbosity
  -> ConfiguredProgram
  -> Compiler
  -> Platform
  -> ReplOptions
  -> GhcOptions
  -> BuildInfo
  -> ComponentLocalBuildInfo
  -> PackageName
  -> IO ()
runReplOrWriteFlags verbosity ghcProg comp platform rflags replOpts bi clbi pkg_name =
  case replOptionsFlagOutput rflags of
    NoFlag -> runGHC verbosity ghcProg comp platform replOpts
    Flag out_dir -> do
      src_dir <- getCurrentDirectory
      let uid = componentUnitId clbi
          this_unit = prettyShow uid
          reexported_modules = [mn | LibComponentLocalBuildInfo{} <- [clbi], IPI.ExposedModule mn (Just{}) <- componentExposedModules clbi]
          hidden_modules = otherModules bi
          extra_opts =
            concat $
              [ ["-this-package-name", prettyShow pkg_name]
              , ["-working-dir", src_dir]
              ]
                ++ [ ["-reexported-module", prettyShow m] | m <- reexported_modules
                   ]
                ++ [ ["-hidden-module", prettyShow m] | m <- hidden_modules
                   ]
      -- Create "paths" subdirectory if it doesn't exist. This is where we write
      -- information about how the PATH was augmented.
      createDirectoryIfMissing False (out_dir </> "paths")
      -- Write out the PATH information into `paths` subdirectory.
      writeFileAtomic (out_dir </> "paths" </> this_unit) (encode ghcProg)
      -- Write out options for this component into a file ready for loading into
      -- the multi-repl
      writeFileAtomic (out_dir </> this_unit) $
        BS.pack $
          escapeArgs $
            extra_opts ++ renderGhcOptions comp platform (replOpts{ghcOptMode = NoFlag})
