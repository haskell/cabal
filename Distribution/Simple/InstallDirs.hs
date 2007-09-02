{-# OPTIONS -cpp -fffi #-}
{-# OPTIONS_GHC -w #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://hackage.haskell.org/trac/ghc/wiki/WorkingConventions#Warnings
-- for details

-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.InstallDirs
-- Copyright   :  Isaac Jones 2003-2004
--
-- Maintainer  :  Isaac Jones <ijones@syntaxpolice.org>
-- Stability   :  alpha
-- Portability :  portable
--
-- Definition of the 'LocalBuildInfo' data type.  This is basically
-- the information that is gathered by the end of the configuration
-- step which could include package information from ghc-pkg, flags
-- the user passed to configure, and the location of tools in the
-- PATH.

{- All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Isaac Jones nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. -}

module Distribution.Simple.InstallDirs (
	InstallDirs(..), haddockdir,
        InstallDirTemplates(..),
        defaultInstallDirs,
        absoluteInstallDirs,
        prefixRelativeInstallDirs,

        PathTemplate,
        PathTemplateVariable(..),
        toPathTemplate,
        fromPathTemplate,
        substPathTemplate,
        initialPathTemplateEnv,
  ) where


import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import System.FilePath ((</>), isPathSeparator)
#if __HUGS__ || __GLASGOW_HASKELL__ > 606
import System.FilePath (dropDrive)
#endif

import Distribution.Package (PackageIdentifier(..), showPackageId)
import Distribution.PackageDescription (PackageDescription(package))
import Data.Version (showVersion)
import Distribution.System (OS(..), os)
import Distribution.Simple.Compiler (CompilerFlavor(..))
-- TODO: move CopyDest to this module
import Distribution.Simple.Setup (CopyDest(..))

#if mingw32_HOST_OS || mingw32_TARGET_OS
import Foreign
import Foreign.C
#endif

-- ---------------------------------------------------------------------------
-- Instalation directories


-- | The directories where we will install files for packages.
--
-- We have several different directories for different types of files since
-- many systems have conventions whereby different types of files in a package
-- are installed in different direcotries. This is particularly the case on
-- unix style systems.
--
data InstallDirs dir = InstallDirs {
        prefix     :: dir,
        bindir     :: dir,
        libdir     :: dir,
        dynlibdir  :: dir,
        libexecdir :: dir,
        progdir    :: dir,
        includedir :: dir,
        datadir    :: dir,
        docdir     :: dir,
        htmldir    :: dir
    } deriving (Read, Show)

-- | The installation dirctories in terms of 'PathTemplate's that contain
-- variables.
--
-- The defaults for most of the directories are relative to each other, in
-- particular they are all relative to a single prefix. This makes it
-- convenient for the user to override the default installation directory
-- by only having to specify --prefix=... rather than overriding each
-- individually. This is done by allowing $-style variables in the dirs.
-- These are expanded by textual substituion (see 'substPathTemplate').
--
-- A few of these installation directories are split into two components, the
-- dir and subdir. The full installation path is formed by combining the two
-- together with @<\/>@. The reason for this is compatability with other unix
-- build systems which also support @--libdir@ and @--datadir@. We would like
-- users to be able to configure @--libdir=\/usr\/lib64@ for example but
-- because by default we want to support installing multiplve versions of
-- packages and building the same package for multiple compilers we append the
-- libdubdir to get: @\/usr\/lib64\/$pkgid\/$compiler@.
--
-- An additional complication is the need to support relocatable packages on
-- systems which support such things, like Windows.
--
data InstallDirTemplates = InstallDirTemplates {
        prefixDirTemplate  :: PathTemplate,
        binDirTemplate     :: PathTemplate,
        libDirTemplate     :: PathTemplate,
        libSubdirTemplate  :: PathTemplate,
        libexecDirTemplate :: PathTemplate,
        progDirTemplate    :: PathTemplate,
        includeDirTemplate :: PathTemplate,
        dataDirTemplate    :: PathTemplate,
        dataSubdirTemplate :: PathTemplate,
        docDirTemplate     :: PathTemplate,
        htmlDirTemplate    :: PathTemplate
    } deriving (Read, Show)

-- ---------------------------------------------------------------------------
-- Default installation directories

defaultInstallDirs :: CompilerFlavor -> Bool -> IO InstallDirTemplates
defaultInstallDirs comp hasLibs = do
  windowsProgramFilesDir <- getWindowsProgramFilesDir
  let prefixDir    = case os of
        Windows _ -> windowsProgramFilesDir </> "Haskell"
        _other    -> "/usr/local"
      binDir       = "$prefix" </> "bin"
      libDir       = case os of
        Windows _ -> "$prefix"
        _other    -> "$prefix" </> "lib"
      libSubdir    = case comp of
           Hugs   -> "hugs" </> "packages" </> "$pkg"
           JHC    -> "$compiler"
           _other -> "$pkgid" </> "$compiler"
      libexecDir   = case os of
        Windows _ -> "$prefix" </> "$pkgid"
        _other    -> "$prefix" </> "libexec"
      progDir      = "$libdir" </> "hugs" </> "programs"
      includeDir   = "$libdir" </> "$libsubdir" </> "include"
      dataDir      = case os of
        Windows _  | hasLibs   -> windowsProgramFilesDir </> "Haskell"
                   | otherwise -> "$prefix"
        _other    -> "$prefix" </> "share"
      dataSubdir   = "$pkgid"
      docDir       = case os of
        Windows _ -> "$prefix"  </> "doc" </> "$pkgid"
	_other    -> "$datadir" </> "doc" </> "$pkgid"
      htmlDir      = "$docdir"  </> "html"
  return InstallDirTemplates {
      prefixDirTemplate  = toPathTemplate prefixDir,
      binDirTemplate     = toPathTemplate binDir,
      libDirTemplate     = toPathTemplate libDir,
      libSubdirTemplate  = toPathTemplate libSubdir,
      libexecDirTemplate = toPathTemplate libexecDir,
      progDirTemplate    = toPathTemplate progDir,
      includeDirTemplate = toPathTemplate includeDir,
      dataDirTemplate    = toPathTemplate dataDir,
      dataSubdirTemplate = toPathTemplate dataSubdir,
      docDirTemplate     = toPathTemplate docDir,
      htmlDirTemplate    = toPathTemplate htmlDir
    }

haddockdir :: InstallDirs FilePath -> PackageDescription -> FilePath
haddockdir installDirs pkg_descr =
  htmldir installDirs </> pkgName (package pkg_descr)

-- ---------------------------------------------------------------------------
-- Converting directories, absolute or prefix-relative

-- | Substitute the install dir templates into each other.
--
-- To prevent cyclic substitutions, only some variables are allowed in
-- particular dir templates. If out of scope vars are present, they are not
-- substituted for. Checking for any remaining unsubstituted vars can be done
-- as a subsequent operation.
--
-- The reason it is done this way is so that in 'prefixRelativeInstallDirs' we
-- can replace 'prefixDirTemplate' with the 'PrefixVar' and get resulting
-- 'PathTemplate's that still have the 'PrefixVar' in them. Doing this makes it
-- each to check which paths are relative to the $prefix.
--
substituteTemplates :: PackageIdentifier -> PackageIdentifier
                    -> InstallDirTemplates -> InstallDirTemplates
substituteTemplates pkgId compilerId dirs = dirs'
  where
    dirs' = InstallDirTemplates {
      -- So this specifies exactly which vars are allowed in each template
      prefixDirTemplate  = subst prefixDirTemplate  [],
      binDirTemplate     = subst binDirTemplate     [prefixDirVar],
      libDirTemplate     = subst libDirTemplate     [prefixDirVar, binDirVar],
      libSubdirTemplate  = subst libSubdirTemplate  [],
      libexecDirTemplate = subst libexecDirTemplate prefixBinLibVars,
      progDirTemplate    = subst progDirTemplate    prefixBinLibVars,
      includeDirTemplate = subst includeDirTemplate prefixBinLibVars,
      dataDirTemplate    = subst dataDirTemplate    prefixBinLibVars,
      dataSubdirTemplate = subst dataSubdirTemplate [],
      docDirTemplate     = subst docDirTemplate   $ prefixBinLibVars
                             ++ [dataDirVar, dataSubdirVar],
      htmlDirTemplate    = subst htmlDirTemplate  $ prefixBinLibVars
                             ++ [dataDirVar, dataSubdirVar, docDirVar]
    }
    -- The initial environment has all the static stuff but no paths
    env = initialPathTemplateEnv pkgId compilerId
    subst dir env' = substPathTemplate (env'++env) (dir dirs)

    prefixDirVar     = (PrefixVar,     prefixDirTemplate  dirs')
    binDirVar        = (BinDirVar,     binDirTemplate     dirs')
    libDirVar        = (LibDirVar,     libDirTemplate     dirs')
    libSubdirVar     = (LibSubdirVar,  libSubdirTemplate  dirs')
    dataDirVar       = (DataDirVar,    dataDirTemplate    dirs')
    dataSubdirVar    = (DataSubdirVar, dataSubdirTemplate dirs')
    docDirVar        = (DocDirVar,     docDirTemplate     dirs')
    prefixBinLibVars = [prefixDirVar, binDirVar, libDirVar, libSubdirVar]

-- | Convert from abstract install directories to actual absolute ones by
-- substituting for all the variables in the abstract paths, to get real
-- absolute path.
absoluteInstallDirs :: PackageIdentifier -> PackageIdentifier -> CopyDest
                    -> InstallDirTemplates -> InstallDirs FilePath
absoluteInstallDirs pkgId compilerId copydest dirs =
  InstallDirs {
    prefix     = copy $ path prefixDirTemplate,
    bindir     = copy $ path binDirTemplate,
    libdir     = copy $ path libDirTemplate </> path libSubdirTemplate,
    dynlibdir  = copy $ path libDirTemplate,
    libexecdir = copy $ path libexecDirTemplate,
    progdir    = copy $ path progDirTemplate,
    includedir = copy $ path includeDirTemplate,
    datadir    = copy $ path dataDirTemplate </> path dataSubdirTemplate,
    docdir     = copy $ path docDirTemplate,
    htmldir    = copy $ path htmlDirTemplate
  }
  where
    dirs' = substituteTemplates pkgId compilerId dirs {
              prefixDirTemplate = case copydest of
                -- possibly override the prefix
	        CopyPrefix p -> toPathTemplate p
                _            -> prefixDirTemplate dirs
            }
    path dir = case dir dirs' of
                 PathTemplate cs -> concat [ c | Ordinary c <- cs ]
    copy dir = case copydest of
      CopyTo destdir -> destdir </> dropDrive dir
      _              ->                       dir

-- | Check which of the paths are relative to the installation $prefix.
--
-- If any of the paths are not relative, ie they are absolute paths, then it
-- prevents us from making a relocatable package (also known as a \"prefix
-- independent\" package).
--
prefixRelativeInstallDirs :: PackageIdentifier -> PackageIdentifier
                          -> InstallDirTemplates
                          -> InstallDirs (Maybe FilePath)
prefixRelativeInstallDirs pkgId compilerId dirs =
  InstallDirs {
    prefix     = relative prefixDirTemplate,
    bindir     = relative binDirTemplate,
    libdir     = (flip fmap) (relative libDirTemplate) (</> path libSubdirTemplate),
    dynlibdir  = (relative libDirTemplate),
    libexecdir = relative libexecDirTemplate,
    progdir    = relative progDirTemplate,
    includedir = relative includeDirTemplate,
    datadir    = (flip fmap) (relative dataDirTemplate) (</> path dataSubdirTemplate),
    docdir     = relative docDirTemplate,
    htmldir    = relative htmlDirTemplate
  }
  where
    -- substitute the path template into each other, except that we map
    -- $prefix back to $prefix. We're trying to end up with templates that
    -- mention no vars except $prefix.
    dirs' = substituteTemplates pkgId compilerId dirs {
              prefixDirTemplate = PathTemplate [Variable PrefixVar]
            }
    -- If it starts with $prefix then it's relative and produce the relative
    -- path by stripping off $prefix/ or $prefix
    relative dir = case dir dirs' of
      PathTemplate cs -> fmap (fromPathTemplate . PathTemplate) (relative' cs)
    relative' (Variable PrefixVar : Ordinary (s:rest) : rest')
                      | isPathSeparator s = Just (Ordinary rest : rest')
    relative' (Variable PrefixVar : rest) = Just rest
    relative' _                           = Nothing
    path dir = fromPathTemplate (dir dirs')

-- ---------------------------------------------------------------------------
-- Path templates

-- | An abstract path, posibly containing variables that need to be
-- substituted for to get a real 'FilePath'.
--
newtype PathTemplate = PathTemplate [PathComponent]

data PathComponent =
       Ordinary FilePath
     | Variable PathTemplateVariable

data PathTemplateVariable =
       PrefixVar     -- ^ The @$prefix@ path variable
     | BinDirVar     -- ^ The @$bindir@ path variable
     | LibDirVar     -- ^ The @$libdir@ path variable
     | LibSubdirVar  -- ^ The @$libsubdir@ path variable
     | DataDirVar    -- ^ The @$datadir@ path variable
     | DataSubdirVar -- ^ The @$datasubdir@ path variable
     | DocDirVar     -- ^ The @$docdir@ path variable
     | PkgNameVar    -- ^ The @$pkg@ package name path variable
     | PkgVerVar     -- ^ The @$version@ package version path variable
     | PkgIdVar      -- ^ The @$pkgid@ package Id path variable, eg @foo-1.0@
     | CompilerVar   -- ^ The compiler name and version, eg @ghc-6.6.1@
  deriving Eq

-- | Convert a 'FilePath' to a 'PathTemplate' including any template vars.
--
toPathTemplate :: FilePath -> PathTemplate
toPathTemplate = PathTemplate . read

-- | Convert back to a path, ingoring any remaining vars
--
fromPathTemplate :: PathTemplate -> FilePath
fromPathTemplate (PathTemplate cs) = concat [ c | Ordinary c <- cs ]

substPathTemplate :: [(PathTemplateVariable, PathTemplate)]
                  -> PathTemplate -> PathTemplate
substPathTemplate environment (PathTemplate template) =
    PathTemplate (concatMap subst template)

    where subst component@(Ordinary _) = [component]
          subst component@(Variable variable) =
              case lookup variable environment of
                  Just (PathTemplate components) -> components
                  Nothing                        -> [component]

-- | The initial environment has all the static stuff but no paths
initialPathTemplateEnv :: PackageIdentifier -> PackageIdentifier
                       -> [(PathTemplateVariable, PathTemplate)]
initialPathTemplateEnv pkgId compilerId =
  map (\(v,s) -> (v, PathTemplate [Ordinary s]))
  [(PkgNameVar,  pkgName pkgId)
  ,(PkgVerVar,   showVersion (pkgVersion pkgId))
  ,(PkgIdVar,    showPackageId pkgId)
  ,(CompilerVar, showPackageId compilerId)]

-- ---------------------------------------------------------------------------
-- Parsing and showing path templates:

-- The textual format is that of an ordinary Haskell String, eg
-- "$prefix/bin"
-- and this gets parsed to the internal representation as a sequence of path
-- spans which are either strings or variables, eg:
-- PathTemplate [Variable PrefixVar, Ordinary "/bin" ]

instance Show PathTemplateVariable where
  show PrefixVar     = "prefix"
  show BinDirVar     = "bindir"
  show LibDirVar     = "libdir"
  show LibSubdirVar  = "libsubdir"
  show DataDirVar    = "datadir"
  show DataSubdirVar = "datasubdir"
  show DocDirVar     = "docdir"
  show PkgNameVar    = "pkg"
  show PkgVerVar     = "version"
  show PkgIdVar      = "pkgid"
  show CompilerVar   = "compiler"

instance Read PathTemplateVariable where
  readsPrec _ s =
    take 1
    [ (var, drop (length varStr) s)
    | (varStr, var) <- vars
    , varStr `isPrefixOf` s ]
    where vars = [("prefix",     PrefixVar)
	         ,("bindir",     BinDirVar)
	         ,("libdir",     LibDirVar)
	         ,("libsubdir",  LibSubdirVar)
	         ,("datadir",    DataDirVar)
	         ,("datasubdir", DataSubdirVar)
	         ,("docdir",     DocDirVar)
	         ,("pkgid",      PkgIdVar)
	         ,("pkg",        PkgNameVar)
	         ,("version",    PkgVerVar)
	         ,("compiler",   CompilerVar)]

instance Show PathComponent where
  show (Ordinary path) = path
  show (Variable var)  = '$':show var
  showList = foldr (\x -> (shows x .)) id

instance Read PathComponent where
  -- for some reason we colapse multiple $ symbols here
  readsPrec _ = lex0
    where lex0 [] = []
          lex0 ('$':'$':s') = lex0 ('$':s')
          lex0 ('$':s') = case [ (Variable var, s'')
                               | (var, s'') <- reads s' ] of
		            [] -> lex1 "$" s'
		            ok -> ok
          lex0 s' = lex1 [] s'
          lex1 ""  ""      = []
          lex1 acc ""      = [(Ordinary (reverse acc), "")]
          lex1 acc ('$':'$':s) = lex1 acc ('$':s)
          lex1 acc ('$':s) = [(Ordinary (reverse acc), '$':s)]
          lex1 acc (c:s)   = lex1 (c:acc) s
  readList [] = [([],"")]
  readList s  = [ (component:components, s'')
                | (component, s') <- reads s
                , (components, s'') <- readList s' ]

instance Show PathTemplate where
  show (PathTemplate template) = show (show template)

instance Read PathTemplate where
  readsPrec p s = [ (PathTemplate template, s')
                  | (path, s')     <- readsPrec p s
                  , (template, "") <- reads path ]

-- ---------------------------------------------------------------------------
-- Internal utilities

getWindowsProgramFilesDir :: IO FilePath
getWindowsProgramFilesDir = do
#if mingw32_HOST_OS || mingw32_TARGET_OS
  m <- shGetFolderPath csidl_PROGRAM_FILES
#else
  let m = Nothing
#endif
  return (fromMaybe "C:\\Program Files" m)

#if mingw32_HOST_OS || mingw32_TARGET_OS
shGetFolderPath :: CInt -> IO (Maybe FilePath)
shGetFolderPath n =
# if __HUGS__
  return Nothing
# else
  allocaBytes long_path_size $ \pPath -> do
     r <- c_SHGetFolderPath nullPtr n nullPtr 0 pPath
     if (r /= 0)
	then return Nothing
	else do s <- peekCString pPath; return (Just s)
  where
    long_path_size      = 1024
# endif

csidl_PROGRAM_FILES, csidl_PROGRAM_FILES_COMMON :: CInt
csidl_PROGRAM_FILES = 0x0026
csidl_PROGRAM_FILES_COMMON = 0x002b

foreign import stdcall unsafe "shlobj.h SHGetFolderPathA"
            c_SHGetFolderPath :: Ptr ()
                              -> CInt
                              -> Ptr ()
                              -> CInt
                              -> CString
                              -> IO CInt
#endif

#if !(__HUGS__ || __GLASGOW_HASKELL__ > 606)
-- Compat: this function only appears in FilePath > 1.0
-- (which at the time of writing is unreleased)
dropDrive :: FilePath -> FilePath
dropDrive (c:cs) | isPathSeparator c = cs
dropDrive (_:':':c:cs) | isWindows
                      && isPathSeparator c = cs  -- path with drive letter
dropDrive (_:':':cs)   | isWindows         = cs
dropDrive cs = cs

isWindows :: Bool
isWindows = case os of
  Windows _ -> True
  _         -> False
#endif
