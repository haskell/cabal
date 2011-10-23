{-# LANGUAGE CPP, ForeignFunctionInterface #-}
{-# OPTIONS_NHC98 -cpp #-}
{-# OPTIONS_JHC -fcpp -fffi #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.InstallDirs
-- Copyright   :  Isaac Jones 2003-2004
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This manages everything to do with where files get installed (though does
-- not get involved with actually doing any installation). It provides an
-- 'InstallDirs' type which is a set of directories for where to install
-- things. It also handles the fact that we use templates in these install
-- dirs. For example most install dirs are relative to some @$prefix@ and by
-- changing the prefix all other dirs still end up changed appropriately. So it
-- provides a 'PathTemplate' type and functions for substituting for these
-- templates.

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
        InstallDirs(..),
        InstallDirTemplates,
        defaultInstallDirs,
        combineInstallDirs,
        absoluteInstallDirs,
        CopyDest(..),
        prefixRelativeInstallDirs,
        substituteInstallDirTemplates,

        PathTemplate,
        PathTemplateVariable(..),
        toPathTemplate,
        fromPathTemplate,
        substPathTemplate,
        initialPathTemplateEnv,
        platformTemplateEnv,
        compilerTemplateEnv,
        packageTemplateEnv,
        installDirsTemplateEnv,
  ) where


import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid(..))
import System.Directory (getAppUserDataDirectory)
import System.FilePath ((</>), isPathSeparator, pathSeparator)
#if __HUGS__ || __GLASGOW_HASKELL__ > 606
import System.FilePath (dropDrive)
#endif

import Distribution.Package
         ( PackageIdentifier, packageName, packageVersion )
import Distribution.System
         ( OS(..), buildOS, Platform(..), buildPlatform )
import Distribution.Compiler
         ( CompilerId, CompilerFlavor(..) )
import Distribution.Text
         ( display )

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
        prefix       :: dir,
        bindir       :: dir,
        libdir       :: dir,
        libsubdir    :: dir,
        dynlibdir    :: dir,
        libexecdir   :: dir,
        progdir      :: dir,
        includedir   :: dir,
        datadir      :: dir,
        datasubdir   :: dir,
        docdir       :: dir,
        mandir       :: dir,
        htmldir      :: dir,
        haddockdir   :: dir
    } deriving (Read, Show)

instance Functor InstallDirs where
  fmap f dirs = InstallDirs {
    prefix       = f (prefix dirs),
    bindir       = f (bindir dirs),
    libdir       = f (libdir dirs),
    libsubdir    = f (libsubdir dirs),
    dynlibdir    = f (dynlibdir dirs),
    libexecdir   = f (libexecdir dirs),
    progdir      = f (progdir dirs),
    includedir   = f (includedir dirs),
    datadir      = f (datadir dirs),
    datasubdir   = f (datasubdir dirs),
    docdir       = f (docdir dirs),
    mandir       = f (mandir dirs),
    htmldir      = f (htmldir dirs),
    haddockdir   = f (haddockdir dirs)
  }

instance Monoid dir => Monoid (InstallDirs dir) where
  mempty = InstallDirs {
      prefix       = mempty,
      bindir       = mempty,
      libdir       = mempty,
      libsubdir    = mempty,
      dynlibdir    = mempty,
      libexecdir   = mempty,
      progdir      = mempty,
      includedir   = mempty,
      datadir      = mempty,
      datasubdir   = mempty,
      docdir       = mempty,
      mandir       = mempty,
      htmldir      = mempty,
      haddockdir   = mempty
  }
  mappend = combineInstallDirs mappend

combineInstallDirs :: (a -> b -> c)
                   -> InstallDirs a
                   -> InstallDirs b
                   -> InstallDirs c
combineInstallDirs combine a b = InstallDirs {
    prefix       = prefix a     `combine` prefix b,
    bindir       = bindir a     `combine` bindir b,
    libdir       = libdir a     `combine` libdir b,
    libsubdir    = libsubdir a  `combine` libsubdir b,
    dynlibdir    = dynlibdir a  `combine` dynlibdir b,
    libexecdir   = libexecdir a `combine` libexecdir b,
    progdir      = progdir a    `combine` progdir b,
    includedir   = includedir a `combine` includedir b,
    datadir      = datadir a    `combine` datadir b,
    datasubdir   = datasubdir a `combine` datasubdir b,
    docdir       = docdir a     `combine` docdir b,
    mandir       = mandir a     `combine` mandir b,
    htmldir      = htmldir a    `combine` htmldir b,
    haddockdir   = haddockdir a `combine` haddockdir b
  }

appendSubdirs :: (a -> a -> a) -> InstallDirs a -> InstallDirs a
appendSubdirs append dirs = dirs {
    libdir     = libdir dirs `append` libsubdir dirs,
    datadir    = datadir dirs `append` datasubdir dirs,
    libsubdir  = error "internal error InstallDirs.libsubdir",
    datasubdir = error "internal error InstallDirs.datasubdir"
  }

-- | The installation directories in terms of 'PathTemplate's that contain
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
-- together with @\/@. The reason for this is compatibility with other unix
-- build systems which also support @--libdir@ and @--datadir@. We would like
-- users to be able to configure @--libdir=\/usr\/lib64@ for example but
-- because by default we want to support installing multiple versions of
-- packages and building the same package for multiple compilers we append the
-- libsubdir to get: @\/usr\/lib64\/$pkgid\/$compiler@.
--
-- An additional complication is the need to support relocatable packages on
-- systems which support such things, like Windows.
--
type InstallDirTemplates = InstallDirs PathTemplate

-- ---------------------------------------------------------------------------
-- Default installation directories

defaultInstallDirs :: CompilerFlavor -> Bool -> Bool -> IO InstallDirTemplates
defaultInstallDirs comp userInstall _hasLibs = do
  installPrefix <-
      if userInstall
      then getAppUserDataDirectory "cabal"
      else case buildOS of
           Windows -> do windowsProgramFilesDir <- getWindowsProgramFilesDir
                         return (windowsProgramFilesDir </> "Haskell")
           _       -> return "/usr/local"
  installLibDir <-
      case buildOS of
      Windows -> return "$prefix"
      _       -> case comp of
                 LHC | userInstall -> getAppUserDataDirectory "lhc"
                 _                 -> return ("$prefix" </> "lib")
  return $ fmap toPathTemplate $ InstallDirs {
      prefix       = installPrefix,
      bindir       = "$prefix" </> "bin",
      libdir       = installLibDir,
      libsubdir    = case comp of
           Hugs   -> "hugs" </> "packages" </> "$pkg"
           JHC    -> "$compiler"
           LHC    -> "$compiler"
           UHC    -> "$pkgid"
           _other -> "$pkgid" </> "$compiler",
      dynlibdir    = "$libdir",
      libexecdir   = case buildOS of
        Windows   -> "$prefix" </> "$pkgid"
        _other    -> "$prefix" </> "libexec",
      progdir      = "$libdir" </> "hugs" </> "programs",
      includedir   = "$libdir" </> "$libsubdir" </> "include",
      datadir      = case buildOS of
        Windows   -> "$prefix"
        _other    -> "$prefix" </> "share",
      datasubdir   = "$pkgid",
      docdir       = "$datadir" </> "doc" </> "$pkgid",
      mandir       = "$datadir" </> "man",
      htmldir      = "$docdir"  </> "html",
      haddockdir   = "$htmldir"
  }

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
-- can replace 'prefix' with the 'PrefixVar' and get resulting
-- 'PathTemplate's that still have the 'PrefixVar' in them. Doing this makes it
-- each to check which paths are relative to the $prefix.
--
substituteInstallDirTemplates :: PathTemplateEnv
                              -> InstallDirTemplates -> InstallDirTemplates
substituteInstallDirTemplates env dirs = dirs'
  where
    dirs' = InstallDirs {
      -- So this specifies exactly which vars are allowed in each template
      prefix     = subst prefix     [],
      bindir     = subst bindir     [prefixVar],
      libdir     = subst libdir     [prefixVar, bindirVar],
      libsubdir  = subst libsubdir  [],
      dynlibdir  = subst dynlibdir  [prefixVar, bindirVar, libdirVar],
      libexecdir = subst libexecdir prefixBinLibVars,
      progdir    = subst progdir    prefixBinLibVars,
      includedir = subst includedir prefixBinLibVars,
      datadir    = subst datadir    prefixBinLibVars,
      datasubdir = subst datasubdir [],
      docdir     = subst docdir     prefixBinLibDataVars,
      mandir     = subst mandir     (prefixBinLibDataVars ++ [docdirVar]),
      htmldir    = subst htmldir    (prefixBinLibDataVars ++ [docdirVar]),
      haddockdir = subst haddockdir (prefixBinLibDataVars ++
                                      [docdirVar, htmldirVar])
    }
    subst dir env' = substPathTemplate (env'++env) (dir dirs)

    prefixVar        = (PrefixVar,     prefix     dirs')
    bindirVar        = (BindirVar,     bindir     dirs')
    libdirVar        = (LibdirVar,     libdir     dirs')
    libsubdirVar     = (LibsubdirVar,  libsubdir  dirs')
    datadirVar       = (DatadirVar,    datadir    dirs')
    datasubdirVar    = (DatasubdirVar, datasubdir dirs')
    docdirVar        = (DocdirVar,     docdir     dirs')
    htmldirVar       = (HtmldirVar,    htmldir    dirs')
    prefixBinLibVars = [prefixVar, bindirVar, libdirVar, libsubdirVar]
    prefixBinLibDataVars = prefixBinLibVars ++ [datadirVar, datasubdirVar]

-- | Convert from abstract install directories to actual absolute ones by
-- substituting for all the variables in the abstract paths, to get real
-- absolute path.
absoluteInstallDirs :: PackageIdentifier -> CompilerId -> CopyDest
                    -> InstallDirs PathTemplate
                    -> InstallDirs FilePath
absoluteInstallDirs pkgId compilerId copydest dirs =
    (case copydest of
       CopyTo destdir -> fmap ((destdir </>) . dropDrive)
       _              -> id)
  . appendSubdirs (</>)
  . fmap fromPathTemplate
  $ substituteInstallDirTemplates env dirs
  where
    env = initialPathTemplateEnv pkgId compilerId


-- |The location prefix for the /copy/ command.
data CopyDest
  = NoCopyDest
  | CopyTo FilePath
  deriving (Eq, Show)

-- | Check which of the paths are relative to the installation $prefix.
--
-- If any of the paths are not relative, ie they are absolute paths, then it
-- prevents us from making a relocatable package (also known as a \"prefix
-- independent\" package).
--
prefixRelativeInstallDirs :: PackageIdentifier -> CompilerId
                          -> InstallDirTemplates
                          -> InstallDirs (Maybe FilePath)
prefixRelativeInstallDirs pkgId compilerId dirs =
    fmap relative
  . appendSubdirs combinePathTemplate
  $ -- substitute the path template into each other, except that we map
    -- \$prefix back to $prefix. We're trying to end up with templates that
    -- mention no vars except $prefix.
    substituteInstallDirTemplates env dirs {
      prefix = PathTemplate [Variable PrefixVar]
    }
  where
    env = initialPathTemplateEnv pkgId compilerId

    -- If it starts with $prefix then it's relative and produce the relative
    -- path by stripping off $prefix/ or $prefix
    relative dir = case dir of
      PathTemplate cs -> fmap (fromPathTemplate . PathTemplate) (relative' cs)
    relative' (Variable PrefixVar : Ordinary (s:rest) : rest')
                      | isPathSeparator s = Just (Ordinary rest : rest')
    relative' (Variable PrefixVar : rest) = Just rest
    relative' _                           = Nothing

-- ---------------------------------------------------------------------------
-- Path templates

-- | An abstract path, posibly containing variables that need to be
-- substituted for to get a real 'FilePath'.
--
newtype PathTemplate = PathTemplate [PathComponent]

data PathComponent =
       Ordinary FilePath
     | Variable PathTemplateVariable
     deriving Eq

data PathTemplateVariable =
       PrefixVar     -- ^ The @$prefix@ path variable
     | BindirVar     -- ^ The @$bindir@ path variable
     | LibdirVar     -- ^ The @$libdir@ path variable
     | LibsubdirVar  -- ^ The @$libsubdir@ path variable
     | DatadirVar    -- ^ The @$datadir@ path variable
     | DatasubdirVar -- ^ The @$datasubdir@ path variable
     | DocdirVar     -- ^ The @$docdir@ path variable
     | HtmldirVar    -- ^ The @$htmldir@ path variable
     | PkgNameVar    -- ^ The @$pkg@ package name path variable
     | PkgVerVar     -- ^ The @$version@ package version path variable
     | PkgIdVar      -- ^ The @$pkgid@ package Id path variable, eg @foo-1.0@
     | CompilerVar   -- ^ The compiler name and version, eg @ghc-6.6.1@
     | OSVar         -- ^ The operating system name, eg @windows@ or @linux@
     | ArchVar       -- ^ The cpu architecture name, eg @i386@ or @x86_64@
     | ExecutableNameVar -- ^ The executable name; used in shell wrappers
     | TestSuiteNameVar   -- ^ The name of the test suite being run
     | TestSuiteResultVar -- ^ The result of the test suite being run, eg @pass@, @fail@, or @error@.
  deriving Eq

type PathTemplateEnv = [(PathTemplateVariable, PathTemplate)]

-- | Convert a 'FilePath' to a 'PathTemplate' including any template vars.
--
toPathTemplate :: FilePath -> PathTemplate
toPathTemplate = PathTemplate . read

-- | Convert back to a path, any remaining vars are included
--
fromPathTemplate :: PathTemplate -> FilePath
fromPathTemplate (PathTemplate template) = show template

combinePathTemplate :: PathTemplate -> PathTemplate -> PathTemplate
combinePathTemplate (PathTemplate t1) (PathTemplate t2) =
  PathTemplate (t1 ++ [Ordinary [pathSeparator]] ++ t2)

substPathTemplate :: PathTemplateEnv -> PathTemplate -> PathTemplate
substPathTemplate environment (PathTemplate template) =
    PathTemplate (concatMap subst template)

    where subst component@(Ordinary _) = [component]
          subst component@(Variable variable) =
              case lookup variable environment of
                  Just (PathTemplate components) -> components
                  Nothing                        -> [component]

-- | The initial environment has all the static stuff but no paths
initialPathTemplateEnv :: PackageIdentifier -> CompilerId -> PathTemplateEnv
initialPathTemplateEnv pkgId compilerId =
     packageTemplateEnv  pkgId
  ++ compilerTemplateEnv compilerId
  ++ platformTemplateEnv buildPlatform -- platform should be param if we want
                                       -- to do cross-platform configuation

packageTemplateEnv :: PackageIdentifier -> PathTemplateEnv
packageTemplateEnv pkgId =
  [(PkgNameVar,  PathTemplate [Ordinary $ display (packageName pkgId)])
  ,(PkgVerVar,   PathTemplate [Ordinary $ display (packageVersion pkgId)])
  ,(PkgIdVar,    PathTemplate [Ordinary $ display pkgId])
  ]

compilerTemplateEnv :: CompilerId -> PathTemplateEnv
compilerTemplateEnv compilerId =
  [(CompilerVar, PathTemplate [Ordinary $ display compilerId])
  ]

platformTemplateEnv :: Platform -> PathTemplateEnv
platformTemplateEnv (Platform arch os) =
  [(OSVar,       PathTemplate [Ordinary $ display os])
  ,(ArchVar,     PathTemplate [Ordinary $ display arch])
  ]

installDirsTemplateEnv :: InstallDirs PathTemplate -> PathTemplateEnv
installDirsTemplateEnv dirs =
  [(PrefixVar,     prefix     dirs)
  ,(BindirVar,     bindir     dirs)
  ,(LibdirVar,     libdir     dirs)
  ,(LibsubdirVar,  libsubdir  dirs)
  ,(DatadirVar,    datadir    dirs)
  ,(DatasubdirVar, datasubdir dirs)
  ,(DocdirVar,     docdir     dirs)
  ,(HtmldirVar,    htmldir    dirs)
  ]


-- ---------------------------------------------------------------------------
-- Parsing and showing path templates:

-- The textual format is that of an ordinary Haskell String, eg
-- "$prefix/bin"
-- and this gets parsed to the internal representation as a sequence of path
-- spans which are either strings or variables, eg:
-- PathTemplate [Variable PrefixVar, Ordinary "/bin" ]

instance Show PathTemplateVariable where
  show PrefixVar     = "prefix"
  show BindirVar     = "bindir"
  show LibdirVar     = "libdir"
  show LibsubdirVar  = "libsubdir"
  show DatadirVar    = "datadir"
  show DatasubdirVar = "datasubdir"
  show DocdirVar     = "docdir"
  show HtmldirVar    = "htmldir"
  show PkgNameVar    = "pkg"
  show PkgVerVar     = "version"
  show PkgIdVar      = "pkgid"
  show CompilerVar   = "compiler"
  show OSVar         = "os"
  show ArchVar       = "arch"
  show ExecutableNameVar = "executablename"
  show TestSuiteNameVar   = "test-suite"
  show TestSuiteResultVar = "result"

instance Read PathTemplateVariable where
  readsPrec _ s =
    take 1
    [ (var, drop (length varStr) s)
    | (varStr, var) <- vars
    , varStr `isPrefixOf` s ]
    where vars = [("prefix",     PrefixVar)
                 ,("bindir",     BindirVar)
                 ,("libdir",     LibdirVar)
                 ,("libsubdir",  LibsubdirVar)
                 ,("datadir",    DatadirVar)
                 ,("datasubdir", DatasubdirVar)
                 ,("docdir",     DocdirVar)
                 ,("htmldir",    HtmldirVar)
                 ,("pkgid",      PkgIdVar)
                 ,("pkg",        PkgNameVar)
                 ,("version",    PkgVerVar)
                 ,("compiler",   CompilerVar)
                 ,("os",         OSVar)
                 ,("arch",       ArchVar)
                 ,("executablename", ExecutableNameVar)
                 ,("test-suite", TestSuiteNameVar)
                 ,("result", TestSuiteResultVar)]

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
  allocaArray long_path_size $ \pPath -> do
     r <- c_SHGetFolderPath nullPtr n nullPtr 0 pPath
     if (r /= 0)
        then return Nothing
        else do s <- peekCWString pPath; return (Just s)
  where
    long_path_size      = 1024 -- MAX_PATH is 260, this should be plenty
# endif

csidl_PROGRAM_FILES :: CInt
csidl_PROGRAM_FILES = 0x0026
-- csidl_PROGRAM_FILES_COMMON :: CInt
-- csidl_PROGRAM_FILES_COMMON = 0x002b

foreign import stdcall unsafe "shlobj.h SHGetFolderPathW"
            c_SHGetFolderPath :: Ptr ()
                              -> CInt
                              -> Ptr ()
                              -> CInt
                              -> CWString
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
isWindows = case buildOS of
  Windows -> True
  _       -> False
#endif
