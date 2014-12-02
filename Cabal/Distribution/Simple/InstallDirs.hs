{-# LANGUAGE CPP, ForeignFunctionInterface #-}
{-# LANGUAGE DeriveGeneric #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.InstallDirs
-- Copyright   :  Isaac Jones 2003-2004
-- License     :  BSD3
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
        PathTemplateEnv,
        toPathTemplate,
        fromPathTemplate,
        substPathTemplate,
        initialPathTemplateEnv,
        platformTemplateEnv,
        compilerTemplateEnv,
        packageTemplateEnv,
        abiTemplateEnv,
        installDirsTemplateEnv,
  ) where


import Data.Binary (Binary)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid(..))
import GHC.Generics (Generic)
import System.Directory (getAppUserDataDirectory)
import System.FilePath ((</>), isPathSeparator, pathSeparator)
import System.FilePath (dropDrive)

import Distribution.Package
         ( PackageIdentifier, PackageKey, packageName, packageVersion )
import Distribution.System
         ( OS(..), buildOS, Platform(..) )
import Distribution.Compiler
         ( AbiTag(..), abiTagString, CompilerInfo(..), CompilerFlavor(..) )
import Distribution.Text
         ( display )

#if mingw32_HOST_OS
import Foreign
import Foreign.C
#endif

-- ---------------------------------------------------------------------------
-- Installation directories


-- | The directories where we will install files for packages.
--
-- We have several different directories for different types of files since
-- many systems have conventions whereby different types of files in a package
-- are installed in different directories. This is particularly the case on
-- Unix style systems.
--
data InstallDirs dir = InstallDirs {
        prefix       :: dir,
        bindir       :: dir,
        libdir       :: dir,
        libsubdir    :: dir,
        dynlibdir    :: dir,
        libexecdir   :: dir,
        includedir   :: dir,
        datadir      :: dir,
        datasubdir   :: dir,
        docdir       :: dir,
        mandir       :: dir,
        htmldir      :: dir,
        haddockdir   :: dir,
        sysconfdir   :: dir
    } deriving (Generic, Read, Show)

instance Binary dir => Binary (InstallDirs dir)

instance Functor InstallDirs where
  fmap f dirs = InstallDirs {
    prefix       = f (prefix dirs),
    bindir       = f (bindir dirs),
    libdir       = f (libdir dirs),
    libsubdir    = f (libsubdir dirs),
    dynlibdir    = f (dynlibdir dirs),
    libexecdir   = f (libexecdir dirs),
    includedir   = f (includedir dirs),
    datadir      = f (datadir dirs),
    datasubdir   = f (datasubdir dirs),
    docdir       = f (docdir dirs),
    mandir       = f (mandir dirs),
    htmldir      = f (htmldir dirs),
    haddockdir   = f (haddockdir dirs),
    sysconfdir   = f (sysconfdir dirs)
  }

instance Monoid dir => Monoid (InstallDirs dir) where
  mempty = InstallDirs {
      prefix       = mempty,
      bindir       = mempty,
      libdir       = mempty,
      libsubdir    = mempty,
      dynlibdir    = mempty,
      libexecdir   = mempty,
      includedir   = mempty,
      datadir      = mempty,
      datasubdir   = mempty,
      docdir       = mempty,
      mandir       = mempty,
      htmldir      = mempty,
      haddockdir   = mempty,
      sysconfdir   = mempty
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
    includedir   = includedir a `combine` includedir b,
    datadir      = datadir a    `combine` datadir b,
    datasubdir   = datasubdir a `combine` datasubdir b,
    docdir       = docdir a     `combine` docdir b,
    mandir       = mandir a     `combine` mandir b,
    htmldir      = htmldir a    `combine` htmldir b,
    haddockdir   = haddockdir a `combine` haddockdir b,
    sysconfdir   = sysconfdir a `combine` sysconfdir b
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
-- These are expanded by textual substitution (see 'substPathTemplate').
--
-- A few of these installation directories are split into two components, the
-- dir and subdir. The full installation path is formed by combining the two
-- together with @\/@. The reason for this is compatibility with other Unix
-- build systems which also support @--libdir@ and @--datadir@. We would like
-- users to be able to configure @--libdir=\/usr\/lib64@ for example but
-- because by default we want to support installing multiple versions of
-- packages and building the same package for multiple compilers we append the
-- libsubdir to get: @\/usr\/lib64\/$pkgkey\/$compiler@.
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
           JHC    -> "$compiler"
           LHC    -> "$compiler"
           UHC    -> "$pkgid"
           _other -> "$abi" </> "$pkgkey",
      dynlibdir    = "$libdir",
      libexecdir   = case buildOS of
        Windows   -> "$prefix" </> "$pkgkey"
        _other    -> "$prefix" </> "libexec",
      includedir   = "$libdir" </> "$libsubdir" </> "include",
      datadir      = case buildOS of
        Windows   -> "$prefix"
        _other    -> "$prefix" </> "share",
      datasubdir   = "$abi" </> "$pkgid",
      docdir       = "$datadir" </> "doc" </> "$abi" </> "$pkgid",
      mandir       = "$datadir" </> "man",
      htmldir      = "$docdir"  </> "html",
      haddockdir   = "$htmldir",
      sysconfdir   = "$prefix" </> "etc"
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
      includedir = subst includedir prefixBinLibVars,
      datadir    = subst datadir    prefixBinLibVars,
      datasubdir = subst datasubdir [],
      docdir     = subst docdir     prefixBinLibDataVars,
      mandir     = subst mandir     (prefixBinLibDataVars ++ [docdirVar]),
      htmldir    = subst htmldir    (prefixBinLibDataVars ++ [docdirVar]),
      haddockdir = subst haddockdir (prefixBinLibDataVars ++
                                      [docdirVar, htmldirVar]),
      sysconfdir = subst sysconfdir prefixBinLibVars
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
absoluteInstallDirs :: PackageIdentifier
                    -> PackageKey
                    -> CompilerInfo
                    -> CopyDest
                    -> Platform
                    -> InstallDirs PathTemplate
                    -> InstallDirs FilePath
absoluteInstallDirs pkgId pkg_key compilerId copydest platform dirs =
    (case copydest of
       CopyTo destdir -> fmap ((destdir </>) . dropDrive)
       _              -> id)
  . appendSubdirs (</>)
  . fmap fromPathTemplate
  $ substituteInstallDirTemplates env dirs
  where
    env = initialPathTemplateEnv pkgId pkg_key compilerId platform


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
prefixRelativeInstallDirs :: PackageIdentifier
                          -> PackageKey
                          -> CompilerInfo
                          -> Platform
                          -> InstallDirTemplates
                          -> InstallDirs (Maybe FilePath)
prefixRelativeInstallDirs pkgId pkg_key compilerId platform dirs =
    fmap relative
  . appendSubdirs combinePathTemplate
  $ -- substitute the path template into each other, except that we map
    -- \$prefix back to $prefix. We're trying to end up with templates that
    -- mention no vars except $prefix.
    substituteInstallDirTemplates env dirs {
      prefix = PathTemplate [Variable PrefixVar]
    }
  where
    env = initialPathTemplateEnv pkgId pkg_key compilerId platform

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

-- | An abstract path, possibly containing variables that need to be
-- substituted for to get a real 'FilePath'.
--
newtype PathTemplate = PathTemplate [PathComponent] deriving (Eq, Generic, Ord)

instance Binary PathTemplate

data PathComponent =
       Ordinary FilePath
     | Variable PathTemplateVariable
     deriving (Eq, Ord, Generic)

instance Binary PathComponent

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
     | PkgKeyVar     -- ^ The @$pkgkey@ package key path variable
     | CompilerVar   -- ^ The compiler name and version, eg @ghc-6.6.1@
     | OSVar         -- ^ The operating system name, eg @windows@ or @linux@
     | ArchVar       -- ^ The CPU architecture name, eg @i386@ or @x86_64@
     | AbiVar        -- ^ The Compiler's ABI identifier, $arch-$os-$compiler-$abitag
     | AbiTagVar     -- ^ The optional ABI tag for the compiler
     | ExecutableNameVar -- ^ The executable name; used in shell wrappers
     | TestSuiteNameVar   -- ^ The name of the test suite being run
     | TestSuiteResultVar -- ^ The result of the test suite being run, eg
                          -- @pass@, @fail@, or @error@.
     | BenchmarkNameVar   -- ^ The name of the benchmark being run
  deriving (Eq, Ord, Generic)

instance Binary PathTemplateVariable

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
initialPathTemplateEnv :: PackageIdentifier
                       -> PackageKey
                       -> CompilerInfo
                       -> Platform
                       -> PathTemplateEnv
initialPathTemplateEnv pkgId pkg_key compiler platform =
     packageTemplateEnv  pkgId pkg_key
  ++ compilerTemplateEnv compiler
  ++ platformTemplateEnv platform
  ++ abiTemplateEnv compiler platform

packageTemplateEnv :: PackageIdentifier -> PackageKey -> PathTemplateEnv
packageTemplateEnv pkgId pkg_key =
  [(PkgNameVar,  PathTemplate [Ordinary $ display (packageName pkgId)])
  ,(PkgVerVar,   PathTemplate [Ordinary $ display (packageVersion pkgId)])
  ,(PkgKeyVar,   PathTemplate [Ordinary $ display pkg_key])
  ,(PkgIdVar,    PathTemplate [Ordinary $ display pkgId])
  ]

compilerTemplateEnv :: CompilerInfo -> PathTemplateEnv
compilerTemplateEnv compiler =
  [(CompilerVar, PathTemplate [Ordinary $ display (compilerInfoId compiler)])
  ]

platformTemplateEnv :: Platform -> PathTemplateEnv
platformTemplateEnv (Platform arch os) =
  [(OSVar,       PathTemplate [Ordinary $ display os])
  ,(ArchVar,     PathTemplate [Ordinary $ display arch])
  ]

abiTemplateEnv :: CompilerInfo -> Platform -> PathTemplateEnv
abiTemplateEnv compiler (Platform arch os) =
  [(AbiVar,      PathTemplate [Ordinary $ display arch ++ '-':display os ++
                                          '-':display (compilerInfoId compiler) ++
                                          case compilerInfoAbiTag compiler of
                                            NoAbiTag   -> ""
                                            AbiTag tag -> '-':tag])
  ,(AbiTagVar,   PathTemplate [Ordinary $ abiTagString (compilerInfoAbiTag compiler)])
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
  show PkgKeyVar     = "pkgkey"
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
  show AbiTagVar     = "abitag"
  show AbiVar        = "abi"
  show ExecutableNameVar = "executablename"
  show TestSuiteNameVar   = "test-suite"
  show TestSuiteResultVar = "result"
  show BenchmarkNameVar   = "benchmark"

instance Read PathTemplateVariable where
  readsPrec _ s =
    take 1
    [ (var, drop (length varStr) s)
    | (varStr, var) <- vars
    , varStr `isPrefixOf` s ]
    -- NB: order matters! Longer strings first
    where vars = [("prefix",     PrefixVar)
                 ,("bindir",     BindirVar)
                 ,("libdir",     LibdirVar)
                 ,("libsubdir",  LibsubdirVar)
                 ,("datadir",    DatadirVar)
                 ,("datasubdir", DatasubdirVar)
                 ,("docdir",     DocdirVar)
                 ,("htmldir",    HtmldirVar)
                 ,("pkgid",      PkgIdVar)
                 ,("pkgkey",     PkgKeyVar)
                 ,("pkg",        PkgNameVar)
                 ,("version",    PkgVerVar)
                 ,("compiler",   CompilerVar)
                 ,("os",         OSVar)
                 ,("arch",       ArchVar)
                 ,("abitag",     AbiTagVar)
                 ,("abi",        AbiVar)
                 ,("executablename", ExecutableNameVar)
                 ,("test-suite", TestSuiteNameVar)
                 ,("result", TestSuiteResultVar)
                 ,("benchmark", BenchmarkNameVar)]

instance Show PathComponent where
  show (Ordinary path) = path
  show (Variable var)  = '$':show var
  showList = foldr (\x -> (shows x .)) id

instance Read PathComponent where
  -- for some reason we collapse multiple $ symbols here
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
#if mingw32_HOST_OS
  m <- shGetFolderPath csidl_PROGRAM_FILES
#else
  let m = Nothing
#endif
  return (fromMaybe "C:\\Program Files" m)

#if mingw32_HOST_OS
shGetFolderPath :: CInt -> IO (Maybe FilePath)
shGetFolderPath n =
  allocaArray long_path_size $ \pPath -> do
     r <- c_SHGetFolderPath nullPtr n nullPtr 0 pPath
     if (r /= 0)
        then return Nothing
        else do s <- peekCWString pPath; return (Just s)
  where
    long_path_size      = 1024 -- MAX_PATH is 260, this should be plenty

csidl_PROGRAM_FILES :: CInt
csidl_PROGRAM_FILES = 0x0026
-- csidl_PROGRAM_FILES_COMMON :: CInt
-- csidl_PROGRAM_FILES_COMMON = 0x002b

#ifdef x86_64_HOST_ARCH
#define CALLCONV ccall
#else
#define CALLCONV stdcall
#endif

foreign import CALLCONV unsafe "shlobj.h SHGetFolderPathW"
            c_SHGetFolderPath :: Ptr ()
                              -> CInt
                              -> Ptr ()
                              -> CInt
                              -> CWString
                              -> IO CInt
#endif
