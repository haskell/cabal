-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.LocalBuildInfo
-- Copyright   :  Isaac Jones 2003-2004
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Once a package has been configured we have resolved conditionals and
-- dependencies, configured the compiler and other needed external programs.
-- The 'LocalBuildInfo' is used to hold all this information. It holds the
-- install dirs, the compiler, the exact package dependencies, the configured
-- programs, the package database to use and a bunch of miscellaneous configure
-- flags. It gets saved and reloaded from a file (@dist\/setup-config@). It gets
-- passed in to very many subsequent build actions.

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

module Distribution.Simple.LocalBuildInfo (
        LocalBuildInfo(..),
        externalPackageDeps,
        inplacePackageId,
        withLibLBI,
        withExeLBI,
        withComponentsLBI,
        withTestLBI,
        ComponentLocalBuildInfo(..),
        -- * Installation directories
        module Distribution.Simple.InstallDirs,
        absoluteInstallDirs, prefixRelativeInstallDirs,
        substPathTemplate
  ) where


import Distribution.Simple.InstallDirs hiding (absoluteInstallDirs,
                                               prefixRelativeInstallDirs,
                                               substPathTemplate, )
import qualified Distribution.Simple.InstallDirs as InstallDirs
import Distribution.Simple.Program (ProgramConfiguration)
import Distribution.PackageDescription
         ( PackageDescription(..), withLib, Library, withExe
         , Executable(exeName), withTest, TestSuite(..)
         , Component(..) )
import Distribution.Package
         ( PackageId, Package(..), InstalledPackageId(..) )
import Distribution.Simple.Compiler
         ( Compiler(..), PackageDBStack, OptimisationLevel )
import Distribution.Simple.PackageIndex
         ( PackageIndex )
import Distribution.Simple.Utils
         ( die )
import Distribution.Simple.Setup
         ( ConfigFlags )
import Distribution.Text
         ( display )

import Data.List (nub)

-- | Data cached after configuration step.  See also
-- 'Distribution.Simple.Setup.ConfigFlags'.
data LocalBuildInfo = LocalBuildInfo {
        configFlags   :: ConfigFlags,
        -- ^ Options passed to the configuration step.
        -- Needed to re-run configuration when .cabal is out of date
        extraConfigArgs     :: [String],
        -- ^ Extra args on the command line for the configuration step.
        -- Needed to re-run configuration when .cabal is out of date
        installDirTemplates :: InstallDirTemplates,
                -- ^ The installation directories for the various differnt
                -- kinds of files
        --TODO: inplaceDirTemplates :: InstallDirs FilePath
        compiler      :: Compiler,
                -- ^ The compiler we're building with
        buildDir      :: FilePath,
                -- ^ Where to build the package.
        --TODO: eliminate hugs's scratchDir, use builddir
        scratchDir    :: FilePath,
                -- ^ Where to put the result of the Hugs build.
        libraryConfig       :: Maybe ComponentLocalBuildInfo,
        executableConfigs   :: [(String, ComponentLocalBuildInfo)],
        compBuildOrder :: [Component],
                -- ^ All the components to build, ordered by topological sort
                -- over the intrapackage dependency graph
        testSuiteConfigs    :: [(String, ComponentLocalBuildInfo)],
        installedPkgs :: PackageIndex,
                -- ^ All the info about the installed packages that the
                -- current package depends on (directly or indirectly).
        pkgDescrFile  :: Maybe FilePath,
                -- ^ the filename containing the .cabal file, if available
        localPkgDescr :: PackageDescription,
                -- ^ The resolved package description, that does not contain
                -- any conditionals.
        withPrograms  :: ProgramConfiguration, -- ^Location and args for all programs
        withPackageDB :: PackageDBStack,  -- ^What package database to use, global\/user
        withVanillaLib:: Bool,  -- ^Whether to build normal libs.
        withProfLib   :: Bool,  -- ^Whether to build profiling versions of libs.
        withSharedLib :: Bool,  -- ^Whether to build shared versions of libs.
        withDynExe    :: Bool,  -- ^Whether to link executables dynamically
        withProfExe   :: Bool,  -- ^Whether to build executables for profiling.
        withOptimization :: OptimisationLevel, -- ^Whether to build with optimization (if available).
        withGHCiLib   :: Bool,  -- ^Whether to build libs suitable for use with GHCi.
        splitObjs     :: Bool,  -- ^Use -split-objs with GHC, if available
        stripExes     :: Bool,  -- ^Whether to strip executables during install
        progPrefix    :: PathTemplate, -- ^Prefix to be prepended to installed executables
        progSuffix    :: PathTemplate -- ^Suffix to be appended to installed executables
  } deriving (Read, Show)

data ComponentLocalBuildInfo = ComponentLocalBuildInfo {
    -- | Resolved internal and external package dependencies for this component.
    -- The 'BuildInfo' specifies a set of build dependencies that must be
    -- satisfied in terms of version ranges. This field fixes those dependencies
    -- to the specific versions available on this machine for this compiler.
    componentPackageDeps :: [(InstalledPackageId, PackageId)]
  }
  deriving (Read, Show)

-- | External package dependencies for the package as a whole, the union of the
-- individual 'targetPackageDeps'.
externalPackageDeps :: LocalBuildInfo -> [(InstalledPackageId, PackageId)]
externalPackageDeps lbi = nub $
  -- TODO:  what about non-buildable components?
     maybe [] componentPackageDeps (libraryConfig lbi)
  ++ concatMap (componentPackageDeps . snd) (executableConfigs lbi)

-- | The installed package Id we use for local packages registered in the local
-- package db. This is what is used for intra-package deps between components.
--
inplacePackageId :: PackageId -> InstalledPackageId
inplacePackageId pkgid = InstalledPackageId (display pkgid ++ "-inplace")

-- |If the package description has a library section, call the given
--  function with the library build info as argument.  Extended version of
-- 'withLib' that also gives corresponding build info.
withLibLBI :: PackageDescription -> LocalBuildInfo
           -> (Library -> ComponentLocalBuildInfo -> IO ()) -> IO ()
withLibLBI pkg_descr lbi f = withLib pkg_descr $ \lib ->
  case libraryConfig lbi of
    Just clbi -> f lib clbi
    Nothing   -> die $ "internal error: the package contains a library "
                    ++ "but there is no corresponding configuration data"

-- | Perform the action on each buildable 'Executable' in the package
-- description.  Extended version of 'withExe' that also gives corresponding
-- build info.
withExeLBI :: PackageDescription -> LocalBuildInfo
           -> (Executable -> ComponentLocalBuildInfo -> IO ()) -> IO ()
withExeLBI pkg_descr lbi f = withExe pkg_descr $ \exe ->
  case lookup (exeName exe) (executableConfigs lbi) of
    Just clbi -> f exe clbi
    Nothing   -> die $ "internal error: the package contains an executable "
                    ++ exeName exe ++ " but there is no corresponding "
                    ++ "configuration data"

-- | Perform the action on each buildable 'Library' or 'Executable' (Component)
-- in the PackageDescription, subject to the build order specified by the
-- 'compBuildOrder' field of the given 'LocalBuildInfo'
withComponentsLBI :: LocalBuildInfo
                  -> (Component -> ComponentLocalBuildInfo -> IO ())
                  -> IO ()
withComponentsLBI lbi f = mapM_ compF (compBuildOrder lbi)
  where
    compF l@(CLib _lib) =
      case libraryConfig lbi of
        Just clbi -> f l clbi
        Nothing   -> die $ "internal error: the package contains a library "
                        ++ "but there is no corresponding configuration data"
    compF r@(CExe exe) =
      case lookup (exeName exe) (executableConfigs lbi) of
        Just clbi -> f r clbi
        Nothing   -> die $ "internal error: the package contains an executable "
                        ++ exeName exe ++ " but there is no corresponding "
                        ++ "configuration data"
    compF t@(CTst test) =
      case lookup (testName test) (testSuiteConfigs lbi) of
        Just clbi -> f t clbi
        Nothing -> die $ "internal error: the package contains a test suite "
                        ++ testName test ++ " but there is no corresponding "
                        ++ "configuration data"

withTestLBI :: PackageDescription -> LocalBuildInfo
            -> (TestSuite -> ComponentLocalBuildInfo -> IO ()) -> IO ()
withTestLBI pkg_descr lbi f =
    let wrapper test = case lookup (testName test) (testSuiteConfigs lbi) of
            Just clbi -> f test clbi
            Nothing -> die $ "internal error: the package contains a test suite "
                            ++ testName test ++ " but there is no corresponding "
                            ++ "configuration data"
    in withTest pkg_descr wrapper

-- -----------------------------------------------------------------------------
-- Wrappers for a couple functions from InstallDirs

-- |See 'InstallDirs.absoluteInstallDirs'
absoluteInstallDirs :: PackageDescription -> LocalBuildInfo -> CopyDest
                    -> InstallDirs FilePath
absoluteInstallDirs pkg lbi copydest =
  InstallDirs.absoluteInstallDirs
    (packageId pkg)
    (compilerId (compiler lbi))
    copydest
    (installDirTemplates lbi)

-- |See 'InstallDirs.prefixRelativeInstallDirs'
prefixRelativeInstallDirs :: PackageId -> LocalBuildInfo
                          -> InstallDirs (Maybe FilePath)
prefixRelativeInstallDirs pkg_descr lbi =
  InstallDirs.prefixRelativeInstallDirs
    (packageId pkg_descr)
    (compilerId (compiler lbi))
    (installDirTemplates lbi)

substPathTemplate :: PackageId -> LocalBuildInfo
                  -> PathTemplate -> FilePath
substPathTemplate pkgid lbi = fromPathTemplate
                                . ( InstallDirs.substPathTemplate env )
    where env = initialPathTemplateEnv
                   pkgid
                   (compilerId (compiler lbi))
