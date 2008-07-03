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
        -- * Installation directories
        module Distribution.Simple.InstallDirs,
        absoluteInstallDirs, prefixRelativeInstallDirs,
        substPathTemplate
  ) where


import Distribution.Simple.InstallDirs hiding (absoluteInstallDirs,
                                               prefixRelativeInstallDirs,
                                               substPathTemplate, )
import qualified Distribution.Simple.InstallDirs as InstallDirs
import Distribution.Simple.Setup (CopyDest(..))
import Distribution.Simple.Program (ProgramConfiguration)
import Distribution.PackageDescription (PackageDescription(..))
import Distribution.Package (PackageIdentifier, Package(..))
import Distribution.Simple.Compiler
         ( Compiler(..), PackageDB, OptimisationLevel )
import Distribution.Simple.PackageIndex (PackageIndex)
import Distribution.InstalledPackageInfo (InstalledPackageInfo)

-- |Data cached after configuration step.  See also
-- 'Distribution.Setup.ConfigFlags'.
data LocalBuildInfo = LocalBuildInfo {
        installDirTemplates :: InstallDirTemplates,
                -- ^ The installation directories for the various differnt
                -- kinds of files
        compiler      :: Compiler,
                -- ^ The compiler we're building with
        buildDir      :: FilePath,
                -- ^ Where to build the package.
        scratchDir    :: FilePath,
                -- ^ Where to put the result of the Hugs build.
        packageDeps   :: [PackageIdentifier],
                -- ^ Which packages we depend on, /exactly/.
                -- The 'Distribution.PackageDescription.PackageDescription'
                -- specifies a set of build dependencies
                -- that must be satisfied in terms of version ranges.  This
                -- field fixes those dependencies to the specific versions
                -- available on this machine for this compiler.
        installedPkgs :: PackageIndex InstalledPackageInfo,
                -- ^ All the info about all installed packages.
        pkgDescrFile  :: Maybe FilePath,
                -- ^ the filename containing the .cabal file, if available
        localPkgDescr :: PackageDescription,
                -- ^ The resolved package description, that does not contain
                -- any conditionals.
        withPrograms  :: ProgramConfiguration, -- ^Location and args for all programs
        withPackageDB :: PackageDB,  -- ^What package database to use, global\/user
        withVanillaLib:: Bool,  -- ^Whether to build normal libs.
        withProfLib   :: Bool,  -- ^Whether to build profiling versions of libs.
        withSharedLib :: Bool,  -- ^Whether to build shared versions of libs.
        withProfExe   :: Bool,  -- ^Whether to build executables for profiling.
        withOptimization :: OptimisationLevel, -- ^Whether to build with optimization (if available).
        withGHCiLib   :: Bool,  -- ^Whether to build libs suitable for use with GHCi.
        splitObjs     :: Bool,  -- ^Use -split-objs with GHC, if available
        stripExes     :: Bool,  -- ^Whether to strip executables during install
        progPrefix    :: PathTemplate, -- ^Prefix to be prepended to installed executables
        progSuffix    :: PathTemplate -- ^Suffix to be appended to installed executables

  } deriving (Read, Show)

-- -----------------------------------------------------------------------------
-- Wrappers for a couple functions from InstallDirs

-- |See 'InstallDirs.absoluteInstallDirs'
absoluteInstallDirs :: PackageDescription -> LocalBuildInfo -> CopyDest
                    -> InstallDirs FilePath
absoluteInstallDirs pkg_descr lbi copydest =
  InstallDirs.absoluteInstallDirs
    (packageId pkg_descr)
    (compilerId (compiler lbi))
    copydest
    (installDirTemplates lbi)

-- |See 'InstallDirs.prefixRelativeInstallDirs'
prefixRelativeInstallDirs :: PackageDescription -> LocalBuildInfo
                          -> InstallDirs (Maybe FilePath)
prefixRelativeInstallDirs pkg_descr lbi =
  InstallDirs.prefixRelativeInstallDirs
    (packageId pkg_descr)
    (compilerId (compiler lbi))
    (installDirTemplates lbi)

substPathTemplate :: PackageDescription -> LocalBuildInfo
                  -> PathTemplate -> FilePath
substPathTemplate pkg_descr lbi = fromPathTemplate
                                . ( InstallDirs.substPathTemplate env )
    where env = initialPathTemplateEnv
                   (packageId pkg_descr)
                   (compilerId (compiler lbi))
