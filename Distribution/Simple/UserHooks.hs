-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.UserHooks
-- Copyright   :  Isaac Jones 2003-2005
-- 
-- Maintainer  :  Isaac Jones <ijones@syntaxpolice.org>
-- Stability   :  alpha
-- Portability :  portable
--
-- Explanation: Simple build system; basically the interface for
-- Distribution.Simple.\* modules.  When given the parsed command-line
-- args and package information, is able to perform basic commands
-- like configure, build, install, register, etc.
--
-- This module isn't called \"Simple\" because it's simple.  Far from
-- it.  It's called \"Simple\" because it does complicated things to
-- simple software.

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

module Distribution.Simple.UserHooks (
        UserHooks(..), Args,
        emptyUserHooks,
  ) where

import Distribution.PackageDescription
         (PackageDescription, GenericPackageDescription,
          HookedBuildInfo, emptyHookedBuildInfo)
import Distribution.Simple.Program    (Program)
import Distribution.Simple.Command    (noExtraFlags)
import Distribution.Simple.PreProcess (PPSuffixHandler)
import Distribution.Simple.Setup
         (ConfigFlags, BuildFlags, MakefileFlags, CleanFlags, CopyFlags,
          InstallFlags, SDistFlags, RegisterFlags, HscolourFlags,
          HaddockFlags)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo)

type Args = [String]

-- | Hooks allow authors to add specific functionality before and after a
-- command is run, and also to specify additional preprocessors.
--
-- * WARNING: The hooks interface is under rather constant flux as we try to
-- understand users needs. Setup files that depend on this interface may
-- break in future releases.
data UserHooks = UserHooks {

    -- | Used for @.\/setup test@
    runTests :: Args -> Bool -> PackageDescription -> LocalBuildInfo -> IO (),
    -- | Read the description file
    readDesc :: IO (Maybe PackageDescription),
    -- | Custom preprocessors in addition to and overriding 'knownSuffixHandlers'.
    hookedPreProcessors :: [ PPSuffixHandler ],
    -- | These programs are detected at configure time.  Arguments for them are
    -- added to the configure command.
    hookedPrograms :: [Program],

    -- |Hook to run before configure command
    preConf  :: Args -> ConfigFlags -> IO HookedBuildInfo,
    -- |Over-ride this hook to get different behavior during configure.
    confHook :: ( Either GenericPackageDescription PackageDescription
               , HookedBuildInfo)
            -> ConfigFlags -> IO LocalBuildInfo,
    -- |Hook to run after configure command
    postConf :: Args -> ConfigFlags -> PackageDescription -> LocalBuildInfo -> IO (),

    -- |Hook to run before build command.  Second arg indicates verbosity level.
    preBuild  :: Args -> BuildFlags -> IO HookedBuildInfo,

    -- |Over-ride this hook to gbet different behavior during build.
    buildHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO (),
    -- |Hook to run after build command.  Second arg indicates verbosity level.
    postBuild :: Args -> BuildFlags -> PackageDescription -> LocalBuildInfo -> IO (),

    -- |Hook to run before makefile command.  Second arg indicates verbosity level.
    preMakefile  :: Args -> MakefileFlags -> IO HookedBuildInfo,

    -- |Over-ride this hook to get different behavior during makefile.
    makefileHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> MakefileFlags -> IO (),
    -- |Hook to run after makefile command.  Second arg indicates verbosity level.
    postMakefile :: Args -> MakefileFlags -> PackageDescription -> LocalBuildInfo -> IO (),

    -- |Hook to run before clean command.  Second arg indicates verbosity level.
    preClean  :: Args -> CleanFlags -> IO HookedBuildInfo,
    -- |Over-ride this hook to get different behavior during clean.
    cleanHook :: PackageDescription -> Maybe LocalBuildInfo -> UserHooks -> CleanFlags -> IO (),
    -- |Hook to run after clean command.  Second arg indicates verbosity level.
    postClean :: Args -> CleanFlags -> PackageDescription -> Maybe LocalBuildInfo -> IO (),

    -- |Hook to run before copy command
    preCopy  :: Args -> CopyFlags -> IO HookedBuildInfo,
    -- |Over-ride this hook to get different behavior during copy.
    copyHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> CopyFlags -> IO (),
    -- |Hook to run after copy command
    postCopy :: Args -> CopyFlags -> PackageDescription -> LocalBuildInfo -> IO (),

    -- |Hook to run before install command
    preInst  :: Args -> InstallFlags -> IO HookedBuildInfo,

    -- |Over-ride this hook to get different behavior during install.
    instHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> InstallFlags -> IO (),
    -- |Hook to run after install command.  postInst should be run
    -- on the target, not on the build machine.
    postInst :: Args -> InstallFlags -> PackageDescription -> LocalBuildInfo -> IO (),

    -- |Hook to run before sdist command.  Second arg indicates verbosity level.
    preSDist  :: Args -> SDistFlags -> IO HookedBuildInfo,
    -- |Over-ride this hook to get different behavior during sdist.
    sDistHook :: PackageDescription -> Maybe LocalBuildInfo -> UserHooks -> SDistFlags -> IO (),
    -- |Hook to run after sdist command.  Second arg indicates verbosity level.
    postSDist :: Args -> SDistFlags -> PackageDescription -> Maybe LocalBuildInfo -> IO (),

    -- |Hook to run before register command
    preReg  :: Args -> RegisterFlags -> IO HookedBuildInfo,
    -- |Over-ride this hook to get different behavior during registration.
    regHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> RegisterFlags -> IO (),
    -- |Hook to run after register command
    postReg :: Args -> RegisterFlags -> PackageDescription -> LocalBuildInfo -> IO (),

    -- |Hook to run before unregister command
    preUnreg  :: Args -> RegisterFlags -> IO HookedBuildInfo,
    -- |Over-ride this hook to get different behavior during registration.
    unregHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> RegisterFlags -> IO (),
    -- |Hook to run after unregister command
    postUnreg :: Args -> RegisterFlags -> PackageDescription -> LocalBuildInfo -> IO (),

    -- |Hook to run before hscolour command.  Second arg indicates verbosity level.
    preHscolour  :: Args -> HscolourFlags -> IO HookedBuildInfo,
    -- |Over-ride this hook to get different behavior during hscolour.
    hscolourHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> HscolourFlags -> IO (),
    -- |Hook to run after hscolour command.  Second arg indicates verbosity level.
    postHscolour :: Args -> HscolourFlags -> PackageDescription -> LocalBuildInfo -> IO (),

    -- |Hook to run before haddock command.  Second arg indicates verbosity level.
    preHaddock  :: Args -> HaddockFlags -> IO HookedBuildInfo,
    -- |Over-ride this hook to get different behavior during haddock.
    haddockHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> HaddockFlags -> IO (),
    -- |Hook to run after haddock command.  Second arg indicates verbosity level.
    postHaddock :: Args -> HaddockFlags -> PackageDescription -> LocalBuildInfo -> IO ()
  }

-- |Empty 'UserHooks' which do nothing.
emptyUserHooks :: UserHooks
emptyUserHooks
  = UserHooks {
      runTests  = ru,
      readDesc  = return Nothing,
      hookedPreProcessors = [],
      hookedPrograms      = [],
      preConf   = rn,
      confHook  = (\_ _ -> return (error "No local build info generated during configure. Over-ride empty configure hook.")),
      postConf  = ru,
      preBuild  = rn,
      buildHook = ru,
      postBuild = ru,
      preMakefile = rn,
      makefileHook = ru,
      postMakefile = ru,
      preClean  = rn,
      cleanHook = ru,
      postClean = ru,
      preCopy   = rn,
      copyHook  = ru,
      postCopy  = ru,
      preInst   = rn,
      instHook  = ru,
      postInst  = ru,
      preSDist  = rn,
      sDistHook = ru,
      postSDist = ru,
      preReg    = rn,
      regHook   = ru,
      postReg   = ru,
      preUnreg  = rn,
      unregHook = ru,
      postUnreg = ru,
      preHscolour  = rn,
      hscolourHook = ru,
      postHscolour = ru,
      preHaddock   = rn,
      haddockHook  = ru,
      postHaddock  = ru
    }
    where rn args  _ = noExtraFlags args >> return emptyHookedBuildInfo
          ru _ _ _ _ = return ()
