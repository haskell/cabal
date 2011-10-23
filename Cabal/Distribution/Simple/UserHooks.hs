-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.UserHooks
-- Copyright   :  Isaac Jones 2003-2005
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This defines the API that @Setup.hs@ scripts can use to customise the way
-- the build works. This module just defines the 'UserHooks' type. The
-- predefined sets of hooks that implement the @Simple@, @Make@ and @Configure@
-- build systems are defined in "Distribution.Simple". The 'UserHooks' is a big
-- record of functions. There are 3 for each action, a pre, post and the action
-- itself. There are few other miscellaneous hooks, ones to extend the set of
-- programs and preprocessors and one to override the function used to read the
-- @.cabal@ file.
--
-- This hooks type is widely agreed to not be the right solution. Partly this
-- is because changes to it usually break custom @Setup.hs@ files and yet many
-- internal code changes do require changes to the hooks. For example we cannot
-- pass any extra parameters to most of the functions that implement the
-- various phases because it would involve changing the types of the
-- corresponding hook. At some point it will have to be replaced.

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
         (ConfigFlags, BuildFlags, CleanFlags, CopyFlags,
          InstallFlags, SDistFlags, RegisterFlags, HscolourFlags,
          HaddockFlags, TestFlags)
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
    readDesc :: IO (Maybe GenericPackageDescription),
    -- | Custom preprocessors in addition to and overriding 'knownSuffixHandlers'.
    hookedPreProcessors :: [ PPSuffixHandler ],
    -- | These programs are detected at configure time.  Arguments for them are
    -- added to the configure command.
    hookedPrograms :: [Program],

    -- |Hook to run before configure command
    preConf  :: Args -> ConfigFlags -> IO HookedBuildInfo,
    -- |Over-ride this hook to get different behavior during configure.
    confHook :: (GenericPackageDescription, HookedBuildInfo)
            -> ConfigFlags -> IO LocalBuildInfo,
    -- |Hook to run after configure command
    postConf :: Args -> ConfigFlags -> PackageDescription -> LocalBuildInfo -> IO (),

    -- |Hook to run before build command.  Second arg indicates verbosity level.
    preBuild  :: Args -> BuildFlags -> IO HookedBuildInfo,

    -- |Over-ride this hook to gbet different behavior during build.
    buildHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO (),
    -- |Hook to run after build command.  Second arg indicates verbosity level.
    postBuild :: Args -> BuildFlags -> PackageDescription -> LocalBuildInfo -> IO (),

    -- |Hook to run before clean command.  Second arg indicates verbosity level.
    preClean  :: Args -> CleanFlags -> IO HookedBuildInfo,
    -- |Over-ride this hook to get different behavior during clean.
    cleanHook :: PackageDescription -> () -> UserHooks -> CleanFlags -> IO (),
    -- |Hook to run after clean command.  Second arg indicates verbosity level.
    postClean :: Args -> CleanFlags -> PackageDescription -> () -> IO (),

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
    postHaddock :: Args -> HaddockFlags -> PackageDescription -> LocalBuildInfo -> IO (),

    -- |Hook to run before test command.
    preTest :: Args -> TestFlags -> IO HookedBuildInfo,
    -- |Over-ride this hook to get different behavior during test.
    testHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> TestFlags -> IO (),
    -- |Hook to run after test command.
    postTest :: Args -> TestFlags -> PackageDescription -> LocalBuildInfo -> IO ()
  }

{-# DEPRECATED runTests "Please use the new testing interface instead!" #-}

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
      postHaddock  = ru,
      preTest = \_ _ -> return emptyHookedBuildInfo, -- same as rn, but without
                                                     -- noExtraFlags
      testHook = ru,
      postTest = ru
    }
    where rn args  _ = noExtraFlags args >> return emptyHookedBuildInfo
          ru _ _ _ _ = return ()
