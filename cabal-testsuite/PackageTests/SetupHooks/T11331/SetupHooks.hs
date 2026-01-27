{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedRecordDot #-}

module SetupHooks (setupHooks) where

-- Cabal
import           Distribution.Simple.Flag (fromFlag)
import           Distribution.Simple.Setup (CommonSetupFlags (..))
import qualified Distribution.Simple.Utils as Utils
import           Distribution.Types.LocalBuildConfig (PackageBuildDescr (..))
import           Distribution.Verbosity

-- Cabal-hooks
import qualified Distribution.Simple.SetupHooks as SetupHooks

--------------------------------------------------------------------------------

setupHooks :: SetupHooks.SetupHooks
setupHooks = SetupHooks.noSetupHooks {
      SetupHooks.configureHooks = myConfigureHooks
    }

myConfigureHooks :: SetupHooks.ConfigureHooks
myConfigureHooks = SetupHooks.noConfigureHooks {
      SetupHooks.postConfPackageHook = Just myPostConfPackageHook
    }

myPostConfPackageHook :: SetupHooks.PostConfPackageInputs -> IO ()
myPostConfPackageHook pcpi = do
    Utils.notice verb "Hello, world"
  where
    verb = getVerbosity pcpi.packageBuildDescr

getVerbosity :: PackageBuildDescr -> Verbosity
getVerbosity pbd =
  fromFlag pbd.configFlags.configCommonFlags.setupVerbosity
