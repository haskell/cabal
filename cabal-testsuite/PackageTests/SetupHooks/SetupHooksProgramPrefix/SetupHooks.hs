module SetupHooks where

import Distribution.Simple.SetupHooks
import Distribution.Simple.InstallDirs ( toPathTemplate )
import Distribution.Types.LocalBuildConfig ( BuildOptions (..) )

setupHooks :: SetupHooks
setupHooks =
  noSetupHooks
    { configureHooks =
        noConfigureHooks
          { preConfPackageHook = Just preConfPkg }
    }

preConfPkg :: PreConfPackageHook
preConfPkg inputs =
  let outputs@PreConfPackageOutputs{buildOptions = opts} = noPreConfPackageOutputs inputs
  in return outputs{buildOptions = opts{programPrefix = Just (toPathTemplate "pre-")}}
