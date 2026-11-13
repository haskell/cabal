import Distribution.Simple
import Distribution.Simple.Flag ( flagToMaybe )
import Distribution.Simple.Setup ( CommonSetupFlags (..), ConfigFlags (..) )

import Control.Monad ( unless )
import System.Directory ( doesFileExist )

main :: IO ()
main = defaultMainWithHooks simpleUserHooks { preConf = checkWorkingDir }
  where
    checkWorkingDir args flags = do
      -- The Setup script is not passed a working directory...
      case flagToMaybe $ setupWorkingDir $ configCommonFlags flags of
        Nothing -> return ()
        Just _ -> fail "the Setup script was passed a working directory"
      -- ...and runs in the package directory.
      found <- doesFileExist "marker.txt"
      unless found $ fail "the Setup script does not run in the package directory"
      putStrLn "pkg Setup.hs: working directory OK"
      preConf simpleUserHooks args flags
