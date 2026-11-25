import Test.Cabal.Prelude
import Test.Cabal.Script (runghc)

import System.Directory (doesDirectoryExist)
import System.FilePath (splitDrive)

-- On Windows the package directory is given as a UNC path, through the
-- administrative share of its drive. Cabal used to turn that path into MSYS
-- form itself, and sh could not find the script at the result.
--
-- On other systems this only checks an absolute --working-dir.
main = setupTest . recordMode DoNotRecord $ do
  env <- getTestEnv
  let pkgDir = viaAdminShare (testCurrentDir env)
  reachable <- liftIO $ doesDirectoryExist pkgDir
  skipUnless ("cannot reach " ++ pkgDir) reachable
  ghcPath <- programPathM ghcProgram
  r <-
    liftIO $
      runghc
        (testScriptEnv env)
        (Just (testTmpDir env))
        (testEnvironment env)
        (testCurrentDir env </> "Setup.hs")
        ["--working-dir", pkgDir, "configure", "--with-ghc", ghcPath]
  recordLog r
  void $ requireSuccess r

-- | @C:\\dir@ as @\\\\localhost\\C$\\dir@. Paths without a drive letter are
-- returned unchanged.
viaAdminShare :: FilePath -> FilePath
viaAdminShare path = case splitDrive path of
  ([letter, ':', _], rest) -> "\\\\localhost\\" ++ [letter] ++ "$\\" ++ rest
  _ -> path
