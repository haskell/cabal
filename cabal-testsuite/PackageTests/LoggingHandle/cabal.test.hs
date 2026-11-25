import Test.Cabal.Prelude

import Data.List (isPrefixOf)
import System.Directory (listDirectory)

-- A dependency built with a build log logs all of its output to the build log
-- (#12529): here, the output of its ./configure script, of its hooks
-- executable, and of GHC.
main = cabalTest $ recordMode DoNotRecord $ do
  mpkgdb <- testPackageDbPath <$> getTestEnv
  case mpkgdb of
    Nothing -> skip "Cabal-hooks library unavailable."
    Just _pkgdb -> withRepo "repo" $ do
      res <- cabal' "build" ["-j2", "pkg"]
      assertOutputDoesNotContain "dep: running ./configure" res
      assertOutputDoesNotContain "unusedDep" res
      assertOutputDoesNotContain "hdep: running pre-conf hook" res
      assertOutputDoesNotContain "unusedHDep" res
      depLog <- buildLogFile "dep"
      assertFileDoesContain depLog "dep: running ./configure"
      assertFileDoesContain depLog "unusedDep"
      hdepLog <- buildLogFile "hdep"
      assertFileDoesContain hdepLog "hdep: running pre-conf hook"
      assertFileDoesContain hdepLog "unusedHDep"

-- The build log of the given package, in @logs/<compiler>/<unit id>.log@.
buildLogFile :: String -> TestM FilePath
buildLogFile pkgName = do
  logsDir <- (</> "logs") . testCabalDir <$> getTestEnv
  logFiles <- liftIO $ do
    compilerDirs <- listDirectory logsDir
    concat <$> sequence
      [ map (compilerLogsDir </>) . filter ((pkgName ++ "-") `isPrefixOf`)
          <$> listDirectory compilerLogsDir
      | compilerDir <- compilerDirs
      , let compilerLogsDir = logsDir </> compilerDir
      ]
  case logFiles of
    [logFile] -> return logFile
    _ -> assertFailure
           ("expected exactly one build log for " ++ pkgName ++ ", but got: " ++ show logFiles)
