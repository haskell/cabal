import Test.Cabal.Prelude

import Data.List (isPrefixOf)
import System.Directory (listDirectory)

-- A dependency built with a build log logs all of its output to the build log
-- (#12529): here, the output of its ./configure script and of GHC.
main = cabalTest $ recordMode DoNotRecord $ withRepo "repo" $ do
  res <- cabal' "build" ["-j2", "pkg"]
  assertOutputDoesNotContain "dep: running ./configure" res
  assertOutputDoesNotContain "unusedDep" res
  logFile <- depLogFile
  assertFileDoesContain logFile "dep: running ./configure"
  assertFileDoesContain logFile "unusedDep"

-- The build log of the dep package, in @logs/<compiler>/<unit id>.log@.
depLogFile :: TestM FilePath
depLogFile = do
  logsDir <- (</> "logs") . testCabalDir <$> getTestEnv
  logFiles <- liftIO $ do
    compilerDirs <- listDirectory logsDir
    concat <$> sequence
      [ map (compilerLogsDir </>) <$> listDirectory compilerLogsDir
      | compilerDir <- compilerDirs
      , let compilerLogsDir = logsDir </> compilerDir
      ]
  case filter (("dep-0.1-" `isPrefixOf`) . takeFileName) logFiles of
    [logFile] -> return logFile
    _ -> assertFailure ("expected one build log for dep in: " ++ show logFiles)
