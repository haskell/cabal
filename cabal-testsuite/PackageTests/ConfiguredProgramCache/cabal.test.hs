import Test.Cabal.Prelude

import System.Directory (executable, getPermissions, setPermissions)
import System.FilePath ((</>))

-- The configure key fingerprints the configured program records: a new
-- version of a tool at the same path reconfigures, and elaborating the plan
-- again with the same programs does not. PathRecomp covers a changed PATH
-- reaching the repl.
main = cabalTest $ recordMode DoNotRecord $ do
  env <- getTestEnv
  let toolName = "versioned-haddock" ++ if isWindows then ".bat" else ""
      toolPath = testCurrentDir env </> toolName
      build = cabal' "build" ["--with-haddock=" ++ toolPath]
  writeSourceFile toolName $
    if isWindows
      then "@type \"%~dp0haddock-version\"\r\n"
      else "#!/bin/sh\ncat \"$(dirname \"$0\")/haddock-version\"\n"
  permissions <- liftIO $ getPermissions toolPath
  liftIO $ setPermissions toolPath permissions{executable = True}
  writeSourceFile "haddock-version" "Haddock version 2.30.0\n"
  _ <- build
  -- The script is the monitored file, not the version file it reads, so
  -- editing cabal.project is what makes cabal elaborate the plan again.
  writeSourceFile "cabal.project" "packages: .\nconstraints: base >= 0\n"
  build >>= assertOutputContains "Up to date"
  delay
  writeSourceFile "haddock-version" "Haddock version 2.31.0\n"
  writeSourceFile "cabal.project" "packages: .\nconstraints: base < 100\n"
  build >>= assertOutputContains "Configuring library"
