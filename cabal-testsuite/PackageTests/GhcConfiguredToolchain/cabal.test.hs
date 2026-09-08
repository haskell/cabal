import Test.Cabal.Prelude

import Data.Char (isSpace)
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe)
import System.Directory (createDirectoryIfMissing, executable, getPermissions, setPermissions)
import System.Environment (lookupEnv)

-- Cabal must use the C toolchain GHC was configured with (the settings file
-- exposed by `ghc --info`) rather than a like-named tool found on the PATH.
main :: IO ()
main = cabalTest $ recordMode DoNotRecord $ do
  env <- getTestEnv
  let pkgDir = testCurrentDir env

  -- Ask GHC what C toolchain it was configured with.
  infoOutput <- resultOutput <$> ghc' ["--info"]
  let mbCC = parseGhcInfo infoOutput >>= lookup "C compiler command"

  case mbCC of
    -- GHC records an absolute path: Cabal must invoke exactly that C
    -- compiler, even when a like-named program shadows it on the PATH.
    Just cc | isAbsolute cc -> do
      decoyEnv <-
        if isWindows
          then return []
          else do
            let decoyBin = pkgDir </> "decoy-bin"
            liftIO $ do
              createDirectoryIfMissing True decoyBin
              let decoy = decoyBin </> takeFileName cc
              writeFile decoy $
                unlines
                  [ "#!/bin/sh"
                  , "exec " ++ show cc ++ " \"$@\""
                  ]
              perms <- getPermissions decoy
              setPermissions decoy perms{executable = True}
            originalPath <- fromMaybe "" <$> liftIO (lookupEnv "PATH")
            return [("PATH", Just (decoyBin ++ searchPathSeparator : originalPath))]
      withEnv decoyEnv $ do
        res <- cabal' "v2-build" ["all"]
        -- Depending on the platform's command-line escaping rules the
        -- recorded path may be quoted.
        assertBool
          "Cabal did not use the C compiler GHC was configured with"
          ( ("-pgmc " ++ cc)
              `isInfixOf` filter (`notElem` ("\"'" :: String)) (resultOutput res)
          )
      withPlan $ runPlanExe "ghc-toolchain" "ghc-toolchain-exe" []
    -- GHC only records a bare program name (or nothing): looking it up on
    -- the PATH is then exactly what GHC does itself.
    _ -> do
      void $ cabal' "v2-build" ["all"]
      withPlan $ runPlanExe "ghc-toolchain" "ghc-toolchain-exe" []

parseGhcInfo :: String -> Maybe [(String, String)]
parseGhcInfo str = case reads str of
  [(xs, rest)] | all isSpace rest -> Just xs
  _ -> Nothing
