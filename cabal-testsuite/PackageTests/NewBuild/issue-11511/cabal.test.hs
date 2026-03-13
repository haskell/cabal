import Test.Cabal.Prelude
import Data.List (isPrefixOf)

main = cabalTest (withProjectFile "cabal.project" $ do

  first_update <- cabal_build
  when (first_update /= 1) $ assertFailure $ "Expected exactly one download on first 'cabal build', but got " <> show first_update

  second_update <- cabal_build
  when (second_update /= 0) $ assertFailure $ "Expected exactly zero downloads on second 'cabal build', but got " <> show second_update
  )
 where
  cabal_build = length
              . filter ("Downloading https://hackage.haskell.org/package/filepath-1.4.2.2/filepath-1.4.2.2.tar.gz" `isPrefixOf`)
              . lines
              . resultOutput
             <$> recordMode DoNotRecord (cabal' "build" ["--dry-run", "--allow-newer=all", "all"])
