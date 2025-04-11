import Control.Monad.Trans.Reader (asks)
import Data.List (isPrefixOf)
import Test.Cabal.Prelude

main = cabalTest $ do
  workdir <- asks testCurrentDir
  writeSourceFile "cabal.project" $
    unlines
      [ "packages: pkg-a"
      , "repository repo"
      , "  url: file+noindex://" <> workdir </> "repo"
      ]
  cabal "build" ["pkg-a"]
