-- 2022-09-20, issue #8487
--

import Test.Cabal.Prelude

main = cabalTest $ do
  cabalG [ "--config-file", "config.file" ] "build" [ "test" ]
