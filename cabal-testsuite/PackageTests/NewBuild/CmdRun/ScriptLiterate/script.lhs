%cabal:
%build-depends: base >= 4.3 && <5
%endcabal
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Prelude

main :: IO ()
main = putStrLn "Hello World"
