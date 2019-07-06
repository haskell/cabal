\iffalse
{- cabal:
build-depends: base >= 4.3 && <5
-}
\fi
\documentclass{article}
\begin{document}
\begin{code}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Prelude

main :: IO ()
main = putStrLn "Hello World"
\end{code}
\end{document}
