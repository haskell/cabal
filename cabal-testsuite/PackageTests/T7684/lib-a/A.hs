{-# LANGUAGE TemplateHaskellQuotes #-}

module A where

import Language.Haskell.TH.Syntax ( Q, Exp )

a :: Q Exp
a = [| putStrLn "a" |]
