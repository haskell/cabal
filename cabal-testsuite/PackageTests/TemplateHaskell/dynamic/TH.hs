{-# LANGUAGE TemplateHaskell #-}
module TH where

import Language.Haskell.TH (ExpQ)

splice :: ExpQ
splice = [| () |]
