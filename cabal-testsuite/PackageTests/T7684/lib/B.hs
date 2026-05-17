{-# LANGUAGE TemplateHaskell #-}

module B where

import A

main = $(a)
