{-# LANGUAGE TemplateHaskell #-}
module Simple where

import Check

$(checkProgramSearchPath True >> return [])
