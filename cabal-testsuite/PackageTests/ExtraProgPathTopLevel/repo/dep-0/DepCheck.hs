{-# LANGUAGE TemplateHaskell #-}
module DepCheck where

import Dep (inPATH)

-- The top-level extra-prog-path of the project does not apply to dependencies.
$(inPATH False >> return [])
