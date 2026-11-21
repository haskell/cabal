{-# LANGUAGE TemplateHaskell #-}
module Local where

import Dep (inPATH)

-- The top-level extra-prog-path applies to local packages.
$(inPATH True >> return [])
