{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
module Custom where

import Check

-- Setups built against Cabal < 3.19 list PATH directories twice.
#ifdef UNIQUE_PATH
$(checkProgramSearchPath True >> return [])
#else
$(checkProgramSearchPath False >> return [])
#endif
