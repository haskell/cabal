{-# LANGUAGE TemplateHaskell #-}
module MyLib where

import Control.Concurrent
import Language.Haskell.TH

-- Must take longer to compile than the testsuite
$(do runIO $
       threadDelay (5*1000*1000) -- 5s
     [d| data X |]
  )
