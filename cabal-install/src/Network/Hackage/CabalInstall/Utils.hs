module Network.Hackage.CabalInstall.Utils where

import Distribution.Verbosity
import Network.Hackage.CabalInstall.Types



isVerbose cfg = configVerbose cfg >= verbose