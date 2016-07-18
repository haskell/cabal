{-# LANGUAGE DeriveGeneric #-}

module Distribution.Simple.Command.Test.Config
    ( TestConfig(..)
    , module Distribution.Simple.Command.Test.Flags
    ) where

import GHC.Generics ( Generic )

import Distribution.Verbosity

import Distribution.Simple.Command.Test.Flags ( TestShowDetails(..) )
import Distribution.Simple.InstallDirs ( PathTemplate )


data TestConfig = TestConfig { testDistPref    :: FilePath
                             , testVerbosity   :: Verbosity
                             , testHumanLog    :: PathTemplate
                             , testMachineLog  :: PathTemplate
                             , testShowDetails :: TestShowDetails
                             , testKeepTix     :: Bool
                             , testOptions     :: [PathTemplate]
                               -- TODO: think about if/how options are
                               -- passed to test exes
                             }
  deriving (Generic)
