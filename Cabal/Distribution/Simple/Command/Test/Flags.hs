{-# LANGUAGE DeriveGeneric #-}

module Distribution.Simple.Command.Test.Flags
    ( TestFlags(..)
    , TestShowDetails(..), knownTestShowDetails
    ) where

import Data.Char ( isAlpha )
import GHC.Generics ( Generic )

import qualified Text.PrettyPrint as Disp
import qualified Distribution.Compat.ReadP as Parse

import Distribution.Compat.Semigroup
import Distribution.Flag
import Distribution.Text
import Distribution.Verbosity

import Distribution.Simple.InstallDirs
import Distribution.Simple.Utils ( lowercase )

data TestFlags = TestFlags { testDistPref    :: Flag FilePath
                           , testVerbosity   :: Flag Verbosity
                           , testHumanLog    :: Flag PathTemplate
                           , testMachineLog  :: Flag PathTemplate
                           , testShowDetails :: Flag TestShowDetails
                           , testKeepTix     :: Flag Bool
                           , testOptions     :: [PathTemplate]
                             -- TODO: think about if/how options are
                             -- passed to test exes
                           }
  deriving (Generic)

instance Monoid TestFlags where
    mempty = gmempty
    mappend = (<>)

instance Semigroup TestFlags where
    (<>) = gmappend

data TestShowDetails = Never | Failures | Always | Streaming | Direct
    deriving (Eq, Ord, Enum, Bounded, Show)

knownTestShowDetails :: [TestShowDetails]
knownTestShowDetails = [minBound..maxBound]

instance Text TestShowDetails where
    disp  = Disp.text . lowercase . show

    parse = maybe Parse.pfail return . classify =<< ident
      where
        ident        = Parse.munch1 (\c -> isAlpha c || c == '_' || c == '-')
        classify str = lookup (lowercase str) enumMap
        enumMap     :: [(String, TestShowDetails)]
        enumMap      = [ (display x, x)
                       | x <- knownTestShowDetails ]

--TODO: do we need this instance?
instance Monoid TestShowDetails where
    mempty = Never
    mappend = (<>)

instance Semigroup TestShowDetails where
    a <> b = if a < b then b else a
