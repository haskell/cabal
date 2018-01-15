{-# LANGUAGE DeriveGeneric #-}
module Distribution.Types.ExposedModule where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Backpack
import Distribution.ModuleName
import Distribution.Parsec.Class
import Distribution.ParseUtils   (parseModuleNameQ)
import Distribution.Pretty
import Distribution.Text

import qualified Distribution.Compat.CharParsing as P
import qualified Distribution.Compat.ReadP       as Parse
import qualified Text.PrettyPrint                as Disp

data ExposedModule
   = ExposedModule {
       exposedName      :: ModuleName,
       exposedReexport  :: Maybe OpenModule
     }
  deriving (Eq, Generic, Read, Show)

instance Pretty ExposedModule where
    pretty (ExposedModule m reexport) =
        Disp.hsep [ pretty m
                  , case reexport of
                     Just m' -> Disp.hsep [Disp.text "from", disp m']
                     Nothing -> Disp.empty
                  ]

instance Parsec ExposedModule where
    parsec = do
        m <- parsecMaybeQuoted parsec
        P.spaces

        reexport <- P.optional $ do
            _ <- P.string "from"
            P.skipSpaces1
            parsec

        return (ExposedModule m reexport)

instance Text ExposedModule where
    parse = do
        m <- parseModuleNameQ
        Parse.skipSpaces
        reexport <- Parse.option Nothing $ do
            _ <- Parse.string "from"
            Parse.skipSpaces
            fmap Just parse
        return (ExposedModule m reexport)

instance Binary ExposedModule
