{-# LANGUAGE DeriveGeneric #-}

module Distribution.Client.Types.OverwritePolicy where

import Distribution.Client.Compat.Prelude
import Prelude ()

import qualified Distribution.Compat.CharParsing as P
import qualified Text.PrettyPrint as PP

data OverwritePolicy
  = NeverOverwrite
  | AlwaysOverwrite
  | PromptOverwrite
  deriving (Show, Eq, Generic, Bounded, Enum)

instance Binary OverwritePolicy
instance Structured OverwritePolicy

instance Parsec OverwritePolicy where
  parsec = do
    name <- P.munch1 isAlpha
    case name of
      "always" -> pure AlwaysOverwrite
      "never" -> pure NeverOverwrite
      "prompt" -> pure PromptOverwrite
      _ -> P.unexpected $ "OverwritePolicy: " ++ name

instance Pretty OverwritePolicy where
  pretty NeverOverwrite = PP.text "never"
  pretty AlwaysOverwrite = PP.text "always"
  pretty PromptOverwrite = PP.text "prompt"
