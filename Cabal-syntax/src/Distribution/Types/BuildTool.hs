{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Distribution.Types.BuildTool
  ( BuildTool (..)
  , knownBuildTools
  , classifyBuildTool
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import qualified Distribution.Compat.CharParsing as P
import Distribution.Parsec (Parsec (..))
import Distribution.Pretty (Pretty (..), prettyShow)
import Distribution.Utils.Generic (lowercase)
import qualified Text.PrettyPrint as Disp

-- | The tool building a package, as queried by the @builder(...)@ conditional.
--
-- This mirrors 'Distribution.Compiler.CompilerFlavor': known tools are
-- enumerated, and any unrecognised name parses to 'OtherBuildTool' rather than
-- failing, so that a @builder(...)@ guarding a name a given builder does not
-- know about simply evaluates to 'False' for it.
--
-- See <https://github.com/haskell/cabal/issues/10386>.
--
-- @since 3.18.0.0
data BuildTool
  = -- | The Cabal library (and the tools that build with it, e.g.
    -- cabal-install).
    Cabal
  | -- | MicroHs's build tool, see <https://github.com/augustss/MicroHs>.
    MCabal
  | OtherBuildTool String
  deriving (Generic, Show, Read, Eq, Ord, Data)

instance Binary BuildTool
instance Structured BuildTool
instance NFData BuildTool where rnf = genericRnf

knownBuildTools :: [BuildTool]
knownBuildTools = [Cabal, MCabal]

instance Pretty BuildTool where
  pretty (OtherBuildTool name) = Disp.text name
  pretty other = Disp.text (lowercase (show other))

instance Parsec BuildTool where
  parsec = classifyBuildTool <$> component
    where
      component = do
        cs <- P.munch1 isAlphaNum
        if all isDigit cs then fail "all digits build tool name" else return cs

classifyBuildTool :: String -> BuildTool
classifyBuildTool s =
  fromMaybe (OtherBuildTool s) $ lookup (lowercase s) buildToolMap
  where
    buildToolMap =
      [ (lowercase (prettyShow tool), tool)
      | tool <- knownBuildTools
      ]
