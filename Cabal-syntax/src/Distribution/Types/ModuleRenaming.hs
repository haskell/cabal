{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}

module Distribution.Types.ModuleRenaming
  ( ModuleRenaming (..)
  , interpModuleRenaming
  , defaultRenaming
  , isDefaultRenaming
  ) where

import Distribution.CabalSpecVersion
import Distribution.Compat.Prelude hiding (empty)
import Prelude ()

import Distribution.ModuleName
import Distribution.Parsec
import Distribution.Pretty

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Distribution.Compat.CharParsing as P
import Text.PrettyPrint (comma, hsep, parens, punctuate, text)

-- | Renaming applied to the modules provided by a package.
-- The boolean indicates whether or not to also include all of the
-- original names of modules.  Thus, @ModuleRenaming False []@ is
-- "don't expose any modules, and @ModuleRenaming True [("Data.Bool", "Bool")]@
-- is, "expose all modules, but also expose @Data.Bool@ as @Bool@".
-- If a renaming is omitted you get the 'DefaultRenaming'.
--
-- (NB: This is a list not a map so that we can preserve order.)
data ModuleRenaming
  = -- | A module renaming/thinning; e.g., @(A as B, C as C)@
    -- brings @B@ and @C@ into scope.
    ModuleRenaming [(ModuleName, ModuleName)]
  | -- | The default renaming, bringing all exported modules
    -- into scope.
    DefaultRenaming
  | -- | Hiding renaming, e.g., @hiding (A, B)@, bringing all
    -- exported modules into scope except the hidden ones.
    HidingRenaming [ModuleName]
  deriving (Show, Read, Eq, Ord, Typeable, Data, Generic)

-- | Interpret a 'ModuleRenaming' as a partial map from 'ModuleName'
-- to 'ModuleName'.  For efficiency, you should partially apply it
-- with 'ModuleRenaming' and then reuse it.
interpModuleRenaming :: ModuleRenaming -> ModuleName -> Maybe ModuleName
interpModuleRenaming DefaultRenaming = Just
interpModuleRenaming (ModuleRenaming rns) =
  let m = Map.fromList rns
   in \k -> Map.lookup k m
interpModuleRenaming (HidingRenaming hs) =
  let s = Set.fromList hs
   in \k -> if k `Set.member` s then Nothing else Just k

-- | The default renaming, if something is specified in @build-depends@
-- only.
defaultRenaming :: ModuleRenaming
defaultRenaming = DefaultRenaming

-- | Tests if its the default renaming; we can use a more compact syntax
-- in 'Distribution.Types.IncludeRenaming.IncludeRenaming' in this case.
isDefaultRenaming :: ModuleRenaming -> Bool
isDefaultRenaming DefaultRenaming = True
isDefaultRenaming _ = False

instance Binary ModuleRenaming
instance Structured ModuleRenaming

instance NFData ModuleRenaming where rnf = genericRnf

-- NB: parentheses are mandatory, because later we may extend this syntax
-- to allow "hiding (A, B)" or other modifier words.
instance Pretty ModuleRenaming where
  pretty DefaultRenaming = mempty
  pretty (HidingRenaming hides) =
    text "hiding" <+> parens (hsep (punctuate comma (map pretty hides)))
  pretty (ModuleRenaming rns) =
    parens . hsep $ punctuate comma (map dispEntry rns)
    where
      dispEntry (orig, new)
        | orig == new = pretty orig
        | otherwise = pretty orig <+> text "as" <+> pretty new

instance Parsec ModuleRenaming where
  parsec = do
    csv <- askCabalSpecVersion
    if csv >= CabalSpecV3_0
      then moduleRenamingParsec parensLax lexemeParsec
      else moduleRenamingParsec parensStrict parsec
    where
      -- For cabal spec versions < 3.0 white spaces were not skipped
      -- after the '(' and ')' tokens in the mixin field. This
      -- parser checks the cabal file version and does the correct
      -- skipping of spaces.
      parensLax p = P.between (P.char '(' >> P.spaces) (P.char ')' >> P.spaces) p
      parensStrict p = P.between (P.char '(' >> warnSpaces) (P.char ')') p

      warnSpaces =
        P.optional $
          P.space *> fail "space after parenthesis, use cabal-version: 3.0 or higher"

moduleRenamingParsec
  :: CabalParsing m
  => (forall a. m a -> m a)
  -- ^ between parens
  -> m ModuleName
  -- ^ module name parser
  -> m ModuleRenaming
moduleRenamingParsec bp mn =
  -- NB: try not necessary as the first token is obvious
  P.choice [parseRename, parseHiding, return DefaultRenaming]
  where
    cma = P.char ',' >> P.spaces
    parseRename = do
      rns <- bp parseList
      P.spaces
      return (ModuleRenaming rns)
    parseHiding = do
      _ <- P.string "hiding"
      P.spaces -- space isn't strictly required as next is an open paren
      hides <- bp (P.sepBy mn cma)
      return (HidingRenaming hides)
    parseList =
      P.sepBy parseEntry cma
    parseEntry = do
      orig <- parsec
      P.spaces
      P.option (orig, orig) $ do
        _ <- P.string "as"
        P.skipSpaces1 -- require space after "as"
        new <- parsec
        P.spaces
        return (orig, new)
