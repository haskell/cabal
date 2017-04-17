{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Distribution.Types.ModuleRenaming (
    ModuleRenaming(..),
    interpModuleRenaming,
    defaultRenaming,
    isDefaultRenaming,
) where

import Prelude ()
import Distribution.Compat.Prelude hiding (empty)

import qualified Distribution.Compat.ReadP as Parse
import Distribution.Compat.ReadP   ((<++))
import Distribution.ModuleName
import Distribution.Text

import qualified Data.Map as Map
import qualified Data.Set as Set

import Text.PrettyPrint

-- | Renaming applied to the modules provided by a package.
-- The boolean indicates whether or not to also include all of the
-- original names of modules.  Thus, @ModuleRenaming False []@ is
-- "don't expose any modules, and @ModuleRenaming True [("Data.Bool", "Bool")]@
-- is, "expose all modules, but also expose @Data.Bool@ as @Bool@".
-- If a renaming is omitted you get the 'DefaultRenaming'.
--
-- (NB: This is a list not a map so that we can preserve order.)
--
data ModuleRenaming
        -- | A module renaming/thinning; e.g., @(A as B, C as C)@
        -- brings @B@ and @C@ into scope.
        = ModuleRenaming [(ModuleName, ModuleName)]
        -- | The default renaming, bringing all exported modules
        -- into scope.
        | DefaultRenaming
        -- | Hiding renaming, e.g., @hiding (A, B)@, bringing all
        -- exported modules into scope except the hidden ones.
        | HidingRenaming [ModuleName]
    deriving (Show, Read, Eq, Ord, Typeable, Data, Generic)

-- | Interpret a 'ModuleRenaming' as a partial map from 'ModuleName'
-- to 'ModuleName'.  For efficiency, you should partially apply it
-- with 'ModuleRenaming' and then reuse it.
interpModuleRenaming :: ModuleRenaming -> ModuleName -> Maybe ModuleName
interpModuleRenaming DefaultRenaming = \m -> Just m
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

instance Binary ModuleRenaming where

-- NB: parentheses are mandatory, because later we may extend this syntax
-- to allow "hiding (A, B)" or other modifier words.
instance Text ModuleRenaming where
  disp DefaultRenaming = empty
  disp (HidingRenaming hides)
        = text "hiding" <+> parens (hsep (punctuate comma (map disp hides)))
  disp (ModuleRenaming rns)
        = parens . hsep $ punctuate comma (map dispEntry rns)
    where dispEntry (orig, new)
            | orig == new = disp orig
            | otherwise = disp orig <+> text "as" <+> disp new

  parse = do fmap ModuleRenaming parseRns
             <++ parseHidingRenaming
             <++ return DefaultRenaming
    where parseRns = do
             rns <- Parse.between (Parse.char '(') (Parse.char ')') parseList
             Parse.skipSpaces
             return rns
          parseHidingRenaming = do
            _ <- Parse.string "hiding"
            Parse.skipSpaces
            hides <- Parse.between (Parse.char '(') (Parse.char ')')
                        (Parse.sepBy parse (Parse.char ',' >> Parse.skipSpaces))
            return (HidingRenaming hides)
          parseList =
            Parse.sepBy parseEntry (Parse.char ',' >> Parse.skipSpaces)
          parseEntry :: Parse.ReadP r (ModuleName, ModuleName)
          parseEntry = do
            orig <- parse
            Parse.skipSpaces
            (do _ <- Parse.string "as"
                Parse.skipSpaces
                new <- parse
                Parse.skipSpaces
                return (orig, new)
             <++
                return (orig, orig))
