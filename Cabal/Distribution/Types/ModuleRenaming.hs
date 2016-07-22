{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Distribution.Types.ModuleRenaming (
    ModuleRenaming(..),
    defaultRenaming,
    lookupRenaming,
) where

import Distribution.Compat.Binary
import Distribution.Compat.Semigroup
import qualified Distribution.Compat.ReadP as Parse
import Distribution.Compat.ReadP   ((<++))
import Distribution.Package
import Distribution.ModuleName
import Distribution.Text

import Data.Typeable               (Typeable)
import Data.Data                  (Data)
import GHC.Generics                (Generic)
import qualified Text.PrettyPrint as Disp
import Text.PrettyPrint ((<+>), text)
import qualified Data.Map as Map
import Data.Map                    (Map)

-- ---------------------------------------------------------------------------
-- Module renaming

-- | Renaming applied to the modules provided by a package.
-- The boolean indicates whether or not to also include all of the
-- original names of modules.  Thus, @ModuleRenaming False []@ is
-- "don't expose any modules, and @ModuleRenaming True [("Data.Bool", "Bool")]@
-- is, "expose all modules, but also expose @Data.Bool@ as @Bool@".
--
data ModuleRenaming = ModuleRenaming Bool [(ModuleName, ModuleName)]
    deriving (Show, Read, Eq, Ord, Typeable, Data, Generic)

defaultRenaming :: ModuleRenaming
defaultRenaming = ModuleRenaming True []

lookupRenaming :: Package pkg => pkg -> Map PackageName ModuleRenaming -> ModuleRenaming
lookupRenaming = Map.findWithDefault defaultRenaming . packageName

instance Binary ModuleRenaming where

instance Monoid ModuleRenaming where
    mempty = ModuleRenaming False []
    mappend = (<>)

instance Semigroup ModuleRenaming where
    ModuleRenaming b rns <> ModuleRenaming b' rns'
        = ModuleRenaming (b || b') (rns ++ rns') -- TODO: dedupe?

-- NB: parentheses are mandatory, because later we may extend this syntax
-- to allow "hiding (A, B)" or other modifier words.
instance Text ModuleRenaming where
  disp (ModuleRenaming True []) = Disp.empty
  disp (ModuleRenaming b vs) = (if b then text "with" else Disp.empty) <+> dispRns
    where dispRns = Disp.parens
                         (Disp.hsep
                            (Disp.punctuate Disp.comma (map dispEntry vs)))
          dispEntry (orig, new)
            | orig == new = disp orig
            | otherwise = disp orig <+> text "as" <+> disp new

  parse = do Parse.string "with" >> Parse.skipSpaces
             fmap (ModuleRenaming True) parseRns
         <++ fmap (ModuleRenaming False) parseRns
         <++ return (ModuleRenaming True [])
    where parseRns = do
             rns <- Parse.between (Parse.char '(') (Parse.char ')') parseList
             Parse.skipSpaces
             return rns
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
