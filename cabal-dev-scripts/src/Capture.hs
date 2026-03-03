module Capture (capture) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax (NameFlavour (..), Name (..))

import Data.Generics as SYB

-- | Capture the source code of declarations in the variable
capture
    :: String   -- ^ variable name
    -> Q [Dec]  -- ^ definitions
    -> Q [Dec]
capture name decls = do
    decls1 <- decls

    -- mangle all names to drop unique suffixes and module prefixes
    let decls2 = SYB.everywhere (SYB.mkT mangleName) decls1
    let declsStr = pprint decls2
    -- liftIO (putStrLn declsStr)

    let nameTyDecl :: Dec
        nameTyDecl = SigD (mkName name) (ConT (mkName "String"))

        nameDecl :: Dec
        nameDecl = ValD (VarP $ mkName name) (NormalB (LitE (StringL declsStr))) []

    return $ nameTyDecl : nameDecl : decls1
  where
    mangleName :: Name -> Name
    mangleName (Name occ _) = Name occ NameS
