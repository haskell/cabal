{-# LANGUAGE DeriveDataTypeable #-}
module Distribution.ModuleExport(ModuleExport(..)) where

import Distribution.Text
         ( Text(disp, parse) )
import Distribution.Compat.ReadP
         ( (+++) )
import Distribution.Package
         ( PackageName, InstalledPackageId )
import qualified Distribution.Compat.ReadP as Parse
import qualified Text.PrettyPrint as Disp
import Text.PrettyPrint ((<+>),(<>))
import Data.Data

-- | Defines a reexport of module 'exportOrigName' from package
-- 'exportOrigPackageId' as new module name 'exportName'.  This data type has an
-- interesting invariant: in the installed package database, a ModuleExport is
-- guaranteed to point to the original module which defined the module. Of
-- course, when a user writes a ModuleExport, it may not have this property.
-- ghc-pkg is responsible for enforcing this invariant.
data ModuleExport m = ModuleExport {
    -- | Original package name of the reexported module, or Nothing if
    -- the user wants us to figure it out automatically.  (Note: this package
    -- could have reexported the module itself.)
    exportOrigPackageName :: Maybe PackageName,
    -- | Original module name of reexported module.
    exportOrigName :: m,
    -- | New module name of reexported module, available to clients
    -- of this package.
    exportName :: m,
    -- | A hack! When ghc-pkg processes 'ModuleExport', it is able to resolve
    -- the true, original location an identifier lived in (this cannot be done
    -- without consulting the package database), it fills it in here so that
    -- GHC can use it.  When we get GHC to stop using 'InstalledPackageInfo',
    -- this hack can go away.
    exportCachedTrueOrig :: Maybe (InstalledPackageId, m)
} deriving (Read, Show, Eq, Data, Typeable)

-- Handy when we need to convert from one ModuleName representation to
-- another (it's used in GHC.)
instance Functor ModuleExport where
    fmap f (ModuleExport pnm m m' c) = ModuleExport pnm (f m) (f m')
                                                    (fmap (\(x,y)->(x,f y)) c)

instance (Eq m, Text m) => Text (ModuleExport m) where
    disp ModuleExport{ exportOrigPackageName = mpnm
                     , exportOrigName = m
                     , exportName = m'
                     , exportCachedTrueOrig = c }
        = (maybe Disp.empty (\pnm -> disp pnm <> Disp.char ':') mpnm)
       <> disp m
      <+> (if m == m'
            then Disp.empty
            else Disp.text "as" <+> disp m')
      <+> (maybe Disp.empty (\(c_ipid, c_m) ->
            Disp.parens (disp c_m <> Disp.char '@' <> disp c_ipid)) c)
    parse = do Parse.skipSpaces
               mpnm <- (do pnm <- parse
                           _ <- Parse.char ':'
                           return (Just pnm)
                        +++ return Nothing)
               m <- parse
               m' <- (do Parse.skipSpaces
                         _ <- Parse.string "as"
                         Parse.skipSpaces
                         parse)
                     +++ return m
               c <- (do Parse.skipSpaces
                        _ <- Parse.char '('
                        c_m <- parse
                        _ <- Parse.char '@'
                        c_ipid <- parse
                        _ <- Parse.char ')'
                        return (Just (c_ipid, c_m))
                     +++ return Nothing)
               return ModuleExport{ exportOrigPackageName = mpnm
                                  , exportOrigName = m
                                  , exportName = m'
                                  , exportCachedTrueOrig = c }
