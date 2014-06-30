{-# LANGUAGE DeriveDataTypeable #-}
module Distribution.ModuleExport(ModuleExport(..)) where

import Distribution.ParseUtils
         ( parseQuoted )
import Distribution.Text
         ( Text(disp, parse) )
import Distribution.Compat.ReadP
         ( (<++) )
import Data.List
         ( intersperse )
import Distribution.Package
         ( PackageId )
import qualified Distribution.Compat.ReadP as Parse
import qualified Text.PrettyPrint as Disp
import Data.Data

-- ModuleExport has a very interesting invariant: in the installed
-- package database, a ModuleExport is guaranteed to point to the
-- original module which defined the module. Of course, when a user
-- writes a ModuleExport, it may not have this property.  ghc-pkg is
-- responsible for enforcing this invariant.

data ModuleExport m = ModuleExport {
    exportName :: m,
    exportOrigPackageId :: PackageId,
    exportOrigName :: m
} deriving (Read, Show, Eq, Data, Typeable)

-- EZY (Jul 2014): Honestly, a bit of a hack, but it's useful in GHC.
instance Functor ModuleExport where
    fmap f (ModuleExport m pid m') = ModuleExport (f m) pid (f m')

instance Text m => Text (ModuleExport m) where
    disp (ModuleExport m pid' m') =
      Disp.hcat (intersperse (Disp.char ':') [disp m, disp pid', disp m'])
    parse = do let parseQ = parseQuoted parse <++ parse
               m <- parseQ
               _ <- Parse.char ':'
               pid' <- parse
               _ <- Parse.char ':'
               m' <- parseQ
               return (ModuleExport m pid' m')
