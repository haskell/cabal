module Distribution.Client.Dependency.Modular.Package
  (module Distribution.Client.Dependency.Modular.Package,
   module Distribution.Package) where

import Data.List as L
import Data.Map as M

import Distribution.Package -- from Cabal
import Distribution.Text    -- from Cabal

import Distribution.Client.Dependency.Modular.Version

-- | A package name.
type PN = PackageName

-- | Unpacking a package name.
unPN :: PN -> String
unPN (PackageName pn) = pn

-- | Package version. A package name plus a version number.
type PV = PackageId

-- | Qualified package version.
type QPV = Q PV

-- | Package id. Currently just a black-box string.
type PId = InstalledPackageId

-- | Location. Info about whether a package is installed or not, and where
-- exactly it is located. For installed packages, uniquely identifies the
-- package instance via its 'PId'.
--
-- TODO: More information is needed about the repo.
data Loc = Inst PId | InRepo
  deriving (Eq, Ord, Show)

-- | Instance. A version number and a location.
data I = I Ver Loc
  deriving (Eq, Ord, Show)

-- | String representation of an instance.
showI :: I -> String
showI (I v InRepo)   = showVer v
showI (I v (Inst (InstalledPackageId i))) = showVer v ++ "/installed" ++ shortId i
  where
    -- A hack to extract the beginning of the package ABI hash
    shortId = snip (splitAt 4) (++ "...") .
              snip ((\ (x, y) -> (reverse x, y)) . break (=='-') . reverse) ('-':)
    snip p f xs = case p xs of
                    (ys, zs) -> (if L.null zs then id else f) ys

-- | Package instance. A package name and an instance.
data PI qpn = PI qpn I
  deriving (Eq, Ord, Show)

-- | String representation of a package instance.
showPI :: PI QPN -> String
showPI (PI qpn i) = showQPN qpn ++ "-" ++ showI i

-- | Checks if a package instance corresponds to an installed package.
instPI :: PI qpn -> Bool
instPI (PI _ (I _ (Inst _))) = True
instPI _                     = False

instI :: I -> Bool
instI (I _ (Inst _)) = True
instI _              = False

instance Functor PI where
  fmap f (PI x y) = PI (f x) y

-- | Package path. (Stored in "reverse" order.)
type PP = [PN]

-- | String representation of a package path.
showPP :: PP -> String
showPP = intercalate "." . L.map display . reverse


-- | A qualified entity. Pairs a package path with the entity.
data Q a = Q PP a
  deriving (Eq, Ord, Show)

-- | Standard string representation of a qualified entity.
showQ :: (a -> String) -> (Q a -> String)
showQ showa (Q [] x) = showa x
showQ showa (Q pp x) = showPP pp ++ "." ++ showa x

-- | Qualified package name.
type QPN = Q PN

-- | String representation of a qualified package path.
showQPN :: QPN -> String
showQPN = showQ display

-- | The scope associates every package with a path. The convention is that packages
-- not in the data structure have an empty path associated with them.
type Scope = Map PN PP

-- | An empty scope structure, for initialization.
emptyScope :: Scope
emptyScope = M.empty

-- | Create artificial parents for each of the package names, making
-- them all independent.
makeIndependent :: [PN] -> Scope
makeIndependent ps = L.foldl (\ sc (n, p) -> M.insert p [PackageName (show n)] sc) emptyScope (zip ([0..] :: [Int]) ps)

qualify :: Scope -> PN -> QPN
qualify sc pn = Q (findWithDefault [] pn sc) pn

unQualify :: Q a -> a
unQualify (Q _ x) = x
