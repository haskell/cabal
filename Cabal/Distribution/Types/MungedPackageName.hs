{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
module Distribution.Types.MungedPackageName
  ( MungedPackageName (..)
  , decodeCompatPackageName
  , encodeCompatPackageName
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Parsec
import Distribution.Pretty
import Distribution.Types.LibraryName
import Distribution.Types.PackageName
import Distribution.Types.UnqualComponentName
import Distribution.FieldGrammar.Described

import qualified Distribution.Compat.CharParsing as P
import qualified Text.PrettyPrint as Disp

-- | A combination of a package and component name used in various legacy
-- interfaces, chiefly bundled with a version as 'MungedPackageId'. It's generally
-- better to use a 'UnitId' to opaquely refer to some compilation/packing unit,
-- but that doesn't always work, e.g. where a "name" is needed, in which case
-- this can be used as a fallback.
--
-- Use 'mkMungedPackageName' and 'unMungedPackageName' to convert from/to a 'String'.
--
-- In @3.0.0.0@ representation was changed from opaque (string) to semantic representation.
--
-- @since 2.0.0.2
--
data MungedPackageName = MungedPackageName !PackageName !LibraryName
  deriving (Generic, Read, Show, Eq, Ord, Typeable, Data)

instance Binary MungedPackageName
instance Structured MungedPackageName
instance NFData MungedPackageName where rnf = genericRnf

-- | Computes the package name for a library.  If this is the public
-- library, it will just be the original package name; otherwise,
-- it will be a munged package name recording the original package
-- name as well as the name of the internal library.
--
-- A lot of tooling in the Haskell ecosystem assumes that if something
-- is installed to the package database with the package name 'foo',
-- then it actually is an entry for the (only public) library in package
-- 'foo'.  With internal packages, this is not necessarily true:
-- a public library as well as arbitrarily many internal libraries may
-- come from the same package.  To prevent tools from getting confused
-- in this case, the package name of these internal libraries is munged
-- so that they do not conflict the public library proper.  A particular
-- case where this matters is ghc-pkg: if we don't munge the package
-- name, the inplace registration will OVERRIDE a different internal
-- library.
--
-- We munge into a reserved namespace, "z-", and encode both the
-- component name and the package name of an internal library using the
-- following format:
--
--      compat-pkg-name ::= "z-" package-name "-z-" library-name
--
-- where package-name and library-name have "-" ( "z" + ) "-"
-- segments encoded by adding an extra "z".
--
-- When we have the public library, the compat-pkg-name is just the
-- package-name, no surprises there!
--
-- >>> prettyShow $ MungedPackageName "servant" LMainLibName
-- "servant"
--
-- >>> prettyShow $ MungedPackageName "servant" (LSubLibName "lackey")
-- "z-servant-z-lackey"
--
instance Pretty MungedPackageName where
    -- First handle the cases where we can just use the original 'PackageName'.
    -- This is for the PRIMARY library, and it is non-Backpack, or the
    -- indefinite package for us.
    pretty = Disp.text . encodeCompatPackageName'

-- |
--
-- >>> simpleParsec "servant" :: Maybe MungedPackageName
-- Just (MungedPackageName (PackageName "servant") LMainLibName)
--
-- >>> simpleParsec "z-servant-z-lackey" :: Maybe MungedPackageName
-- Just (MungedPackageName (PackageName "servant") (LSubLibName (UnqualComponentName "lackey")))
--
-- >>> simpleParsec "z-servant-zz" :: Maybe MungedPackageName
-- Just (MungedPackageName (PackageName "z-servant-zz") LMainLibName)
--
instance Parsec MungedPackageName where
    parsec = decodeCompatPackageName' <$> parsecUnqualComponentName

instance Described MungedPackageName where
    describe _ = RETodo

-------------------------------------------------------------------------------
-- ZDashCode conversions
-------------------------------------------------------------------------------

-- | Intended for internal use only
--
-- >>> decodeCompatPackageName "z-servant-z-lackey"
-- MungedPackageName (PackageName "servant") (LSubLibName (UnqualComponentName "lackey"))
--
decodeCompatPackageName :: PackageName -> MungedPackageName
decodeCompatPackageName = decodeCompatPackageName' . unPackageName

-- | Intended for internal use only
--
-- >>> encodeCompatPackageName $ MungedPackageName "servant" (LSubLibName "lackey")
-- PackageName "z-servant-z-lackey"
--
-- This is used in @cabal-install@ in the Solver.
-- May become obsolete as solver moves to per-component solving.
--
encodeCompatPackageName :: MungedPackageName -> PackageName
encodeCompatPackageName = mkPackageName . encodeCompatPackageName'

decodeCompatPackageName' :: String -> MungedPackageName
decodeCompatPackageName' m =
    case m of
        'z':'-':rest | Right [pn, cn] <- explicitEitherParsec parseZDashCode rest
            -> MungedPackageName (mkPackageName pn) (LSubLibName (mkUnqualComponentName cn))
        s   -> MungedPackageName (mkPackageName s) LMainLibName

encodeCompatPackageName' :: MungedPackageName -> String
encodeCompatPackageName' (MungedPackageName pn LMainLibName)      = unPackageName pn
encodeCompatPackageName' (MungedPackageName pn (LSubLibName uqn)) =
     "z-" ++ zdashcode (unPackageName pn) ++
    "-z-" ++ zdashcode (unUnqualComponentName uqn)

zdashcode :: String -> String
zdashcode s = go s (Nothing :: Maybe Int) []
    where go [] _ r = reverse r
          go ('-':z) (Just n) r | n > 0 = go z (Just 0) ('-':'z':r)
          go ('-':z) _        r = go z (Just 0) ('-':r)
          go ('z':z) (Just n) r = go z (Just (n+1)) ('z':r)
          go (c:z)   _        r = go z Nothing (c:r)

parseZDashCode :: CabalParsing m => m [String]
parseZDashCode = do
    ns <- toList <$> P.sepByNonEmpty (some (P.satisfy (/= '-'))) (P.char '-')
    return (go ns)
  where
    go ns = case break (=="z") ns of
                (_, []) -> [paste ns]
                (as, "z":bs) -> paste as : go bs
                _ -> error "parseZDashCode: go"
    unZ :: String -> String
    unZ "" = error "parseZDashCode: unZ"
    unZ r@('z':zs) | all (=='z') zs = zs
                   | otherwise      = r
    unZ r = r
    paste :: [String] -> String
    paste = intercalate "-" . map unZ

-- $setup
-- >>> :seti -XOverloadedStrings
