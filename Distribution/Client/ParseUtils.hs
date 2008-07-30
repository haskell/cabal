module Distribution.Client.ParseUtils where

import Distribution.Compat.ReadP
         ( ReadP, readP_to_S, pfail, get, look, choice, (+++) )
import Distribution.Package (PackageIdentifier(..), Dependency(..))
import Distribution.ParseUtils 
         ( Field(..), FieldDescr(..), ParseResult(..), PError
         , field, liftField, readFields
         , warning, lineNo, locatedErrorMsg)
import Distribution.Text
         ( Text(parse) )
import Distribution.Version (Version(..), VersionRange(..))

import Control.Monad (foldM, liftM)
import Data.Char (isSpace, toLower)
import Data.Maybe (listToMaybe)
import Text.PrettyPrint.HughesPJ (Doc, render, vcat, text, (<>), (<+>))


showPError :: PError -> String
showPError err = let (ml,msg) = locatedErrorMsg err
                  in maybe "" (\l -> "On line " ++ show l ++ ": ") ml ++ msg



readPToMaybe :: ReadP a a -> String -> Maybe a
readPToMaybe p str = listToMaybe [ r | (r,s) <- readP_to_S p str, all isSpace s ]

ignoreWarnings :: ParseResult a -> ParseResult a
ignoreWarnings (ParseOk _ x) = ParseOk [] x
ignoreWarnings r = r 

parseBasicStanza :: [FieldDescr a] -> a -> String -> ParseResult a
parseBasicStanza fields empty inp = 
    readFields inp >>= foldM (setField fields) empty

setField :: [FieldDescr a]
         -> a
         -> Field
         -> ParseResult a
setField fs x (F line f val) =
    case lookupFieldDescr fs f of
      Nothing -> 
          do warning ("Unrecognized field " ++ f ++ " on line " ++ show line)
             return x
      Just (FieldDescr _ _ set) -> set line val x
setField _ x s = 
    do warning ("Unrecognized stanza on line " ++ show (lineNo s))
       return x

lookupFieldDescr :: [FieldDescr a] -> String -> Maybe (FieldDescr a)
lookupFieldDescr fs n = listToMaybe [f | f@(FieldDescr name _ _) <- fs, name == n]

boolField :: String -> (a -> Bool) -> (Bool -> a -> a) -> FieldDescr a
boolField name g s = liftField g s $ field name showBool readBool
  where
    showBool :: Bool -> Doc
    showBool True = text "true"
    showBool False = text "false"

    readBool :: ReadP r Bool
    readBool = choice [ stringNoCase "true"  >> return True
                      , stringNoCase "false" >> return False
                      , stringNoCase "yes"   >> return True
                      , stringNoCase "no"    >> return False]

showFields :: [FieldDescr a] -> a -> String
showFields fs x = render $ vcat [ text name <> text ":" <+> g x | FieldDescr name g _ <- fs]


stringNoCase :: String -> ReadP r String
stringNoCase this = look >>= scan this
 where
  scan []     _                               = return this
  scan (x:xs) (y:ys) | toLower x == toLower y = get >> scan xs ys
  scan _      _                               = pfail

parseDependencyOrPackageId :: ReadP r Dependency
parseDependencyOrPackageId = parse +++ liftM pkgToDep parse
  where pkgToDep p = case pkgVersion p of
          Version [] _ -> Dependency (pkgName p) AnyVersion
          version      -> Dependency (pkgName p) (ThisVersion version)
