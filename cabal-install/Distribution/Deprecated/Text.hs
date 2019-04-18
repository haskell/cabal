{-# LANGUAGE DefaultSignatures #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Deprecated.Text
-- Copyright   :  Duncan Coutts 2007
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- classes. The difference is that it uses a modern pretty printer and parser
-- system and the format is not expected to be Haskell concrete syntax but
-- rather the external human readable representation used by Cabal.
--
module Distribution.Deprecated.Text (
  Text(..),
  defaultStyle,
  display,
  flatStyle,
  simpleParse,
  stdParse,
  -- parse utils
  parsePackageName,
  ) where

import Distribution.Client.Compat.Prelude
import Prelude (read)

import           Distribution.Deprecated.ReadP ((<++))
import qualified Distribution.Deprecated.ReadP as Parse

import           Data.Functor.Identity     (Identity (..))
import           Distribution.Parsec
import           Distribution.Pretty
import qualified Text.PrettyPrint          as Disp

import qualified Data.Set as Set

import Data.Version (Version (Version))

import qualified Distribution.Compiler                       as D
import qualified Distribution.License                        as D
import qualified Distribution.ModuleName                     as D
import qualified Distribution.Package                        as D
import qualified Distribution.PackageDescription             as D
import qualified Distribution.Simple.Setup                   as D
import qualified Distribution.System                         as D
import qualified Distribution.Types.PackageVersionConstraint as D
import qualified Distribution.Types.SourceRepo               as D
import qualified Distribution.Types.UnqualComponentName      as D
import qualified Distribution.Version                        as D
import qualified Language.Haskell.Extension                  as E

-- | /Note:/ this class will soon be deprecated.
-- It's not yet, so that we are @-Wall@ clean.
class Text a where
  disp  :: a -> Disp.Doc
  default disp :: Pretty a => a -> Disp.Doc
  disp = pretty

  parse :: Parse.ReadP r a
  default parse :: Parsec a => Parse.ReadP r a
  parse = parsec

-- | Pretty-prints with the default style.
display :: Text a => a -> String
display = Disp.renderStyle defaultStyle . disp

simpleParse :: Text a => String -> Maybe a
simpleParse str = case [ p | (p, s) <- Parse.readP_to_S parse str
                       , all isSpace s ] of
  []    -> Nothing
  (p:_) -> Just p

stdParse :: Text ver => (ver -> String -> res) -> Parse.ReadP r res
stdParse f = do
  cs   <- Parse.sepBy1 component (Parse.char '-')
  _    <- Parse.char '-'
  ver  <- parse
  let name = intercalate "-" cs
  return $! f ver (lowercase name)
  where
    component = do
      cs <- Parse.munch1 isAlphaNum
      if all isDigit cs then Parse.pfail else return cs
      -- each component must contain an alphabetic character, to avoid
      -- ambiguity in identifiers like foo-1 (the 1 is the version number).

lowercase :: String -> String
lowercase = map toLower

-- -----------------------------------------------------------------------------
-- Instances for types from the base package

instance Text Bool where
  parse = Parse.choice [ (Parse.string "True" Parse.+++
                          Parse.string "true") >> return True
                       , (Parse.string "False" Parse.+++
                          Parse.string "false") >> return False ]

instance Text Int where
  parse = fmap negate (Parse.char '-' >> parseNat) Parse.+++ parseNat

instance Text a => Text (Identity a) where
    disp = disp . runIdentity
    parse = fmap Identity parse

-- | Parser for non-negative integers.
parseNat :: Parse.ReadP r Int
parseNat = read `fmap` Parse.munch1 isDigit -- TODO: eradicateNoParse


instance Text Version where
  disp (Version branch _tags)     -- Death to version tags!!
    = Disp.hcat (Disp.punctuate (Disp.char '.') (map Disp.int branch))

  parse = do
      branch <- Parse.sepBy1 parseNat (Parse.char '.')
                -- allow but ignore tags:
      _tags  <- Parse.many (Parse.char '-' >> Parse.munch1 isAlphaNum)
      return (Version branch [])

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

instance Text D.Arch where
  parse = fmap (D.classifyArch D.Strict) ident

instance Text D.BuildType where
  parse = do
    name <- Parse.munch1 isAlphaNum
    case name of
      "Simple"    -> return D.Simple
      "Configure" -> return D.Configure
      "Custom"    -> return D.Custom
      "Make"      -> return D.Make
      "Default"   -> return D.Custom
      _           -> fail ("unknown build-type: '" ++ name ++ "'")

instance Text D.CompilerFlavor where
  parse = do
    comp <- Parse.munch1 isAlphaNum
    when (all isDigit comp) Parse.pfail
    return (D.classifyCompilerFlavor comp)

instance Text D.CompilerId where
  parse = do
    flavour <- parse
    version <- (Parse.char '-' >> parse) Parse.<++ return D.nullVersion
    return (D.CompilerId flavour version)

instance Text D.ComponentId where
  parse = D.mkComponentId `fmap` Parse.munch1 abi_char
   where abi_char c = isAlphaNum c || c `elem` "-_."

instance Text D.DefUnitId where
  parse = D.unsafeMkDefUnitId `fmap` parse

instance Text D.UnitId where
    parse = D.mkUnitId <$> Parse.munch1 (\c -> isAlphaNum c || c `elem` "-_.+")

instance Text D.Dependency where
  parse = do name <- parse
             Parse.skipSpaces
             libs <- Parse.option [D.LMainLibName]
                   $ (Parse.char ':' *>)
                   $ pure <$> parseLib name <|> parseMultipleLibs name
             Parse.skipSpaces
             ver <- parse Parse.<++ return D.anyVersion
             Parse.skipSpaces
             return $ D.Dependency name ver $ Set.fromList libs
    where makeLib pn ln | D.unPackageName pn == ln = D.LMainLibName
                        | otherwise = D.LSubLibName $ D.mkUnqualComponentName ln
          parseLib pn = makeLib pn <$> parsecUnqualComponentName
          parseMultipleLibs pn = Parse.between (Parse.char '{' *> Parse.skipSpaces)
                                         (Parse.skipSpaces <* Parse.char '}')
                                         $ parsecCommaList $ parseLib pn


instance Text E.Extension where
  parse = do
    extension <- Parse.munch1 isAlphaNum
    return (E.classifyExtension extension)

instance Text D.FlagName where
    -- Note:  we don't check that FlagName doesn't have leading dash,
    -- cabal check will do that.
    parse = D.mkFlagName . lowercase <$> parse'
      where
        parse' = (:) <$> lead <*> rest
        lead = Parse.satisfy (\c ->  isAlphaNum c || c == '_')
        rest = Parse.munch (\c -> isAlphaNum c ||  c == '_' || c == '-')

instance Text D.HaddockTarget where
    parse = Parse.choice [ Parse.string "for-hackage"     >> return D.ForHackage
                         , Parse.string "for-development" >> return D.ForDevelopment]

instance Text E.Language where
  parse = do
    lang <- Parse.munch1 isAlphaNum
    return (E.classifyLanguage lang)

instance Text D.License where
  parse = do
    name    <- Parse.munch1 (\c -> isAlphaNum c && c /= '-')
    version <- Parse.option Nothing (Parse.char '-' >> fmap Just parse)
    return $! case (name, version :: Maybe D.Version) of
      ("GPL",               _      ) -> D.GPL  version
      ("LGPL",              _      ) -> D.LGPL version
      ("AGPL",              _      ) -> D.AGPL version
      ("BSD2",              Nothing) -> D.BSD2
      ("BSD3",              Nothing) -> D.BSD3
      ("BSD4",              Nothing) -> D.BSD4
      ("ISC",               Nothing) -> D.ISC
      ("MIT",               Nothing) -> D.MIT
      ("MPL",         Just version') -> D.MPL version'
      ("Apache",            _      ) -> D.Apache version
      ("PublicDomain",      Nothing) -> D.PublicDomain
      ("AllRightsReserved", Nothing) -> D.AllRightsReserved
      ("OtherLicense",      Nothing) -> D.OtherLicense
      _                              -> D.UnknownLicense $ name ++
                                        maybe "" (('-':) . display) version

instance Text D.Module where
    parse = do
        uid <- parse
        _ <- Parse.char ':'
        mod_name <- parse
        return (D.Module uid mod_name)

instance Text D.ModuleName where
  parse = do
    ms <- Parse.sepBy1 component (Parse.char '.')
    return (D.fromComponents ms)

    where
      component = do
        c  <- Parse.satisfy isUpper
        cs <- Parse.munch validModuleChar
        return (c:cs)

instance Text D.OS where
  parse = fmap (D.classifyOS D.Compat) ident

instance Text D.PackageVersionConstraint where
  parse = do name <- parse
             Parse.skipSpaces
             ver <- parse Parse.<++ return D.anyVersion
             Parse.skipSpaces
             return (D.PackageVersionConstraint name ver)

instance Text D.PkgconfigName where
  parse = D.mkPkgconfigName
          <$> Parse.munch1 (\c -> isAlphaNum c || c `elem` "+-._")

instance Text D.Platform where
  -- TODO: there are ambigious platforms like: `arch-word-os`
  -- which could be parsed as
  --   * Platform "arch-word" "os"
  --   * Platform "arch" "word-os"
  -- We could support that preferring variants 'OtherOS' or 'OtherArch'
  --
  -- For now we split into arch and os parts on the first dash.
  parse = do
    arch <- parseDashlessArch
    _ <- Parse.char '-'
    os   <- parse
    return (D.Platform arch os)
      where
        parseDashlessArch :: Parse.ReadP r D.Arch
        parseDashlessArch = fmap (D.classifyArch D.Strict) dashlessIdent

        dashlessIdent :: Parse.ReadP r String
        dashlessIdent = liftM2 (:) firstChar rest
          where firstChar = Parse.satisfy isAlpha
                rest = Parse.munch (\c -> isAlphaNum c || c == '_')

instance Text D.RepoKind where
  parse = fmap D.classifyRepoKind ident

instance Text D.RepoType where
  parse = fmap D.classifyRepoType ident

instance Text D.UnqualComponentName where
  parse = D.mkUnqualComponentName <$> parsePackageName

instance Text D.PackageIdentifier where
  parse = do
    n <- parse
    v <- (Parse.char '-' >> parse) <++ return D.nullVersion
    return (D.PackageIdentifier n v)

instance Text D.PackageName where
  parse = D.mkPackageName <$> parsePackageName

instance Text D.Version where
  parse = do
      branch <- Parse.sepBy1 parseNat (Parse.char '.')
                -- allow but ignore tags:
      _tags  <- Parse.many (Parse.char '-' >> Parse.munch1 isAlphaNum)
      return (D.mkVersion branch)

instance Text D.VersionRange where
  parse = expr
   where
        expr   = do Parse.skipSpaces
                    t <- term
                    Parse.skipSpaces
                    (do _  <- Parse.string "||"
                        Parse.skipSpaces
                        e <- expr
                        return (D.unionVersionRanges t e)
                     Parse.+++
                     return t)
        term   = do f <- factor
                    Parse.skipSpaces
                    (do _  <- Parse.string "&&"
                        Parse.skipSpaces
                        t <- term
                        return (D.intersectVersionRanges f t)
                     Parse.+++
                     return f)
        factor = Parse.choice $ parens expr
                              : parseAnyVersion
                              : parseNoVersion
                              : parseWildcardRange
                              : map parseRangeOp rangeOps
        parseAnyVersion    = Parse.string "-any" >> return D.anyVersion
        parseNoVersion     = Parse.string "-none" >> return D.noVersion

        parseWildcardRange = do
          _ <- Parse.string "=="
          Parse.skipSpaces
          branch <- Parse.sepBy1 digits (Parse.char '.')
          _ <- Parse.char '.'
          _ <- Parse.char '*'
          return (D.withinVersion (D.mkVersion branch))

        parens p = Parse.between (Parse.char '(' >> Parse.skipSpaces)
                                 (Parse.char ')' >> Parse.skipSpaces)
                                 (do a <- p
                                     Parse.skipSpaces
                                     return (D.VersionRangeParens a))
        digits = do
          firstDigit <- Parse.satisfy isDigit
          if firstDigit == '0'
            then return 0
            else do rest <- Parse.munch isDigit
                    return (read (firstDigit : rest)) -- TODO: eradicateNoParse

        parseRangeOp (s,f) = Parse.string s >> Parse.skipSpaces >> fmap f parse
        rangeOps = [ ("<",  D.earlierVersion),
                     ("<=", D.orEarlierVersion),
                     (">",  D.laterVersion),
                     (">=", D.orLaterVersion),
                     ("^>=", D.majorBoundVersion),
                     ("==", D.thisVersion) ]

-------------------------------------------------------------------------------
-- ParseUtils
-------------------------------------------------------------------------------

parsePackageName :: Parse.ReadP r String
parsePackageName = do
  ns <- Parse.sepBy1 component (Parse.char '-')
  return $ intercalate "-" ns
  where
    component = do
      cs <- Parse.munch1 isAlphaNum
      if all isDigit cs then Parse.pfail else return cs
      -- each component must contain an alphabetic character, to avoid
      -- ambiguity in identifiers like foo-1 (the 1 is the version number).


ident :: Parse.ReadP r String
ident = liftM2 (:) firstChar rest
  where firstChar = Parse.satisfy isAlpha
        rest = Parse.munch (\c -> isAlphaNum c || c == '_' || c == '-')

validModuleChar :: Char -> Bool
validModuleChar c = isAlphaNum c || c == '_' || c == '\''

-------------------------------------------------------------------------------
-- Rest of instances, we don't seem to need
-------------------------------------------------------------------------------

-- instance Text D.AbiDependency
-- instance Text D.AbiHash
-- instance Text D.AbiTa
-- instance Text D.BenchmarkType
-- instance Text D.ExecutableScope
-- instance Text D.ExeDependency
-- instance Text D.ExposedModule
-- instance Text D.ForeignLibOption
-- instance Text D.ForeignLibType
-- instance Text D.IncludeRenaming
-- instance Text D.KnownExtension
-- instance Text D.LegacyExeDependency
-- instance Text D.LibVersionInfo
-- instance Text D.License
-- instance Text D.Mixin
-- instance Text D.ModuleReexport
-- instance Text D.ModuleRenaming
-- instance Text D.MungedPackageName
-- instance Text D.OpenModule
-- instance Text D.OpenUnitId
-- instance Text D.PackageVersionConstraint
-- instance Text D.PkgconfigDependency
-- instance Text D.TestType
