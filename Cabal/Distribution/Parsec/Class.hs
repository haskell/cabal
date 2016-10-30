{-# LANGUAGE FlexibleContexts #-}
module Distribution.Parsec.Class (
    Parsec(..),
    -- * Warnings
    parsecWarning,
    -- * Utilities
    parsecTestedWith,
    parsecPkgconfigDependency,
    parsecBuildTool,
    parsecToken,
    parsecToken',
    parsecFilePath,
    parsecQuoted,
    parsecMaybeQuoted,
    parsecCommaList,
    parsecOptCommaList,
    ) where

import           Prelude ()
import           Distribution.Compat.Prelude
import           Data.Functor.Identity                        (Identity)
import qualified Distribution.Compat.Parsec                   as P
import           Distribution.Parsec.Types.Common
                 (PWarnType (..), PWarning (..), Position (..))
import qualified Text.Parsec                                  as Parsec
import qualified Text.Parsec.Language                         as Parsec
import qualified Text.Parsec.Token                            as Parsec

-- Instances

import           Distribution.Compiler
                 (CompilerFlavor (..), classifyCompilerFlavor)
import           Distribution.License                         (License (..))
import           Distribution.ModuleName                      (ModuleName)
import qualified Distribution.ModuleName                      as ModuleName
import           Distribution.Package
                 (Dependency (..),
                 UnqualComponentName, mkUnqualComponentName,
                 PackageName, mkPackageName)
import           Distribution.System
                 (Arch (..), ClassificationStrictness (..), OS (..),
                 classifyArch, classifyOS)
import           Distribution.Text                            (display)
import           Distribution.Types.BenchmarkType
                 (BenchmarkType (..))
import           Distribution.Types.BuildType                 (BuildType (..))
import           Distribution.Types.GenericPackageDescription (FlagName, mkFlagName)
import           Distribution.Types.ModuleReexport
                 (ModuleReexport (..))
import           Distribution.Types.SourceRepo
                 (RepoKind, RepoType, classifyRepoKind, classifyRepoType)
import           Distribution.Types.TestType                  (TestType (..))
import           Distribution.Types.ForeignLibType            (ForeignLibType (..))
import           Distribution.Types.ForeignLibOption          (ForeignLibOption (..))
import           Distribution.Types.ModuleRenaming
import           Distribution.Types.IncludeRenaming
import           Distribution.Types.Mixin
import           Distribution.Version
                 (Version, VersionRange (..), anyVersion, earlierVersion,
                 intersectVersionRanges, laterVersion, majorBoundVersion,
                 mkVersion, noVersion, orEarlierVersion, orLaterVersion,
                 thisVersion, unionVersionRanges, withinVersion)
import           Language.Haskell.Extension
                 (Extension, Language, classifyExtension, classifyLanguage)

-------------------------------------------------------------------------------
-- Class
-------------------------------------------------------------------------------

-- |
--
-- TODO: implementation details: should be careful about consuming trailing whitespace?
-- Should we always consume it?
class Parsec a where
    parsec :: P.Stream s Identity Char => P.Parsec s [PWarning] a

    -- | 'parsec' /could/ consume trailing spaces, this function /must/ consume.
    lexemeParsec :: P.Stream s Identity Char => P.Parsec s [PWarning] a
    lexemeParsec = parsec <* P.spaces

parsecWarning :: PWarnType -> String -> P.Parsec s [PWarning] ()
parsecWarning t w =
    Parsec.modifyState (PWarning t (Position 0 0) w :)

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

-- TODO: use lexemeParsec

-- TODO avoid String
parsecUnqualComponentName :: P.Stream s Identity Char => P.Parsec s [PWarning] String
parsecUnqualComponentName = intercalate "-" <$> P.sepBy1 component (P.char '-')
  where
    component :: P.Stream s Identity Char => P.Parsec s [PWarning] String
    component = do
      cs <- P.munch1 isAlphaNum
      if all isDigit cs
        then fail "all digits in portion of unqualified component name"
        else return cs

instance Parsec UnqualComponentName where
  parsec = mkUnqualComponentName <$> parsecUnqualComponentName

instance Parsec PackageName where
  parsec = mkPackageName <$> parsecUnqualComponentName

instance Parsec ModuleName where
    parsec = ModuleName.fromComponents <$> P.sepBy1 component (P.char '.')
      where
        component = do
            c  <- P.satisfy isUpper
            cs <- P.munch validModuleChar
            return (c:cs)

        validModuleChar :: Char -> Bool
        validModuleChar c = isAlphaNum c || c == '_' || c == '\''

instance Parsec FlagName where
    parsec = mkFlagName . map toLower . intercalate "-" <$> P.sepBy1 component (P.char '-')
      where
        -- http://hackage.haskell.org/package/cabal-debian-4.24.8/cabal-debian.cabal
        -- has flag with all digit component: pretty-112
        component :: P.Stream s Identity Char => P.Parsec s [PWarning] String
        component = P.munch1 (\c -> isAlphaNum c || c `elem` "_")

instance Parsec Dependency where
    parsec = do
        name <- lexemeParsec
        ver  <- parsec <|> pure anyVersion
        return (Dependency name ver)

instance Parsec Version where
    parsec = mkVersion <$>
        P.sepBy1 P.integral (P.char '.')
        <* tags
      where
        tags = do
            ts <- P.optionMaybe $ some $ P.char '-' *> some (P.satisfy isAlphaNum)
            case ts of
                Nothing -> pure ()
                -- TODO: make this warning severe
                Just _  -> parsecWarning PWTVersionTag "version with tags"

-- TODO: this is not good parsec code
-- use lexer, also see D.P.ConfVar
instance Parsec VersionRange where
    parsec = expr
      where
        expr   = do P.spaces
                    t <- term
                    P.spaces
                    (do _  <- P.string "||"
                        P.spaces
                        e <- expr
                        return (unionVersionRanges t e)
                     <|>
                     return t)
        term   = do f <- factor
                    P.spaces
                    (do _  <- P.string "&&"
                        P.spaces
                        t <- term
                        return (intersectVersionRanges f t)
                     <|>
                     return f)
        factor = P.choice
            $ parens expr
            : parseAnyVersion
            : parseNoVersion
            : parseWildcardRange
            : map parseRangeOp rangeOps
        parseAnyVersion    = P.string "-any" >> return anyVersion
        parseNoVersion     = P.string "-none" >> return noVersion

        parseWildcardRange = P.try $ do
          _ <- P.string "=="
          P.spaces
          branch <- some (P.integral <* P.char '.')
          _ <- P.char '*'
          return (withinVersion (mkVersion branch))

        parens p = P.between
            (P.char '(' >> P.spaces)
            (P.char ')' >> P.spaces)
            (do a <- p
                P.spaces
                return (VersionRangeParens a))

        -- TODO: make those non back-tracking
        parseRangeOp (s,f) = P.try (P.string s *> P.spaces *> fmap f parsec)
        rangeOps = [ ("<",  earlierVersion),
                     ("<=", orEarlierVersion),
                     (">",  laterVersion),
                     (">=", orLaterVersion),
                     ("^>=", majorBoundVersion),
                     ("==", thisVersion) ]

instance Parsec Language where
    parsec = classifyLanguage <$> P.munch1 isAlphaNum

instance Parsec Extension where
    parsec = classifyExtension <$> P.munch1 isAlphaNum

instance Parsec RepoType where
    parsec = classifyRepoType <$> P.munch1 isIdent

instance Parsec RepoKind where
    parsec = classifyRepoKind <$> P.munch1 isIdent

instance Parsec License where
  parsec = do
    name    <- P.munch1 isAlphaNum
    version <- P.optionMaybe (P.char '-' *> parsec)
    return $! case (name, version :: Maybe Version) of
      ("GPL",               _      ) -> GPL  version
      ("LGPL",              _      ) -> LGPL version
      ("AGPL",              _      ) -> AGPL version
      ("BSD2",              Nothing) -> BSD2
      ("BSD3",              Nothing) -> BSD3
      ("BSD4",              Nothing) -> BSD4
      ("ISC",               Nothing) -> ISC
      ("MIT",               Nothing) -> MIT
      ("MPL",         Just version') -> MPL version'
      ("Apache",            _      ) -> Apache version
      ("PublicDomain",      Nothing) -> PublicDomain
      ("AllRightsReserved", Nothing) -> AllRightsReserved
      ("OtherLicense",      Nothing) -> OtherLicense
      _                              -> UnknownLicense $ name ++
                                        maybe "" (('-':) . display) version

instance Parsec BuildType where
  parsec = do
    name <- P.munch1 isAlphaNum
    return $ case name of
      "Simple"    -> Simple
      "Configure" -> Configure
      "Custom"    -> Custom
      "Make"      -> Make
      _           -> UnknownBuildType name

instance Parsec TestType where
  parsec = stdParse $ \ver name -> case name of
      "exitcode-stdio" -> TestTypeExe ver
      "detailed"       -> TestTypeLib ver
      _                -> TestTypeUnknown name ver

instance Parsec BenchmarkType where
    parsec = stdParse $ \ver name -> case name of
       "exitcode-stdio" -> BenchmarkTypeExe ver
       _                -> BenchmarkTypeUnknown name ver

instance Parsec ForeignLibType where
  parsec = do
    name <- P.munch1 (\c -> isAlphaNum c || c == '-')
    return $ case name of
      "native-shared" -> ForeignLibNativeShared
      "native-static" -> ForeignLibNativeStatic
      _               -> ForeignLibTypeUnknown

instance Parsec ForeignLibOption where
  parsec = do
    name <- P.munch1 (\c -> isAlphaNum c || c == '-')
    case name of
      "standalone" -> return ForeignLibStandalone
      _            -> fail "unrecognized foreign-library option"

instance Parsec OS where
    parsec = classifyOS Compat <$> parsecIdent

instance Parsec Arch where
    parsec = classifyArch Strict <$> parsecIdent

instance Parsec CompilerFlavor where
    parsec = classifyCompilerFlavor <$> component
      where
        component :: P.Stream s Identity Char => P.Parsec s [PWarning] String
        component = do
          cs <- P.munch1 isAlphaNum
          if all isDigit cs then fail "all digits compiler name" else return cs

instance Parsec ModuleReexport where
    parsec = do
        mpkgname <- P.optionMaybe (P.try $ parsec <* P.char ':')
        origname <- parsec
        newname  <- P.option origname $ P.try $ do
            P.spaces
            _ <- P.string "as"
            P.spaces
            parsec
        return (ModuleReexport mpkgname origname newname)

instance Parsec ModuleRenaming where
    -- NB: try not necessary as the first token is obvious
    parsec = P.choice [ parseRename, parseHiding, return DefaultRenaming ]
      where
        parseRename = do
            rns <- P.between (P.char '(') (P.char ')') parseList
            P.spaces
            return (ModuleRenaming rns)
        parseHiding = do
            _ <- P.string "hiding"
            P.spaces
            hides <- P.between (P.char '(') (P.char ')')
                        (P.sepBy parsec (P.char ',' >> P.spaces))
            return (HidingRenaming hides)
        parseList =
            P.sepBy parseEntry (P.char ',' >> P.spaces)
        parseEntry = do
            orig <- parsec
            P.spaces
            P.option (orig, orig) $ do
                _ <- P.string "as"
                P.spaces
                new <- parsec
                P.spaces
                return (orig, new)

instance Parsec IncludeRenaming where
    parsec = do
        prov_rn <- parsec
        req_rn <- P.option defaultRenaming $ P.try $ do
            P.spaces
            _ <- P.string "requires"
            P.spaces
            parsec
        return (IncludeRenaming prov_rn req_rn)

instance Parsec Mixin where
    parsec = do
        mod_name <- parsec
        P.spaces
        incl <- parsec
        return (Mixin mod_name incl)

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

isIdent :: Char -> Bool
isIdent c = isAlphaNum c || c == '_' || c == '-'

parsecTestedWith :: P.Stream s Identity Char => P.Parsec s [PWarning] (CompilerFlavor, VersionRange)
parsecTestedWith = do
    name <- lexemeParsec
    ver  <- parsec <|> pure anyVersion
    return (name, ver)

parsecPkgconfigDependency :: P.Stream s Identity Char => P.Parsec s [PWarning] Dependency
parsecPkgconfigDependency = do
    name <- P.munch1 (\c -> isAlphaNum c || c `elem` "+-._")
    P.spaces
    verRange <- parsec <|> pure anyVersion
    pure $ Dependency (mkPackageName name) verRange

parsecBuildTool :: P.Stream s Identity Char => P.Parsec s [PWarning] Dependency
parsecBuildTool = do
    name <- parsecMaybeQuoted nameP
    P.spaces
    verRange <- parsecMaybeQuoted parsec <|> pure anyVersion
    pure $ Dependency (mkPackageName name) verRange
  where
    nameP = intercalate "-" <$> P.sepBy1 component (P.char '-')
    component = do
        cs <- P.munch1 (\c -> isAlphaNum c || c == '+' || c == '_')
        if all isDigit cs then fail "invalid component" else return cs

parsecToken :: P.Stream s Identity Char => P.Parsec s [PWarning] String
parsecToken = parsecHaskellString <|> (P.munch1 (\x -> not (isSpace x) && x /= ',')  P.<?> "identifier" )

parsecToken' :: P.Stream s Identity Char => P.Parsec s [PWarning] String
parsecToken' = parsecHaskellString <|> (P.munch1 (not . isSpace) P.<?> "token")

parsecFilePath :: P.Stream s Identity Char => P.Parsec s [PWarning] String
parsecFilePath = parsecToken

-- | Parse a benchmark/test-suite types.
stdParse
    :: P.Stream s Identity Char
    => (Version -> String -> a)
    -> P.Parsec s [PWarning] a
stdParse f = do
    -- TODO: this backtracks
    cs   <- some $ P.try (component <* P.char '-')
    ver  <- parsec
    let name = map toLower (intercalate "-" cs)
    return $! f ver name
  where
    component = do
      cs <- P.munch1 isAlphaNum
      if all isDigit cs then fail "all digit component" else return cs
      -- each component must contain an alphabetic character, to avoid
      -- ambiguity in identifiers like foo-1 (the 1 is the version number).

parsecCommaList
    :: P.Stream s Identity Char
    => P.Parsec s [PWarning] a
    -> P.Parsec s [PWarning] [a]
parsecCommaList p = P.sepBy (p <* P.spaces) (P.char ',' *> P.spaces)

parsecOptCommaList
    :: P.Stream s Identity Char
    => P.Parsec s [PWarning] a
    -> P.Parsec s [PWarning] [a]
parsecOptCommaList p = P.sepBy (p <* P.spaces) (P.optional comma)
  where
    comma = P.char ',' *>  P.spaces


-- | Content isn't unquoted
parsecQuoted
     :: P.Stream s Identity Char
     => P.Parsec s [PWarning] a
     -> P.Parsec s [PWarning] a
parsecQuoted = P.between (P.char '"') (P.char '"')

-- | @parsecMaybeQuoted p = 'parsecQuoted' p <|> p@.
parsecMaybeQuoted
     :: P.Stream s Identity Char
     => P.Parsec s [PWarning] a
     -> P.Parsec s [PWarning] a
parsecMaybeQuoted p = parsecQuoted p <|> p

parsecHaskellString :: P.Stream s Identity Char => P.Parsec s [PWarning] String
parsecHaskellString = Parsec.stringLiteral $ Parsec.makeTokenParser Parsec.emptyDef
    { Parsec.commentStart   = "{-"
    , Parsec.commentEnd     = "-}"
    , Parsec.commentLine    = "--"
    , Parsec.nestedComments = True
    , Parsec.identStart     = P.satisfy isAlphaNum
    , Parsec.identLetter    = P.satisfy isAlphaNum <|> P.oneOf "_'"
    , Parsec.opStart        = opl
    , Parsec.opLetter       = opl
    , Parsec.reservedOpNames= []
    , Parsec.reservedNames  = []
    , Parsec.caseSensitive  = True
    }
  where
    opl = P.oneOf ":!#$%&*+./<=>?@\\^|-~"

parsecIdent :: P.Stream s Identity Char => P.Parsec s [PWarning] String
parsecIdent = (:) <$> firstChar <*> rest
  where
    firstChar = P.satisfy isAlpha
    rest      = P.munch (\c -> isAlphaNum c || c == '_' || c == '-')
