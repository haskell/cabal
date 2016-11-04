{-# LANGUAGE OverloadedStrings #-}
module Distribution.Parsec.ConfVar (parseConditionConfVar) where

import           Prelude ()
import           Distribution.Compat.Prelude
import           Distribution.Compat.Parsec                   (integral)
import           Distribution.Parsec.Class                    (Parsec (..))
import           Distribution.Parsec.Types.Common
import           Distribution.Parsec.Types.Field              (SectionArg (..))
import           Distribution.Parsec.Types.ParseResult
import           Distribution.Simple.Utils                    (fromUTF8BS)
import           Distribution.Types.GenericPackageDescription
                 (Condition (..), ConfVar (..))
import           Distribution.Version
                 (anyVersion, earlierVersion, intersectVersionRanges,
                 laterVersion, majorBoundVersion, mkVersion, noVersion,
                 orEarlierVersion, orLaterVersion, thisVersion,
                 unionVersionRanges, withinVersion)
import qualified Text.Parsec                                  as P
import qualified Text.Parsec.Error                            as P

-- | Parse @'Condition' 'ConfVar'@ from section arguments provided by parsec
-- based outline parser.
parseConditionConfVar :: [SectionArg Position] -> ParseResult (Condition ConfVar)
parseConditionConfVar args = do
    -- preprocess glued operators
    args' <- preprocess args
    -- The name of the input file is irrelevant, as we reformat the error message.
    case P.runParser (parser <* P.eof) () "<condition>" args' of
        Right x  -> pure x
        Left err -> do
            -- Mangle the position to the actual one
            let ppos = P.errorPos err
            let epos = Position (P.sourceLine ppos) (P.sourceColumn ppos)
            let msg = P.showErrorMessages
                    "or" "unknown parse error" "expecting" "unexpected" "end of input"
                    (P.errorMessages err)
            parseFailure epos msg
            pure $ Lit True

-- This is a hack, as we have "broken" .cabal files on Hackage
--
-- There are glued operators "&&!" (no whitespace) in some cabal files.
-- E.g. http://hackage.haskell.org/package/hblas-0.2.0.0/hblas.cabal
preprocess :: [SectionArg Position] -> ParseResult [SectionArg Position]
preprocess (SecArgOther pos "&&!" : rest) = do
    parseWarning pos PWTGluedOperators "Glued operators: &&!"
    (\rest' -> SecArgOther pos "&&" : SecArgOther pos "!" : rest') <$> preprocess rest
preprocess (x : rest) =
    (x: ) <$> preprocess rest
preprocess [] = pure []

type Parser = P.Parsec [SectionArg Position] ()

parser :: Parser (Condition ConfVar)
parser = condOr
  where
    condOr       = P.sepBy1 condAnd (oper "||") >>= return . foldl1 COr
    condAnd      = P.sepBy1 cond    (oper "&&") >>= return . foldl1 CAnd
    cond         = P.choice
         [ boolLiteral, parens condOr,  notCond, osCond, archCond, flagCond, implCond ]

    notCond      = CNot <$ oper "!" <*> cond

    boolLiteral  = Lit <$> boolLiteral'
    osCond       = Var . OS   <$ string "os"   <*> parens fromParsec
    flagCond     = Var . Flag <$ string "flag" <*> parens fromParsec
    archCond     = Var . Arch <$ string "arch" <*> parens fromParsec
    implCond     = Var        <$ string "impl" <*> parens implCond'

    implCond'    = Impl
        <$> fromParsec
        <*> P.option anyVersion versionRange

    version = fromParsec
    versionStar  = mkVersion <$> fromParsec' versionStar' <* oper "*"
    versionStar' = some (integral <* P.char '.')

    versionRange = expr
      where
        expr = foldl1 unionVersionRanges     <$> P.sepBy1 term   (oper "||")
        term = foldl1 intersectVersionRanges <$> P.sepBy1 factor (oper "&&")

        factor = P.choice
            $ parens expr
            : parseAnyVersion
            : parseNoVersion
            : parseWildcardRange
            : map parseRangeOp rangeOps

        parseAnyVersion    = anyVersion <$ string "-any"
        parseNoVersion     = noVersion  <$ string "-none"

        parseWildcardRange = P.try $ withinVersion <$ oper "==" <*> versionStar

        parseRangeOp (s,f) = P.try (f <$ oper s <*> version)
        rangeOps = [ ("<",  earlierVersion),
                     ("<=", orEarlierVersion),
                     (">",  laterVersion),
                     (">=", orLaterVersion),
                     ("^>=", majorBoundVersion),
                     ("==", thisVersion) ]

    -- Number token can have many dots in it: SecArgNum (Position 65 15) "7.6.1"
    ident = tokenPrim $ \t -> case t of
        SecArgName _ s -> Just $ fromUTF8BS s
        SecArgNum  _ s -> Just $ fromUTF8BS s
        _              -> Nothing

    boolLiteral' = tokenPrim $ \t -> case t of
        SecArgName _ s
            | s == "True"  -> Just True
            | s == "true"  -> Just True
            | s == "False" -> Just False
            | s == "false" -> Just False
        _                  -> Nothing

    string s = tokenPrim $ \t -> case t of
        SecArgName _ s' | s == s' -> Just ()
        _                         -> Nothing

    oper o = tokenPrim $ \t -> case t of
        SecArgOther _ o' | o == o' -> Just ()
        _                          -> Nothing

    parens = P.between (oper "(") (oper ")")

    tokenPrim = P.tokenPrim prettySectionArg updatePosition
    -- TODO: check where the errors are reported
    updatePosition x _ _ = x
    prettySectionArg = show

    fromParsec :: Parsec a => Parser a
    fromParsec = fromParsec' parsec

    fromParsec' p = do
        i <- ident
        case P.runParser (p <* P.eof) [] "<ident>" i of
            Right x  -> pure x
            -- TODO: better lifting or errors / warnings
            Left err -> fail $ show err
