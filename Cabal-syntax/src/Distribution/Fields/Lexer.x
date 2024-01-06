{
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Fields.Lexer
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Lexer for the cabal files.
{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
#ifdef CABAL_PARSEC_DEBUG
{-# LANGUAGE PatternGuards #-}
#endif
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Distribution.Fields.Lexer
  (ltest, lexToken, Token(..), LToken(..)
  ,bol_section, in_section, in_field_layout, in_field_braces
  ,mkLexState) where

import Prelude ()
import qualified Prelude as Prelude
import Distribution.Compat.Prelude

import Distribution.Fields.LexerMonad
import Distribution.Parsec.Position (Position (..), incPos, retPos)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B.Char8
import qualified Data.Word as Word

#ifdef CABAL_PARSEC_DEBUG
import Debug.Trace
import qualified Data.Vector as V
import qualified Data.Text   as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
#endif

}
-- Various character classes

%encoding "latin1"

$space           = \          -- single space char
$ctlchar         = [\x0-\x1f \x7f]
$printable       = \x0-\xff # $ctlchar   -- so no \n \r
$symbol'         = [ \, \= \< \> \+ \* \& \| \! \$ \% \^ \@ \# \? \/ \\ \~ ]
$symbol          = [$symbol' \- \.]
$spacetab        = [$space \t]

$paren           = [ \( \) \[ \] ]
$field_layout    = [$printable \t]
$field_layout'   = [$printable] # [$space]
$field_braces    = [$printable \t] # [\{ \}]
$field_braces'   = [$printable] # [\{ \} $space]
$comment         = [$printable \t]
$namecore        = [$printable] # [$space \: \" \{ \} $paren $symbol']
$instr           = [$printable $space] # [\"]
$instresc        = $printable

@bom          = \xef \xbb \xbf
@nbsp         = \xc2 \xa0
@nbspspacetab = ($spacetab | @nbsp)
@nbspspace    = ($space | @nbsp)
@nl           = \n | \r\n | \r
@name         = $namecore+
@string       = \" ( $instr | \\ $instresc )* \"
@oplike       = $symbol+


tokens :-

<0> {
  @bom?  { \pos len _ -> do
              when (len /= 0) $ addWarningAt pos LexWarningBOM
              setPos pos -- reset position as if BOM didn't exist
              setStartCode bol_section
              lexToken
         }
}

<bol_section, bol_field_layout, bol_field_braces> {
  @nbspspacetab* @nl         { \pos len inp -> checkWhitespace pos len inp >> adjustPos retPos >> lexToken }
  -- no @nl here to allow for comments on last line of the file with no trailing \n
  $spacetab* "--" $comment*  ;  -- TODO: check the lack of @nl works here
                                -- including counting line numbers
}

<bol_section> {
  @nbspspacetab*   { \pos len inp -> checkLeadingWhitespace pos len inp >>= \len' ->
                                     -- len' is character whitespace length (counting nbsp as one)
                                     if B.length inp == len
                                       then return (L pos EOF)
                                       else do
                                        -- Small hack: if char and byte length mismatch
                                        -- subtract the difference, so lexToken will count position correctly.
                                        -- Proper (and slower) fix is to count utf8 length in lexToken
                                        when (len' /= len) $ adjustPos (incPos (len' - len))
                                        setStartCode in_section
                                        return (L pos (Indent len')) }
  $spacetab* \{    { tok  OpenBrace }
  $spacetab* \}    { tok  CloseBrace }
}

<in_section> {
  $spacetab+   ; --TODO: don't allow tab as leading space

  "--" $comment* ;

  @name        { toki TokSym }
  @string      { \pos len inp -> return $! L pos (TokStr (B.take (len - 2) (B.tail inp))) }
  @oplike      { toki TokOther }
  $paren       { toki TokOther }
  \:           { tok  Colon }
  \{           { tok  OpenBrace }
  \}           { tok  CloseBrace }
  @nl          { \_ _ _ -> adjustPos retPos >> setStartCode bol_section >> lexToken }
}

<bol_field_layout> {
  @nbspspacetab* { \pos len inp -> checkLeadingWhitespace pos len inp >>= \len' ->
                                  if B.length inp == len
                                    then return (L pos EOF)
                                    else do
                                      -- Small hack: if char and byte length mismatch
                                      -- subtract the difference, so lexToken will count position correctly.
                                      -- Proper (and slower) fix is to count utf8 length in lexToken
                                      when (len' /= len) $ adjustPos (incPos (len' - len))
                                      setStartCode in_field_layout
                                      return (L pos (Indent len')) }
}

<in_field_layout> {
  $spacetab+;
  $field_layout' $field_layout*  { toki TokFieldLine }
  @nl             { \_ _ _ -> adjustPos retPos >> setStartCode bol_field_layout >> lexToken }
}

<bol_field_braces> {
   ()                { \_ _ _ -> setStartCode in_field_braces >> lexToken }
}

<in_field_braces> {
  $spacetab+;
  $field_braces' $field_braces*    { toki TokFieldLine }
  \{                { tok  OpenBrace  }
  \}                { tok  CloseBrace }
  @nl               { \_ _ _ -> adjustPos retPos >> setStartCode bol_field_braces >> lexToken }
}

{

-- | Tokens of outer cabal file structure. Field values are treated opaquely.
data Token = TokSym   !ByteString       -- ^ Haskell-like identifier, number or operator
           | TokStr   !ByteString       -- ^ String in quotes
           | TokOther !ByteString       -- ^ Operators and parens
           | Indent   !Int              -- ^ Indentation token
           | TokFieldLine !ByteString   -- ^ Lines after @:@
           | Colon
           | OpenBrace
           | CloseBrace
           | EOF
           | LexicalError InputStream --TODO: add separate string lexical error
  deriving Show

data LToken = L !Position !Token
  deriving Show

toki :: (ByteString -> Token) -> Position -> Int -> ByteString -> Lex LToken
toki t pos  len  input = return $! L pos (t (B.take len input))

tok :: Token -> Position -> Int -> ByteString -> Lex LToken
tok  t pos _len _input = return $! L pos t

checkLeadingWhitespace :: Position -> Int -> ByteString -> Lex Int
checkLeadingWhitespace pos len bs
    | B.any (== 9) (B.take len bs) = do
        addWarningAt pos LexWarningTab
        checkWhitespace pos len bs
    | otherwise = checkWhitespace pos len bs

checkWhitespace :: Position -> Int -> ByteString -> Lex Int
checkWhitespace pos len bs
    -- UTF8 NBSP is 194 160. This function is called on whitespace bytestrings,
    -- therefore counting 194 bytes is enough to count non-breaking spaces.
    -- We subtract the amount of 194 bytes to convert bytes length into char length
    | B.any (== 194) (B.take len bs) = do
        addWarningAt pos LexWarningNBSP
        return $ len - B.count 194 (B.take len bs)
    | otherwise = return len

-- -----------------------------------------------------------------------------
-- The input type

type AlexInput = InputStream

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar _ = error "alexInputPrevChar not used"

alexGetByte :: AlexInput -> Maybe (Word.Word8,AlexInput)
alexGetByte = B.uncons

lexicalError :: Position -> InputStream -> Lex LToken
lexicalError pos inp = do
  setInput B.empty
  return $! L pos (LexicalError inp)

lexToken :: Lex LToken
lexToken = do
  pos <- getPos
  inp <- getInput
  st  <- getStartCode
  case alexScan inp st of
    AlexEOF -> return (L pos EOF)
    AlexError inp' ->
        let !len_bytes = B.length inp - B.length inp' in
            --FIXME: we want len_chars here really
            -- need to decode utf8 up to this point
        lexicalError (incPos len_bytes pos) inp'
    AlexSkip  inp' len_chars -> do
        checkPosition pos inp inp' len_chars
        adjustPos (incPos len_chars)
        setInput inp'
        lexToken
    AlexToken inp' len_chars action -> do
        checkPosition pos inp inp' len_chars
        adjustPos (incPos len_chars)
        setInput inp'
        let !len_bytes = B.length inp - B.length inp'
        t <- action pos len_bytes inp
        --traceShow t $ return tok
        return t


checkPosition :: Position -> ByteString -> ByteString -> Int -> Lex ()
#ifdef CABAL_PARSEC_DEBUG
checkPosition pos@(Position lineno colno) inp inp' len_chars = do
    text_lines <- getDbgText
    let len_bytes = B.length inp - B.length inp'
        pos_txt   | lineno-1 < V.length text_lines = T.take len_chars (T.drop (colno-1) (text_lines V.! (lineno-1)))
                  | otherwise = T.empty
        real_txt  = B.take len_bytes inp
    when (pos_txt /= T.decodeUtf8 real_txt) $
      traceShow (pos, pos_txt, T.decodeUtf8 real_txt) $
      traceShow (take 3 (V.toList text_lines)) $ return ()
  where
    getDbgText = Lex $ \s@LexState{ dbgText = txt } -> LexResult s txt
#else
checkPosition _ _ _ _ = return ()
#endif

lexAll :: Lex [LToken]
lexAll = do
  t <- lexToken
  case t of
    L _ EOF -> return [t]
    _       -> do ts <- lexAll
                  return (t : ts)

ltest :: Int -> String -> Prelude.IO ()
ltest code s =
  let (ws, xs) = execLexer (setStartCode code >> lexAll) (B.Char8.pack s)
   in traverse_ print ws >> traverse_ print xs


mkLexState :: ByteString -> LexState
mkLexState input = LexState
  { curPos   = Position 1 1
  , curInput = input
  , curCode  = 0
  , warnings = []
#ifdef CABAL_PARSEC_DEBUG
  , dbgText  = V.fromList . lines' . T.decodeUtf8With T.lenientDecode $ input
#endif
  }

#ifdef CABAL_PARSEC_DEBUG
lines' :: T.Text -> [T.Text]
lines' s1
  | T.null s1 = []
  | otherwise = case T.break (\c -> c == '\r' || c == '\n') s1 of
                  (l, s2) | Just (c,s3) <- T.uncons s2
                         -> case T.uncons s3 of
                              Just ('\n', s4) | c == '\r' -> l `T.snoc` '\r' `T.snoc` '\n' : lines' s4
                              _                           -> l `T.snoc` c : lines' s3

                          | otherwise
                         -> [l]
#endif
}
