{
{-# LANGUAGE BangPatterns #-}
module Lexer (ltest, lexToken, Token(..), LToken(..)
              ,bol_section, in_section, in_field_layout, in_field_braces
              ,mkLexState) where

import LexerMonad
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B.Char8
import Data.Word (Word8)
import qualified Data.Char
import Data.Char (chr, ord)
import Data.List (stripPrefix)
--import Distribution.Simple.Utils (fromUTF8) --don't decode, keep as ByteString

-- testing only:
import Debug.Trace
import Control.Exception (assert)
import qualified Data.Vector as V
import qualified Data.Text   as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
}

-- Various character classes

$space           = \          -- single space char
$digit           = 0-9        -- digits
$alpha           = [a-z A-Z]  -- alphabetic characters
$symbol          = [\= \< \> \+ \* \- \& \| \! \$ \% \^ \@ \# \? \/ \\ \~]
$ctlchar         = [\x0-\x1f \x7f]
$printable       = \x0-\x10ffff # $ctlchar   -- so no \n \r
$spacetab        = [$space \t]
$bom             = \xfeff

$paren           = [ \( \) \[ \] ]
$field_layout    = [$printable \t]
$field_braces    = [$printable \t] # [\{ \}]
$comment         = [$printable \t]
$namecore        = [$alpha]
$nameextra       = [$namecore $digit \- \_ \. \']
$instr           = [$printable $space] # [\"]
$instresc        = $printable

@nl          = \n | \r\n | \r
@name        = $nameextra* $namecore $nameextra*
@string      = \" ( $instr | \\ $instresc )* \"
@numlike     = $digit [$digit \.]*
@oplike      = [ \, \. \= \< \> \+ \* \- \& \| \! \$ \% \^ \@ \# \? \/ \\ \~ ]+

tokens :-

<0> {
  $bom   ;
  ()     ;
}

<bol_section, bol_field_layout, bol_field_braces> {
  $spacetab* @nl                        { \_ _ _ -> adjustPos retPos >> lexToken }
  -- no @nl here to allow for comments on last line of the file with no trailing \n
  $spacetab* "--" $comment*             ;  -- TODO: check the lack of @nl works here
                                        -- including counting line numbers
}

<bol_section> {
  $spacetab*  --TODO prevent or record leading tabs
                   { \pos len inp -> if B.length inp == len
                                       then return (L pos EOF)
                                       else setStartCode in_section
                                         >> return (L pos (Indent len)) }
  $spacetab* \{    { tok  OpenBrace }
  $spacetab* \}    { tok  CloseBrace }
}

<in_section> {
  $spacetab+   ; --TODO: don't allow tab as leading space

  "--" $comment* ;

  @name        { toki TokSym }
  @string      { \p l i -> case reads (B.Char8.unpack (B.take l i)) of
                             [(str,[])] -> return (L p (TokStr str))
                             _          -> lexicalError p i }
  @numlike     { toki TokNum }
  @oplike      { toki TokOther }
  $paren       { toki TokOther }
  \:           { tok  Colon }
  \{           { tok  OpenBrace }
  \}           { tok  CloseBrace }
  @nl          { \_ _ _ -> adjustPos retPos >> setStartCode bol_section >> lexToken }
}

<bol_field_layout> {
  $spacetab*   --TODO prevent or record leading tabs
                { \pos len inp -> if B.length inp == len
                                    then return (L pos EOF)
                                    else setStartCode in_field_layout
                                      >> return (L pos (Indent len)) }
}

<in_field_layout> {
  $field_layout+  { toki TokFieldLine }  --TODO prevent or record leading tabs
  @nl             { \_ _ _ -> adjustPos retPos >> setStartCode bol_field_layout >> lexToken }
}

<bol_field_braces> {
   ()                { \_ _ _ -> setStartCode in_field_braces >> lexToken }
}

<in_field_braces> {
  $field_braces+    { toki TokFieldLine }
  \{                { tok  OpenBrace  }
  \}                { tok  CloseBrace }
  @nl               { \_ _ _ -> adjustPos retPos >> setStartCode bol_field_braces >> lexToken }
}

{

data Token = TokSym   !ByteString
           | TokStr   !String
           | TokNum   !ByteString
           | TokOther !ByteString
           | Indent   !Int
           | TokFieldLine !ByteString
           | Colon
           | OpenBrace
           | CloseBrace
           | EOF
           | LexicalError InputStream --TODO: add separate string lexical error
  deriving Show

data LToken = L !Position !Token
  deriving Show

toki t pos len input = return $! L pos (t (B.take len input))
tokl t pos len input = return $! L pos (t len)
tok  t pos len input = return $! L pos t

-- -----------------------------------------------------------------------------
-- The input type

type AlexInput = InputStream

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar _ = error "alexInputPrevChar not used"

alexGetByte :: AlexInput -> Maybe (Word8,AlexInput)
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
        tok <- action pos len_bytes inp
        --traceShow tok $ return tok
        return tok

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

lexAll :: Lex [LToken]
lexAll = do
  t <- lexToken
  case t of
    L _ EOF -> return [t]
    _       -> do ts <- lexAll
                  return (t : ts)

ltest :: Int -> String -> IO ()
ltest code s =
  let xs = execLexer (setStartCode code >> lexAll) (B.Char8.pack s)
   in mapM_ print xs


mkLexState :: B.ByteString -> LexState
mkLexState input = LexState {
                     curPos   = Position 1 1,
                     curInput = input,
                     curCode  = bol_section,
                     dbgText  = V.fromList . lines' . T.decodeUtf8With T.lenientDecode $ input
                   }

lines' s1
  | T.null s1 = []
  | otherwise = case T.break (\c -> c == '\r' || c == '\n') s1 of
                  (l, s2) | Just (c,s3) <- T.uncons s2
                         -> case T.uncons s3 of
                              Just ('\n', s4) | c == '\r' -> l `T.snoc` '\r' `T.snoc` '\n' : lines' s4
                              _                           -> l `T.snoc` c : lines' s3

                          | otherwise
                         -> [l]

{-
lex_string :: String -> Lex Token
lex_string s = do
  i <- getInput
  case alexGetChar i of
    Nothing -> lit_error i

    Just ('"',i)  -> do
        setInput i
        return $! TokSym $! (reverse s)

    Just (c, i) -> do
        case c of
          '\\' -> do setInput i
                     c' <- lex_escape
                     lex_string (c':s)
          c | isAny c -> do setInput i
                            lex_string (c:s)
          _other -> lit_error i

isAny :: Char -> Bool
isAny c | c >= '\x7f' = Data.Char.isPrint c
        | otherwise   = c > ' '

lex_escape :: Lex Char
lex_escape = do
  i0 <- getInput
  c <- getCharOrFail i0
  case c of
        'a'   -> return '\a'
        'b'   -> return '\b'
        'f'   -> return '\f'
        'n'   -> return '\n'
        'r'   -> return '\r'
        't'   -> return '\t'
        'v'   -> return '\v'
        '\\'  -> return '\\'
        '"'   -> return '\"'
        '\''  -> return '\''
        '^'   -> do i1 <- getInput
                    c <- getCharOrFail i1
                    if c >= '@' && c <= '_'
                        then return (chr (ord c - ord '@'))
                        else lit_error i1

        'x'   -> readNum is_hexdigit 16 hexDigit
        'o'   -> readNum is_octdigit  8 octDecDigit
        x | is_decdigit x -> readNum2 is_decdigit 10 octDecDigit (octDecDigit x)

        c1 ->  do
           i <- getInput
           case alexGetChar i of
            Nothing -> lit_error i0
            Just (c2,i2) ->
              case alexGetChar i2 of
                Nothing -> do lit_error i0
                Just (c3,i3) ->
                   let str = [c1,c2,c3] in
                   case [ (c,rest) | (p,c) <- silly_escape_chars,
                                     Just rest <- [stripPrefix p str] ] of
                          (escape_char,[]):_ -> do
                                setInput i3
                                return escape_char
                          (escape_char,_:_):_ -> do
                                setInput i2
                                return escape_char
                          [] -> lit_error i0

readNum :: (Char -> Bool) -> Int -> (Char -> Int) -> Lex Char
readNum is_digit base conv = do
  i <- getInput
  c <- getCharOrFail i
  if is_digit c
        then readNum2 is_digit base conv (conv c)
        else lit_error i

readNum2 :: (Char -> Bool) -> Int -> (Char -> Int) -> Int -> Lex Char
readNum2 is_digit base conv i = do
  input <- getInput
  read i input
  where read i input = do
          case alexGetChar input of
            Just (c,input') | is_digit c -> do
               let i' = i*base + conv c
               if i' > 0x10ffff
                  then setInput input >> lexError "numeric escape sequence out of range"
                  else read i' input'
            _other -> do
              setInput input; return (chr i)

silly_escape_chars :: [(String, Char)]
silly_escape_chars = [
        ("NUL", '\NUL'),
        ("SOH", '\SOH'),
        ("STX", '\STX'),
        ("ETX", '\ETX'),
        ("EOT", '\EOT'),
        ("ENQ", '\ENQ'),
        ("ACK", '\ACK'),
        ("BEL", '\BEL'),
        ("BS", '\BS'),
        ("HT", '\HT'),
        ("LF", '\LF'),
        ("VT", '\VT'),
        ("FF", '\FF'),
        ("CR", '\CR'),
        ("SO", '\SO'),
        ("SI", '\SI'),
        ("DLE", '\DLE'),
        ("DC1", '\DC1'),
        ("DC2", '\DC2'),
        ("DC3", '\DC3'),
        ("DC4", '\DC4'),
        ("NAK", '\NAK'),
        ("SYN", '\SYN'),
        ("ETB", '\ETB'),
        ("CAN", '\CAN'),
        ("EM", '\EM'),
        ("SUB", '\SUB'),
        ("ESC", '\ESC'),
        ("FS", '\FS'),
        ("GS", '\GS'),
        ("RS", '\RS'),
        ("US", '\US'),
        ("SP", '\SP'),
        ("DEL", '\DEL')
        ]

hexDigit :: Char -> Int
hexDigit c | is_decdigit c = ord c - ord '0'
           | otherwise     = ord (to_lower c) - ord 'a' + 10

octDecDigit :: Char -> Int
octDecDigit c = ord c - ord '0'

is_decdigit :: Char -> Bool
is_decdigit c
        =  c >= '0' && c <= '9'

is_hexdigit :: Char -> Bool
is_hexdigit c
        =  is_decdigit c
        || (c >= 'a' && c <= 'f')
        || (c >= 'A' && c <= 'F')

is_octdigit :: Char -> Bool
is_octdigit c = c >= '0' && c <= '7'

to_lower :: Char -> Char
to_lower c
  | c >=  'A' && c <= 'Z' = chr (ord c - (ord 'A' - ord 'a'))


-- before calling lit_error, ensure that the current input is pointing to
-- the position of the error in the buffer.  This is so that we can report
-- a correct location to the user, but also so we can detect UTF-8 decoding
-- errors if they occur.
lit_error :: AlexInput -> Lex a
lit_error i = do setInput i; lexError "lexical error in string/character literal"

lexError :: String -> Lex a
lexError = error

getCharOrFail :: AlexInput -> Lex Char
getCharOrFail i =  do
  case alexGetChar i of
        Nothing -> lexError "unexpected end-of-file in string/character literal"
        Just (c,i)  -> do setInput i; return c

-- This version does not squash unicode characters, it is used when
-- lexing strings.
alexGetChar :: AlexInput -> Maybe (Char,AlexInput)
alexGetChar = undefined
-}

}

