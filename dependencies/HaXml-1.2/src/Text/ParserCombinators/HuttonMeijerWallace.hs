{-----------------------------------------------------------------------------

                 A LIBRARY OF MONADIC PARSER COMBINATORS

                              29th July 1996

                 Graham Hutton               Erik Meijer
            University of Nottingham    University of Utrecht

This Haskell 1.3 script defines a library of parser combinators, and is taken
from sections 1-6 of our article "Monadic Parser Combinators".  Some changes
to the library have been made in the move from Gofer to Haskell:

   * Do notation is used in place of monad comprehension notation;

   * The parser datatype is defined using "newtype", to avoid the overhead
     of tagging and untagging parsers with the P constructor.

------------------------------------------------------------------------------
** Extended to allow a symbol table/state to be threaded through the monad.
** Extended to allow a parameterised token type, rather than just strings.
** Extended to allow error-reporting.

(Extensions: 1998-2000 Malcolm.Wallace@cs.york.ac.uk)
(More extensions: 2004 gk-haskell@ninebynine.org)

------------------------------------------------------------------------------}

-- | This library of monadic parser combinators is based on the ones
--   defined by Graham Hutton and Erik Meijer.  It has been extended by
--   Malcolm Wallace to use an abstract token type (no longer just a
--   string) as input, and to incorporate a State Transformer monad, useful
--   for symbol tables, macros, and so on.  Basic facilities for error
--   reporting have also been added, and later extended by Graham Klyne
--   to return the errors through an @Either@ type, rather than just
--   calling @error@.

module Text.ParserCombinators.HuttonMeijerWallace
  (
  -- * The parser monad
    Parser(..)
  -- * Primitive parser combinators
  , item, eof, papply, papply'
  -- * Derived combinators
  , (+++), {-sat,-} tok, nottok, many, many1
  , sepby, sepby1, chainl, chainl1, chainr, chainr1, ops, bracket
  , toEOF
  -- * Error handling
  , elserror
  -- * State handling
  , stupd, stquery, stget
  -- * Re-parsing
  , reparse
  ) where

import Data.Char
import Control.Monad

infixr 5 +++

--- The parser monad ---------------------------------------------------------

type ParseResult s t e a = Either e [(a,s,[Either e t])]

newtype Parser s t e a   = P ( s -> [Either e t] -> ParseResult s t e a )
    -- ^ The parser type is parametrised on the types of the state @s@,
    --   the input tokens @t@, error-type @e@, and the result value @a@.
    --   The state and remaining input are threaded through the monad.

instance Functor (Parser s t e) where
   -- fmap        :: (a -> b) -> (Parser s t e a -> Parser s t e b)
   fmap f (P p)    = P (\st inp -> case p st inp of
                        Right res -> Right [(f v, s, out) | (v,s,out) <- res]
                        Left err  -> Left err
                       )

instance Monad (Parser s t e) where
   -- return      :: a -> Parser s t e a
   return v        = P (\st inp -> Right [(v,st,inp)])
   -- >>=         :: Parser s t e a -> (a -> Parser s t e b) -> Parser s t e b
   (P p) >>= f     = P (\st inp -> case p st inp of
                        Right res -> foldr joinresults (Right [])
                            [ papply' (f v) s out | (v,s,out) <- res ]
                        Left err  -> Left err
                       )
   -- fail        :: String -> Parser s t e a
   fail err        = P (\st inp -> Right [])
  -- I know it's counterintuitive, but we want no-parse, not an error.

instance MonadPlus (Parser s t e) where
   -- mzero       :: Parser s t e a
   mzero           = P (\st inp -> Right [])
   -- mplus       :: Parser s t e a -> Parser s t e a -> Parser s t e a
   (P p) `mplus` (P q) = P (\st inp -> joinresults (p st inp) (q st inp))

-- joinresults ensures that explicitly raised errors are dominant,
-- provided no parse has yet been found.  The commented out code is
-- a slightly stricter specification of the real code.
joinresults :: ParseResult s t e a -> ParseResult s t e a -> ParseResult s t e a
{-
joinresults (Left  p)  (Left  q)  = Left  p
joinresults (Left  p)  (Right _)  = Left  p
joinresults (Right []) (Left  q)  = Left  q
joinresults (Right p)  (Left  q)  = Right p
joinresults (Right p)  (Right q)  = Right (p++q)
-}
joinresults (Left  p)  q  = Left p
joinresults (Right []) q  = q
joinresults (Right p)  q  = Right (p++ case q of Left _  -> []
                                                 Right r -> r)


--- Primitive parser combinators ---------------------------------------------

-- | Deliver the first remaining token.
item              :: Parser s t e t
item               = P (\st inp -> case inp of
                        []            -> Right []
                        (Left e: _)   -> Left e
                        (Right x: xs) -> Right [(x,st,xs)]
                       )

-- | Fail if end of input is not reached
eof               :: Show p => Parser s (p,t) String ()
eof                = P (\st inp -> case inp of
                        []         -> Right [((),st,[])]
                        (Left e:_) -> Left e
                        (Right (p,_):_) -> Left ("End of input expected at "
                                                 ++show p++"\n  but found text")
                       )

{-
-- | Ensure the value delivered by the parser is evaluated to WHNF.
force             :: Parser s t e a -> Parser s t e a
force (P p)        = P (\st inp -> let Right xs = p st inp
                                       h = head xs in
                                   h `seq` Right (h: tail xs)
                       )
--  [[[GK]]]  ^^^^^^
--  WHNF = Weak Head Normal Form, meaning that it has no top-level redex.
--  In this case, I think that means that the first element of the list
--  is fully evaluated.
--
--  NOTE:  the original form of this function fails if there is no parse
--  result for p st inp (head xs fails if xs is null), so the modified
--  form can assume a Right value only.
--
--  Why is this needed?
--  It's not exported, and the only use of this I see is commented out.
---------------------------------------
-}


-- | Deliver the first parse result only, eliminating any backtracking.
first             :: Parser s t e a -> Parser s t e a
first (P p)        = P (\st inp -> case p st inp of
                                   Right (x:xs) -> Right [x]
                                   otherwise    -> otherwise
                       )

-- | Apply the parser to some real input, given an initial state value.
--   If the parser fails, raise 'error' to halt the program.
--   (This is the original exported behaviour - to allow the caller to
--   deal with the error differently, see @papply'@.)
papply            :: Parser s t String a -> s -> [Either String t]
                                              -> [(a,s,[Either String t])]
papply (P p) st inp = either error id (p st inp)

-- | Apply the parser to some real input, given an initial state value.
--   If the parser fails, return a diagnostic message to the caller.
papply'           :: Parser s t e a -> s -> [Either e t]
                                         -> Either e [(a,s,[Either e t])]
papply' (P p) st inp = p st inp

--- Derived combinators ------------------------------------------------------

-- | A choice between parsers.  Keep only the first success.
(+++)             :: Parser s t e a -> Parser s t e a -> Parser s t e a
p +++ q            = first (p `mplus` q)

-- | Deliver the first token if it satisfies a predicate.
sat               :: (t -> Bool) -> Parser s (p,t) e t
sat p              = do {(_,x) <- item; if p x then return x else mzero}

-- | Deliver the first token if it equals the argument.
tok               :: Eq t => t -> Parser s (p,t) e t
tok t              = do {(_,x) <- item; if x==t then return t else mzero}

-- | Deliver the first token if it does not equal the argument.
nottok            :: Eq t => [t] -> Parser s (p,t) e t
nottok ts          = do {(_,x) <- item; if x `notElem` ts then return x
                                        else mzero}

-- | Deliver zero or more values of @a@.
many              :: Parser s t e a -> Parser s t e [a]
many p             = many1 p +++ return []
--many p           = force (many1 p +++ return [])

-- | Deliver one or more values of @a@.
many1             :: Parser s t e a -> Parser s t e [a]
many1 p            = do {x <- p; xs <- many p; return (x:xs)}

-- | Deliver zero or more values of @a@ separated by @b@'s.
sepby             :: Parser s t e a -> Parser s t e b -> Parser s t e [a]
p `sepby` sep      = (p `sepby1` sep) +++ return []

-- | Deliver one or more values of @a@ separated by @b@'s.
sepby1            :: Parser s t e a -> Parser s t e b -> Parser s t e [a]
p `sepby1` sep     = do {x <- p; xs <- many (do {sep; p}); return (x:xs)}

chainl            :: Parser s t e a -> Parser s t e (a->a->a) -> a
                                                              -> Parser s t e a
chainl p op v      = (p `chainl1` op) +++ return v

chainl1           :: Parser s t e a -> Parser s t e (a->a->a) -> Parser s t e a
p `chainl1` op     = do {x <- p; rest x}
                     where
                        rest x = do {f <- op; y <- p; rest (f x y)}
                                 +++ return x

chainr            :: Parser s t e a -> Parser s t e (a->a->a) -> a
                                                              -> Parser s t e a
chainr p op v      = (p `chainr1` op) +++ return v

chainr1           :: Parser s t e a -> Parser s t e (a->a->a) -> Parser s t e a
p `chainr1` op     = do {x <- p; rest x}
                     where
                        rest x = do { f <- op
                                    ; y <- p `chainr1` op
                                    ; return (f x y)
                                    }
                                 +++ return x

ops               :: [(Parser s t e a, b)] -> Parser s t e b
ops xs             = foldr1 (+++) [do {p; return op} | (p,op) <- xs]

bracket           :: (Show p,Show t) =>
                     Parser s (p,t) e a -> Parser s (p,t) e b ->
                               Parser s (p,t) e c -> Parser s (p,t) e b
bracket open p close = do { open
                          ; x <- p
                          ; close -- `elserror` "improperly matched construct";
                          ; return x
                          }

-- | Accept a complete parse of the input only, no partial parses.
toEOF             :: Show p =>
                     Parser s (p,t) String a -> Parser s (p,t) String a
toEOF p            = do { x <- p; eof; return x }


--- Error handling -----------------------------------------------------------

-- | Return an error using the supplied diagnostic string, and a token type
--   which includes position information.
parseerror :: (Show p,Show t) => String -> Parser s (p,t) String a
parseerror err = P (\st inp ->
                         case inp of
                           [] -> Left "Parse error: unexpected EOF\n"
                           (Left e:_) -> Left ("Lexical error:  "++e)
                           (Right (p,t):_)  ->
                                 Left ("Parse error: in  "++show p++"\n    "
                                       ++err++"\n  "++"Found "++show t)
                   )


-- | If the parser fails, generate an error message.
elserror          :: (Show p,Show t) => Parser s (p,t) String a -> String
                                        -> Parser s (p,t) String a
p `elserror` s     = p +++ parseerror s

--- State handling -----------------------------------------------------------

-- | Update the internal state.
stupd      :: (s->s) -> Parser s t e ()
stupd f     = P (\st inp-> {-let newst = f st in newst `seq`-}
                           Right [((), f st, inp)])

-- | Query the internal state.
stquery    :: (s->a) -> Parser s t e a
stquery f   = P (\st inp-> Right [(f st, st, inp)])

-- | Deliver the entire internal state.
stget      :: Parser s t e s
stget       = P (\st inp-> Right [(st, st, inp)])


--- Push some tokens back onto the input stream and reparse ------------------

-- | This is useful for recursively expanding macros.  When the
--   user-parser recognises a macro use, it can lookup the macro
--   expansion from the parse state, lex it, and then stuff the
--   lexed expansion back down into the parser.
reparse    :: [Either e t] -> Parser s t e ()
reparse ts  = P (\st inp-> Right [((), st, ts++inp)])

------------------------------------------------------------------------------
