{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Parsec.LexerMonad
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
module Distribution.Parsec.LexerMonad (
    InputStream,
    LexState(..),
    LexResult(..),

    Lex(..),
    execLexer,

    getPos,
    setPos,
    adjustPos,

    getInput,
    setInput,

    getStartCode,
    setStartCode,

    addComment,

    LexWarning(..),
    LexWarningType(..),
    addWarning,
    toPWarnings,

  ) where

import qualified Data.ByteString             as B
import           Distribution.Compat.Prelude
import           Distribution.Parsec.Common  (PWarnType (..), PWarning (..), Position (..), showPos)
import           Prelude ()

import qualified Distribution.Compat.Map.Strict as Map

#ifdef CABAL_PARSEC_DEBUG
-- testing only:
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector        as V
#endif

-- simple state monad
newtype Lex a = Lex { unLex :: LexState -> LexResult a }

instance Functor Lex where
  fmap = liftM

instance Applicative Lex where
  pure = returnLex
  (<*>) = ap

instance Monad Lex where
  return = pure
  (>>=)  = thenLex

data LexResult a = LexResult {-# UNPACK #-} !LexState a

data LexWarningType
    = LexWarningNBSP  -- ^ Encountered non breaking space
    | LexWarningBOM   -- ^ BOM at the start of the cabal file
    | LexWarningTab   -- ^ Leading tags
  deriving (Eq, Ord, Show)

data LexWarning = LexWarning                !LexWarningType
                             {-# UNPACK #-} !Position
  deriving (Show)

toPWarnings :: [LexWarning] -> [PWarning]
toPWarnings
    = map (uncurry toWarning)
    . Map.toList
    . Map.fromListWith (++)
    . map (\(LexWarning t p) -> (t, [p]))
  where
    toWarning LexWarningBOM poss =
        PWarning PWTLexBOM (head poss) "Byte-order mark found at the beginning of the file"
    toWarning LexWarningNBSP poss =
        PWarning PWTLexNBSP (head poss) $ "Non breaking spaces at " ++ intercalate ", " (map showPos poss)
    toWarning LexWarningTab poss =
        PWarning PWTLexTab (head poss) $ "Tabs used as indentation at " ++ intercalate ", " (map showPos poss)

data LexState = LexState {
        curPos   :: {-# UNPACK #-} !Position,        -- ^ position at current input location
        curInput :: {-# UNPACK #-} !InputStream,     -- ^ the current input
        curCode  :: {-# UNPACK #-} !StartCode,       -- ^ lexer code
        comments :: [(Position, B.ByteString)],
        warnings :: [LexWarning]
#ifdef CABAL_PARSEC_DEBUG
        , dbgText  :: V.Vector T.Text                -- ^ input lines, to print pretty debug info
#endif
     } --TODO: check if we should cache the first token
       -- since it looks like parsec's uncons can be called many times on the same input

type StartCode   = Int    -- ^ An @alex@ lexer start code
type InputStream = B.ByteString



-- | Execute the given lexer on the supplied input stream.
execLexer :: Lex a -> InputStream -> ([LexWarning], a)
execLexer (Lex lexer) input =
    case lexer initialState of
      LexResult LexState{ warnings = ws } result -> (ws, result)
  where
    initialState = LexState
      -- TODO: add 'startPosition'
      { curPos   = Position 1 1
      , curInput = input
      , curCode  = 0
      , warnings = []
#ifdef CABAL_PARSEC_DEBUG
      , dbgText  = V.fromList . T.lines . T.decodeUtf8 $ input
#endif
      }

{-# INLINE returnLex #-}
returnLex :: a -> Lex a
returnLex a = Lex $ \s -> LexResult s a

{-# INLINE thenLex #-}
thenLex :: Lex a -> (a -> Lex b) -> Lex b
(Lex m) `thenLex` k = Lex $ \s -> case m s of LexResult s' a -> (unLex (k a)) s'

setPos :: Position -> Lex ()
setPos pos = Lex $ \s -> LexResult s{ curPos = pos } ()

getPos :: Lex Position
getPos = Lex $ \s@LexState{ curPos = pos } -> LexResult s pos

adjustPos :: (Position -> Position) -> Lex ()
adjustPos f = Lex $ \s@LexState{ curPos = pos } -> LexResult s{ curPos = f pos } ()

getInput :: Lex InputStream
getInput = Lex $ \s@LexState{ curInput = i } -> LexResult s i

setInput :: InputStream -> Lex ()
setInput i = Lex $ \s -> LexResult s{ curInput = i } ()

getStartCode :: Lex Int
getStartCode = Lex $ \s@LexState{ curCode = c } -> LexResult s c

setStartCode :: Int -> Lex ()
setStartCode c = Lex $ \s -> LexResult s{ curCode = c } ()

addComment :: Position -> B.ByteString -> Lex ()
addComment p b = Lex $ \s@LexState{ comments = cs } -> LexResult (s { comments = (p,b) : cs }) ()

-- | Add warning at the current position
addWarning :: LexWarningType -> Lex ()
addWarning wt = Lex $ \s@LexState{ curPos = pos, warnings = ws  } ->
    LexResult s{ warnings = LexWarning wt pos : ws } ()
