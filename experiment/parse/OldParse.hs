module OldParse {-(readFields, ParseResult(..), runP)-} where

import Control.Applicative (Applicative(..))
import Control.Monad (ap, liftM)

import Distribution.Compat.ReadP as ReadP hiding (get)
import Distribution.Simple.Utils (intercalate)

import Data.Char (isSpace, toLower, isAlphaNum)
import Data.Tree as Tree (Tree(..), flatten)

-- -----------------------------------------------------------------------------

type LineNo = Int

data PError = AmbigousParse String LineNo
            | NoParse String LineNo
            | TabsError LineNo
            | FromString String (Maybe LineNo)
        deriving Show

data PWarning = PWarning String
              | UTFWarning LineNo String
        deriving Show


data ParseResult a = ParseFailed PError | ParseOk [PWarning] a
        deriving Show

instance Functor ParseResult where
        fmap = liftM

instance Applicative ParseResult where
        pure = ParseOk []
        (<*>) = ap

instance Monad ParseResult where
        return x = ParseOk [] x
        ParseFailed err >>= _ = ParseFailed err
        ParseOk ws x >>= f = case f x of
                               ParseFailed err -> ParseFailed err
                               ParseOk ws' x' -> ParseOk (ws'++ws) x'
        fail s = ParseFailed (FromString s Nothing)

runP :: LineNo -> String -> ReadP a a -> String -> ParseResult a
runP line fieldname p s =
  case [ x | (x,"") <- results ] of
    [a] -> ParseOk (utf8Warnings line fieldname s) a
    --TODO: what is this double parse thing all about?
    --      Can't we just do the all isSpace test the first time?
    []  -> case [ x | (x,ys) <- results, all isSpace ys ] of
             [a] -> ParseOk (utf8Warnings line fieldname s) a
             []  -> ParseFailed (NoParse fieldname line)
             _   -> ParseFailed (AmbigousParse fieldname line)
    _   -> ParseFailed (AmbigousParse fieldname line)
  where results = readP_to_S p s

utf8Warnings :: LineNo -> String -> String -> [PWarning]
utf8Warnings line fieldname s =
  take 1 [ UTFWarning n fieldname
         | (n,l) <- zip [line..] (lines s)
         , '\xfffd' `elem` l ]

syntaxError :: LineNo -> String -> ParseResult a
syntaxError n s = ParseFailed $ FromString s (Just n)

tabsError :: LineNo -> ParseResult a
tabsError ln = ParseFailed $ TabsError ln


------------------------------------------------------------------------------

-- The data type for our three syntactic categories
data Field
    = F LineNo String String
      -- ^ A regular @<property>: <value>@ field
    | Section LineNo String String [Field]
      -- ^ A section with a name and possible parameter.  The syntactic
      -- structure is:
      --
      -- @
      --   <sectionname> <arg> {
      --     <field>*
      --   }
      -- @
    | IfBlock LineNo String [Field] [Field]
      -- ^ A conditional block with an optional else branch:
      --
      -- @
      --  if <condition> {
      --    <field>*
      --  } else {
      --    <field>*
      --  }
      -- @
      deriving (Show
               ,Eq)   -- for testing


readFields :: String -> ParseResult [Field]
readFields input = ifelse
               =<< mapM (mkField 0)
               =<< mkTree tokens

  where ls = (lines . normaliseLineEndings) input
        tokens = (concatMap tokeniseLine . trimLines) ls

-- attach line number and determine indentation
trimLines :: [String] -> [(LineNo, Indent, HasTabs, String)]
trimLines ls = [ (lineno, indent, hastabs, (trimTrailing l'))
               | (lineno, l) <- zip [1..] ls
               , let (sps, l') = span isSpace l
                     indent    = length sps
                     hastabs   = '\t' `elem` sps
               , validLine l' ]
  where validLine ('-':'-':_) = False      -- Comment
        validLine []          = False      -- blank line
        validLine _           = True

-- | We parse generically based on indent level and braces '{' '}'. To do that
-- we split into lines and then '{' '}' tokens and other spans within a line.
data Token =
       -- | The 'Line' token is for bits that /start/ a line, eg:
       --
       -- > "\n  blah blah { blah"
       --
       -- tokenises to:
       --
       -- > [Line n 2 False "blah blah", OpenBracket, Span n "blah"]
       --
       -- so lines are the only ones that can have nested layout, since they
       -- have a known indentation level.
       --
       -- eg: we can't have this:
       --
       -- > if ... {
       -- > } else
       -- >     other
       --
       -- because other cannot nest under else, since else doesn't start a line
       -- so cannot have nested layout. It'd have to be:
       --
       -- > if ... {
       -- > }
       -- >   else
       -- >     other
       --
       -- but that's not so common, people would normally use layout or
       -- brackets not both in a single @if else@ construct.
       --
       -- > if ... { foo : bar }
       -- > else
       -- >    other
       --
       -- this is ok
       Line LineNo Indent HasTabs String
     | Span LineNo                String  -- ^ span in a line, following brackets
     | OpenBracket LineNo | CloseBracket LineNo

type Indent = Int
type HasTabs = Bool

-- | Tokenise a single line, splitting on '{' '}' and the spans inbetween.
-- Also trims leading & trailing space on those spans within the line.
tokeniseLine :: (LineNo, Indent, HasTabs, String) -> [Token]
tokeniseLine (n0, i, t, l) = case split n0 l of
                            (Span _ l':ss) -> Line n0 i t l' :ss
                            cs              -> cs
  where split _ "" = []
        split n s  = case span (\c -> c /='}' && c /= '{') s of
          ("", '{' : s') ->             OpenBracket  n : split n s'
          (w , '{' : s') -> mkspan n w (OpenBracket  n : split n s')
          ("", '}' : s') ->             CloseBracket n : split n s'
          (w , '}' : s') -> mkspan n w (CloseBracket n : split n s')
          (w ,        _) -> mkspan n w []

        mkspan n s ss | null s'   =             ss
                      | otherwise = Span n s' : ss
          where s' = trimTrailing (trimLeading s)

trimLeading, trimTrailing :: String -> String
trimLeading  = dropWhile isSpace
trimTrailing = reverse . dropWhile isSpace . reverse


-- | Fix different systems silly line ending conventions
normaliseLineEndings :: String -> String
normaliseLineEndings [] = []
normaliseLineEndings ('\r':'\n':s) = '\n' : normaliseLineEndings s -- windows
normaliseLineEndings ('\r':s)      = '\n' : normaliseLineEndings s -- old osx
normaliseLineEndings (  c :s)      =   c  : normaliseLineEndings s

type SyntaxTree = Tree (LineNo, HasTabs, String)

-- | Parse the stream of tokens into a tree of them, based on indent \/ layout
mkTree :: [Token] -> ParseResult [SyntaxTree]
mkTree toks =
  layout 0 [] toks >>= \(trees, trailing) -> case trailing of
    []               -> return trees
    OpenBracket  n:_ -> syntaxError n "mismatched backets, unexpected {"
    CloseBracket n:_ -> syntaxError n "mismatched backets, unexpected }"
    -- the following two should never happen:
    Span n     l  :_ -> syntaxError n $ "unexpected span: " ++ show l
    Line n _ _ l  :_ -> syntaxError n $ "unexpected line: " ++ show l


-- | Parse the stream of tokens into a tree of them, based on indent
-- This parse state expect to be in a layout context, though possibly
-- nested within a braces context so we may still encounter closing braces.
layout :: Indent       -- ^ indent level of the parent\/previous line
       -> [SyntaxTree] -- ^ accumulating param, trees in this level
       -> [Token]      -- ^ remaining tokens
       -> ParseResult ([SyntaxTree], [Token])
                       -- ^ collected trees on this level and trailing tokens
layout _ a []                               = return (reverse a, [])
layout i a (s@(Line _ i' _ _):ss) | i' < i  = return (reverse a, s:ss)
layout i a (Line n _ t l:OpenBracket n':ss) = do
    (sub, ss') <- braces n' [] ss
    layout i (Node (n,t,l) sub:a) ss'

layout i a (Span n     l:OpenBracket n':ss) = do
    (sub, ss') <- braces n' [] ss
    layout i (Node (n,False,l) sub:a) ss'

-- look ahead to see if following lines are more indented, giving a sub-tree
layout i a (Line n i' t l:ss) = do
    lookahead <- layout (i'+1) [] ss
    case lookahead of
        ([], _)   -> layout i (Node (n,t,l) [] :a) ss
        (ts, ss') -> layout i (Node (n,t,l) ts :a) ss'

layout _ _ (   OpenBracket  n :_)  = syntaxError n $ "unexpected '{'"
layout _ a (s@(CloseBracket _):ss) = return (reverse a, s:ss)
layout _ _ (   Span n l       : _) = syntaxError n $ "unexpected span: "
                                                  ++ show l

-- | Parse the stream of tokens into a tree of them, based on explicit braces
-- This parse state expects to find a closing bracket.
braces :: LineNo       -- ^ line of the '{', used for error messages
       -> [SyntaxTree] -- ^ accumulating param, trees in this level
       -> [Token]      -- ^ remaining tokens
       -> ParseResult ([SyntaxTree],[Token])
                       -- ^ collected trees on this level and trailing tokens
braces m a (Line n _ t l:OpenBracket n':ss) = do
    (sub, ss') <- braces n' [] ss
    braces m (Node (n,t,l) sub:a) ss'

braces m a (Span n     l:OpenBracket n':ss) = do
    (sub, ss') <- braces n' [] ss
    braces m (Node (n,False,l) sub:a) ss'

braces m a (Line n i t l:ss) = do
    lookahead <- layout (i+1) [] ss
    case lookahead of
        ([], _)   -> braces m (Node (n,t,l) [] :a) ss
        (ts, ss') -> braces m (Node (n,t,l) ts :a) ss'

braces m a (Span n       l:ss) = braces m (Node (n,False,l) []:a) ss
braces _ a (CloseBracket _:ss) = return (reverse a, ss)
braces n _ []                  = syntaxError n $ "opening brace '{'"
                              ++ "has no matching closing brace '}'"
braces _ _ (OpenBracket  n:_)  = syntaxError n "unexpected '{'"

-- | Convert the parse tree into the Field AST
-- Also check for dodgy uses of tabs in indentation.
mkField :: Int -> SyntaxTree -> ParseResult Field
mkField d (Node (n,t,_) _) | d >= 1 && t = tabsError n
mkField d (Node (n,_,l) ts) = case span (\c -> isAlphaNum c || c == '-') l of
  ([], _)       -> syntaxError n $ "unrecognised field or section: " ++ show l
  (name, rest)  -> case trimLeading rest of
    (':':rest') -> do let followingLines = concatMap Tree.flatten ts
                          tabs = not (null [()| (_,True,_) <- followingLines ])
                      if tabs && d >= 1
                        then tabsError n
                        else return $ F n (map toLower name)
                                          (fieldValue rest' followingLines)
    rest'       -> do ts' <- mapM (mkField (d+1)) ts
                      return (Section n (map toLower name) rest' ts')
    where fieldValue firstLine followingLines =
            let firstLine' = trimLeading firstLine
                followingLines' = map (\(_,_,s) -> stripDot s) followingLines
                allLines | null firstLine' =              followingLines'
                         | otherwise       = firstLine' : followingLines'
             in intercalate "\n" allLines
          stripDot "." = ""
          stripDot s   = s

-- | Convert if/then/else 'Section's to 'IfBlock's
ifelse :: [Field] -> ParseResult [Field]
ifelse [] = return []
ifelse (Section n "if"   cond thenpart
       :Section _ "else" as   elsepart:fs)
       | null cond     = syntaxError n "'if' with missing condition"
       | null thenpart = syntaxError n "'then' branch of 'if' is empty"
       | not (null as) = syntaxError n "'else' takes no arguments"
       | null elsepart = syntaxError n "'else' branch of 'if' is empty"
       | otherwise     = do tp  <- ifelse thenpart
                            ep  <- ifelse elsepart
                            fs' <- ifelse fs
                            return (IfBlock n cond tp ep:fs')
ifelse (Section n "if"   cond thenpart:fs)
       | null cond     = syntaxError n "'if' with missing condition"
       | null thenpart = syntaxError n "'then' branch of 'if' is empty"
       | otherwise     = do tp  <- ifelse thenpart
                            fs' <- ifelse fs
                            return (IfBlock n cond tp []:fs')
ifelse (Section n "else" _ _:_) = syntaxError n "stray 'else' with no preceding 'if'"
ifelse (Section n s a fs':fs) = do fs''  <- ifelse fs'
                                   fs''' <- ifelse fs
                                   return (Section n s a fs'' : fs''')
ifelse (f:fs) = do fs' <- ifelse fs
                   return (f : fs')

