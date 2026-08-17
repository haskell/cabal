{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      :  Distribution.GetOpt
-- Copyright   :  (c) Sven Panne 2002-2005
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Portability :  portable
--
-- This is a fork of "System.Console.GetOpt" with the following changes:
--
-- * Treat "cabal --flag command" as "cabal command --flag" e.g.
--   "cabal -v configure" to mean "cabal configure -v" For flags that are
--   not recognised as global flags, pass them on to the sub-command. See
--   the difference in 'shortOpt'.
--
-- * Line wrapping in the 'usageInfo' output, plus a more compact
--   rendering of short options, and slightly less padding.
--
-- * Parsing of option arguments is allowed to fail.
--
-- * 'ReturnInOrder' argument order is removed.
module Distribution.GetOpt
  ( -- * GetOpt
    getOpt
  , getOpt'
  , usageInfo
  , ArgOrder (..)
  , OptDescr (..)
  , ArgDescr (..)

    -- * Example

    -- | See "System.Console.GetOpt" for examples
  ) where

import Distribution.Compat.Prelude
import Prelude ()

-- | What to do with options following non-options
data ArgOrder a
  = -- | no option processing after first non-option
    RequireOrder
  | -- | freely intersperse options and non-options
    Permute

data OptDescr a -- description of a single options:
  = Option
      [Char] --    list of short option characters
      [String] --    list of long option strings (without "--")
      (ArgDescr a) --    argument descriptor
      String --    explanation of option for user

instance Functor OptDescr where
  fmap f (Option a b argDescr c) = Option a b (fmap f argDescr) c

-- | Describes whether an option takes an argument or not, and if so
-- how the argument is parsed to a value of type @a@.
--
-- Compared to System.Console.GetOpt, we allow for parse errors.
data ArgDescr a
  = -- |   no argument expected
    NoArg a
  | -- |   option requires argument
    ReqArg (String -> Either String a) String
  | -- |   optional argument
    OptArg String (Maybe String -> Either String a) String

instance Functor ArgDescr where
  fmap f (NoArg a) = NoArg (f a)
  fmap f (ReqArg g s) = ReqArg (fmap f . g) s
  fmap f (OptArg dv g s) = OptArg dv (fmap f . g) s

data OptKind a -- kind of cmd line arg (internal use only):
  = Opt a --    an option
  | UnreqOpt String --    an un-recognized option
  | NonOpt String --    a non-option
  | EndOfOpts --    end-of-options marker (i.e. "--")
  | OptErr String --    something went wrong...

data OptHelp = OptHelp
  { optNames :: String
  , optHelp :: String
  }

-- | Lays out the header followed with a formatted table of options.
--
-- An item in the table consists of the option and its help.  The option line is
-- a comma-separated list with all of the short options followed by only the
-- first long option.  No attempt is made to wrap this option line.
--
-- If space allows, the option help text is placed on the same line as the
-- option line.  If the option line is too long, the help text is placed on the
-- next line.  The help text is wrapped to fit within 80 columns and always
-- starts at the same column, column 33. Its first line is prefixed with a @#@
-- herald.
usageInfo :: String -> [OptDescr a] -> String
usageInfo header (map flattenNames -> options) = unlines (header : table)
  where
    (nameWidth, helpWidth) = let w = 30 in (w, 80 - (w + 3))
    indent = ' '

    table = do
      OptHelp{optNames, optHelp} <- options
      let wrappedHelp = wrapText helpWidth optHelp
      if length optNames >= nameWidth - 1
        then rows optNames [] ++ rows "" wrappedHelp
        else rows optNames wrappedHelp

    rows x [] = [indent : x]
    rows x (y : ys) = markedRow x y : map unmarkedRow ys

    markedRow name help = rowOption name ++ ' ' : '#' : ' ' : help
    unmarkedRow help = rowOption "" ++ "   " ++ help

    rowOption name = indent : padTo (nameWidth - 2) name

-- | Pad a string to a given length with spaces.
-- >>> padTo 5 "123"
-- "123  "
--
-- If the string is longer than the given length, it is not truncated.
-- >>> padTo 3 "12345"
-- "12345"
padTo :: Int -> String -> String
padTo = flip $ foldr (\ch rest n -> ch : rest (n - 1)) (`replicate` ' ')

-- | Flatten the short and long option names into a single string for display.
flattenNames :: OptDescr a -> OptHelp
flattenNames (Option sos los ad help) =
  OptHelp
    { optNames = intercalate ", " $ map (fmtShort ad) sos ++ map (fmtLong ad) (take 1 los)
    , optHelp = help
    }

-- | Pretty printing of short options.
-- * With required arguments can be given as:
--    @-w PATH or -wPATH (but not -w=PATH)@
--   This is displayed as:
--    @-w PATH or -wPATH@
-- * With optional but default arguments can be given as:
--    @-j or -jNUM (but not -j=NUM or -j NUM)@
--   This is displayed as:
--    @-j[NUM]@
fmtShort :: ArgDescr a -> Char -> String
fmtShort (NoArg _) so = "-" ++ [so]
fmtShort (ReqArg _ ad) so =
  let opt = "-" ++ [so]
   in opt ++ " " ++ ad ++ " or " ++ opt ++ ad
fmtShort (OptArg _ _ ad) so =
  let opt = "-" ++ [so]
   in opt ++ "[" ++ ad ++ "]"

-- | Pretty printing of long options.
-- * With required arguments can be given as:
--    @--with-compiler=PATH (but not --with-compiler PATH)@
--   This is displayed as:
--    @--with-compiler=PATH@
-- * With optional but default arguments can be given as:
--    @--jobs or --jobs=NUM (but not --jobs NUM)@
--   This is displayed as:
--    @--jobs[=NUM]@
fmtLong :: ArgDescr a -> String -> String
fmtLong (NoArg _) lo = "--" ++ lo
fmtLong (ReqArg _ ad) lo =
  let opt = "--" ++ lo
   in opt ++ "=" ++ ad
fmtLong (OptArg _ _ ad) lo =
  let opt = "--" ++ lo
   in opt ++ "[=" ++ ad ++ "]"

wrapText :: Int -> String -> [String]
wrapText width = map unwords . wrap 0 [] . words
  where
    wrap :: Int -> [String] -> [String] -> [[String]]
    wrap 0 [] (w : ws)
      | length w + 1 > width =
          wrap (length w) [w] ws
    wrap col line (w : ws)
      | col + length w + 1 > width =
          reverse line : wrap 0 [] (w : ws)
    wrap col line (w : ws) =
      let col' = col + length w + 1
       in wrap col' (w : line) ws
    wrap _ [] [] = []
    wrap _ line [] = [reverse line]

-- |
-- Process the command-line, and return the list of values that matched
-- (and those that didn\'t). The arguments are:
--
-- * The order requirements (see 'ArgOrder')
--
-- * The option descriptions (see 'OptDescr')
--
-- * The actual command line arguments (presumably got from
--   'System.Environment.getArgs').
--
-- 'getOpt' returns a triple consisting of the option arguments, a list
-- of non-options, and a list of error messages.
getOpt
  :: ArgOrder a -- non-option handling
  -> [OptDescr a] -- option descriptors
  -> [String] -- the command-line arguments
  -> ([a], [String], [String]) -- (options,non-options,error messages)
getOpt ordering optDescr args = (os, xs, es ++ map errUnrec us)
  where
    (os, xs, us, es) = getOpt' ordering optDescr args

-- |
-- This is almost the same as 'getOpt', but returns a quadruple
-- consisting of the option arguments, a list of non-options, a list of
-- unrecognized options, and a list of error messages.
getOpt'
  :: ArgOrder a -- non-option handling
  -> [OptDescr a] -- option descriptors
  -> [String] -- the command-line arguments
  -> ([a], [String], [String], [String]) -- (options,non-options,unrecognized,error messages)
getOpt' _ _ [] = ([], [], [], [])
getOpt' ordering optDescr (arg : args) = procNextOpt opt ordering
  where
    procNextOpt (Opt o) _ = (o : os, xs, us, es)
    procNextOpt (UnreqOpt u) _ = (os, xs, u : us, es)
    procNextOpt (NonOpt x) RequireOrder = ([], x : rest, [], [])
    procNextOpt (NonOpt x) Permute = (os, x : xs, us, es)
    procNextOpt EndOfOpts RequireOrder = ([], rest, [], [])
    procNextOpt EndOfOpts Permute = ([], rest, [], [])
    procNextOpt (OptErr e) _ = (os, xs, us, e : es)

    (opt, rest) = getNext arg args optDescr
    (os, xs, us, es) = getOpt' ordering optDescr rest

-- take a look at the next cmd line arg and decide what to do with it
getNext :: String -> [String] -> [OptDescr a] -> (OptKind a, [String])
getNext ['-', '-'] rest _ = (EndOfOpts, rest)
getNext ('-' : '-' : xs) rest optDescr = longOpt xs rest optDescr
getNext ('-' : x : xs) rest optDescr = shortOpt x xs rest optDescr
getNext a rest _ = (NonOpt a, rest)

-- handle long option
longOpt :: String -> [String] -> [OptDescr a] -> (OptKind a, [String])
longOpt ls rs optDescr = long ads arg rs
  where
    (opt, arg) = break (== '=') ls
    getWith p =
      [ o | o@(Option _ xs _ _) <- optDescr, isJust (find (p opt) xs)
      ]
    exact = getWith (==)
    options = if null exact then getWith isPrefixOf else exact
    ads = [ad | Option _ _ ad _ <- options]
    optStr = "--" ++ opt
    fromRes = fromParseResult optStr

    long (_ : _ : _) _ rest = (errAmbig options optStr, rest)
    long [NoArg a] [] rest = (Opt a, rest)
    long [NoArg _] ('=' : _) rest = (errNoArg optStr, rest)
    long [ReqArg _ d] [] [] = (errReq d optStr, [])
    long [ReqArg f _] [] (r : rest) = (fromRes (f r), rest)
    long [ReqArg f _] ('=' : xs) rest = (fromRes (f xs), rest)
    long [OptArg _ f _] [] rest = (fromRes (f Nothing), rest)
    long [OptArg _ f _] ('=' : xs) rest = (fromRes (f (Just xs)), rest)
    long _ _ rest = (UnreqOpt ("--" ++ ls), rest)

-- handle short option
shortOpt :: Char -> String -> [String] -> [OptDescr a] -> (OptKind a, [String])
shortOpt y ys rs optDescr = short ads ys rs
  where
    options = [o | o@(Option ss _ _ _) <- optDescr, s <- ss, y == s]
    ads = [ad | Option _ _ ad _ <- options]
    optStr = '-' : [y]
    fromRes = fromParseResult optStr

    short (_ : _ : _) _ rest = (errAmbig options optStr, rest)
    short (NoArg a : _) [] rest = (Opt a, rest)
    short (NoArg a : _) xs rest = (Opt a, ('-' : xs) : rest)
    short (ReqArg _ d : _) [] [] = (errReq d optStr, [])
    short (ReqArg f _ : _) [] (r : rest) = (fromRes (f r), rest)
    short (ReqArg f _ : _) xs rest = (fromRes (f xs), rest)
    short (OptArg _ f _ : _) [] rest = (fromRes (f Nothing), rest)
    short (OptArg _ f _ : _) xs rest = (fromRes (f (Just xs)), rest)
    short [] [] rest = (UnreqOpt optStr, rest)
    short [] xs rest = (UnreqOpt (optStr ++ xs), rest)

-- This is different vs upstream = (UnreqOpt optStr,('-':xs):rest)
-- Apparently this was part of the change so that flags that are
-- not recognised as global flags are passed on to the sub-command.
-- But why was no equivalent change required for longOpt? So could
-- this change go upstream?

fromParseResult :: String -> Either String a -> OptKind a
fromParseResult optStr res = case res of
  Right x -> Opt x
  Left err -> OptErr ("invalid argument to option `" ++ optStr ++ "': " ++ err ++ "\n")

-- miscellaneous error formatting

errAmbig :: [OptDescr a] -> String -> OptKind b
errAmbig ods optStr = OptErr (usageInfo header ods)
  where
    header = "option `" ++ optStr ++ "' is ambiguous; could be one of:"

errReq :: String -> String -> OptKind a
errReq d optStr = OptErr ("option `" ++ optStr ++ "' requires an argument " ++ d ++ "\n")

errUnrec :: String -> String
errUnrec optStr = "unrecognized option `" ++ optStr ++ "'\n"

errNoArg :: String -> OptKind a
errNoArg optStr = OptErr ("option `" ++ optStr ++ "' doesn't allow an argument\n")
