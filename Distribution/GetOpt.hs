-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.GetOpt
-- Copyright   :  (c) Sven Panne 2002-2005
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Portability :  portable
--
-- This library provides facilities for parsing the command-line options
-- in a standalone program.  It is essentially a Haskell port of the GNU
-- @getopt@ library.
--
-----------------------------------------------------------------------------

{-
Sven Panne <Sven.Panne@informatik.uni-muenchen.de> Oct. 1996 (small
changes Dec. 1997)

Two rather obscure features are missing: The Bash 2.0 non-option hack
(if you don't already know it, you probably don't want to hear about
it...) and the recognition of long options with a single dash
(e.g. '-help' is recognised as '--help', as long as there is no short
option 'h').

Other differences between GNU's getopt and this implementation:

* To enforce a coherent description of options and arguments, there
  are explanation fields in the option/argument descriptor.

* Error messages are now more informative, but no longer POSIX
  compliant... :-(

And a final Haskell advertisement: The GNU C implementation uses well
over 1100 lines, we need only 195 here, including a 46 line example!
:-)
-}

-- #hide
module Distribution.GetOpt (
   -- * GetOpt
   getOpt, getOpt',
   usageInfo,
   ArgOrder(..),
   OptDescr(..),
   ArgDescr(..),

   -- * Example

   -- $example
) where

import Data.List ( isPrefixOf, intersperse, find )

-- |What to do with options following non-options
data ArgOrder a
  = RequireOrder                -- ^ no option processing after first non-option
  | Permute                     -- ^ freely intersperse options and non-options
  | ReturnInOrder (String -> a) -- ^ wrap non-options into options

{-|
Each 'OptDescr' describes a single option.

The arguments to 'Option' are:

* list of short option characters

* list of long option strings (without \"--\")

* argument descriptor

* explanation of option for user
-}
data OptDescr a =              -- description of a single options:
   Option [Char]                --    list of short option characters
          [String]              --    list of long option strings (without "--")
          (ArgDescr a)          --    argument descriptor
          String                --    explanation of option for user

-- |Describes whether an option takes an argument or not, and if so
-- how the argument is injected into a value of type @a@.
data ArgDescr a
   = NoArg                   a         -- ^   no argument expected
   | ReqArg (String       -> a) String -- ^   option requires argument
   | OptArg (Maybe String -> a) String -- ^   optional argument

data OptKind a                -- kind of cmd line arg (internal use only):
   = Opt       a                --    an option
   | UnreqOpt  String           --    an un-recognized option
   | NonOpt    String           --    a non-option
   | EndOfOpts                  --    end-of-options marker (i.e. "--")
   | OptErr    String           --    something went wrong...

-- | Return a string describing the usage of a command, derived from
-- the header (first argument) and the options described by the
-- second argument.
usageInfo :: String                    -- header
          -> [OptDescr a]              -- option descriptors
          -> String                    -- nicely formatted decription of options
usageInfo header optDescr = unlines (header:table)
   where (ss,ls,ds) = unzip3 [ (sepBy ", " (map (fmtShort ad) sos)
                               ,concatMap (fmtLong  ad) (take 1 los)
                               ,d)
                             | Option sos los ad d <- optDescr ]
         ssWidth    = (maximum . map length) ss
         lsWidth    = (maximum . map length) ls
         dsWidth    = 30 `max` (80 - (ssWidth + lsWidth + 3))
         table      = [ " " ++ padTo ssWidth so' ++
                        " " ++ padTo lsWidth lo' ++
                        " " ++ d'
                      | (so,lo,d) <- zip3 ss ls ds
                      , (so',lo',d') <- fmtOpt dsWidth so lo d ]
         padTo n x  = take n (x ++ repeat ' ')
         sepBy s    = concat . intersperse s

fmtOpt :: Int -> String -> String -> String -> [(String, String, String)]
fmtOpt descrWidth so lo descr =
   case wrapText descrWidth descr of
     []     -> [(so,lo,"")]
     (d:ds) -> (so,lo,d) : [ ("","",d') | d' <- ds ]

fmtShort :: ArgDescr a -> Char -> String
fmtShort (NoArg  _   ) so = "-" ++ [so]
fmtShort (ReqArg _  _) so = "-" ++ [so]
fmtShort (OptArg _  _) so = "-" ++ [so]

fmtLong :: ArgDescr a -> String -> String
fmtLong (NoArg  _   ) lo = "--" ++ lo
fmtLong (ReqArg _ ad) lo = "--" ++ lo ++ "=" ++ ad
fmtLong (OptArg _ ad) lo = "--" ++ lo ++ "[=" ++ ad ++ "]"

wrapText :: Int -> String -> [String]
wrapText width = map unwords . wrap 0 [] . words
  where wrap :: Int -> [String] -> [String] -> [[String]]
        wrap 0   []   (w:ws)
          | length w + 1 > width
          = wrap (length w) [w] ws
        wrap col line (w:ws)
          | col + length w + 1 > width
          = reverse line : wrap 0 [] (w:ws)
        wrap col line (w:ws)
          = let col' = col + length w + 1
             in wrap col' (w:line) ws
        wrap _ []   [] = []
        wrap _ line [] = [reverse line]

{-|
Process the command-line, and return the list of values that matched
(and those that didn\'t). The arguments are:

* The order requirements (see 'ArgOrder')

* The option descriptions (see 'OptDescr')

* The actual command line arguments (presumably got from
  'System.Environment.getArgs').

'getOpt' returns a triple consisting of the option arguments, a list
of non-options, and a list of error messages.
-}
getOpt :: ArgOrder a                   -- non-option handling
       -> [OptDescr a]                 -- option descriptors
       -> [String]                     -- the command-line arguments
       -> ([a],[String],[String])      -- (options,non-options,error messages)
getOpt ordering optDescr args = (os,xs,es ++ map errUnrec us)
   where (os,xs,us,es) = getOpt' ordering optDescr args

{-|
This is almost the same as 'getOpt', but returns a quadruple
consisting of the option arguments, a list of non-options, a list of
unrecognized options, and a list of error messages.
-}
getOpt' :: ArgOrder a                         -- non-option handling
        -> [OptDescr a]                       -- option descriptors
        -> [String]                           -- the command-line arguments
        -> ([a],[String], [String] ,[String]) -- (options,non-options,unrecognized,error messages)
getOpt' _        _        []         =  ([],[],[],[])
getOpt' ordering optDescr (arg:args) = procNextOpt opt ordering
   where procNextOpt (Opt o)      _                 = (o:os,xs,us,es)
         procNextOpt (UnreqOpt u) _                 = (os,xs,u:us,es)
         procNextOpt (NonOpt x)   RequireOrder      = ([],x:rest,[],[])
         procNextOpt (NonOpt x)   Permute           = (os,x:xs,us,es)
         procNextOpt (NonOpt x)   (ReturnInOrder f) = (f x :os, xs,us,es)
         procNextOpt EndOfOpts    RequireOrder      = ([],rest,[],[])
         procNextOpt EndOfOpts    Permute           = ([],rest,[],[])
         procNextOpt EndOfOpts    (ReturnInOrder f) = (map f rest,[],[],[])
         procNextOpt (OptErr e)   _                 = (os,xs,us,e:es)

         (opt,rest) = getNext arg args optDescr
         (os,xs,us,es) = getOpt' ordering optDescr rest

-- take a look at the next cmd line arg and decide what to do with it
getNext :: String -> [String] -> [OptDescr a] -> (OptKind a,[String])
getNext ('-':'-':[]) rest _        = (EndOfOpts,rest)
getNext ('-':'-':xs) rest optDescr = longOpt xs rest optDescr
getNext ('-': x :xs) rest optDescr = shortOpt x xs rest optDescr
getNext a            rest _        = (NonOpt a,rest)

-- handle long option
longOpt :: String -> [String] -> [OptDescr a] -> (OptKind a,[String])
longOpt ls rs optDescr = long ads arg rs
   where (opt,arg) = break (=='=') ls
         getWith p = [ o  | o@(Option _ xs _ _) <- optDescr
                          , find (p opt) xs /= Nothing]
         exact     = getWith (==)
         options   = if null exact then getWith isPrefixOf else exact
         ads       = [ ad | Option _ _ ad _ <- options ]
         optStr    = ("--"++opt)

         long (_:_:_)      _        rest     = (errAmbig options optStr,rest)
         long [NoArg  a  ] []       rest     = (Opt a,rest)
         long [NoArg  _  ] ('=':_)  rest     = (errNoArg optStr,rest)
         long [ReqArg _ d] []       []       = (errReq d optStr,[])
         long [ReqArg f _] []       (r:rest) = (Opt (f r),rest)
         long [ReqArg f _] ('=':xs) rest     = (Opt (f xs),rest)
         long [OptArg f _] []       rest     = (Opt (f Nothing),rest)
         long [OptArg f _] ('=':xs) rest     = (Opt (f (Just xs)),rest)
         long _            _        rest     = (UnreqOpt ("--"++ls),rest)

-- handle short option
shortOpt :: Char -> String -> [String] -> [OptDescr a] -> (OptKind a,[String])
shortOpt y ys rs optDescr = short ads ys rs
  where options = [ o  | o@(Option ss _ _ _) <- optDescr, s <- ss, y == s ]
        ads     = [ ad | Option _ _ ad _ <- options ]
        optStr  = '-':[y]

        short (_:_:_)        _  rest     = (errAmbig options optStr,rest)
        short (NoArg  a  :_) [] rest     = (Opt a,rest)
        short (NoArg  a  :_) xs rest     = (Opt a,('-':xs):rest)
        short (ReqArg _ d:_) [] []       = (errReq d optStr,[])
        short (ReqArg f _:_) [] (r:rest) = (Opt (f r),rest)
        short (ReqArg f _:_) xs rest     = (Opt (f xs),rest)
        short (OptArg f _:_) [] rest     = (Opt (f Nothing),rest)
        short (OptArg f _:_) xs rest     = (Opt (f (Just xs)),rest)
        short []             [] rest     = (UnreqOpt optStr,rest)
        short []             xs rest     = (UnreqOpt (optStr++xs),rest)

-- miscellaneous error formatting

errAmbig :: [OptDescr a] -> String -> OptKind a
errAmbig ods optStr = OptErr (usageInfo header ods)
   where header = "option `" ++ optStr ++ "' is ambiguous; could be one of:"

errReq :: String -> String -> OptKind a
errReq d optStr = OptErr ("option `" ++ optStr ++ "' requires an argument " ++ d ++ "\n")

errUnrec :: String -> String
errUnrec optStr = "unrecognized option `" ++ optStr ++ "'\n"

errNoArg :: String -> OptKind a
errNoArg optStr = OptErr ("option `" ++ optStr ++ "' doesn't allow an argument\n")

{-
-----------------------------------------------------------------------------------------
-- and here a small and hopefully enlightening example:

data Flag = Verbose | Version | Name String | Output String | Arg String   deriving Show

options :: [OptDescr Flag]
options =
   [Option ['v']     ["verbose"]           (NoArg Verbose)      "verbosely list files",
    Option ['V','?'] ["version","release"] (NoArg Version)      "show version info",
    Option ['o']     ["output"]            (OptArg out "FILE")  "use FILE for dump",
    Option ['n']     ["name"]              (ReqArg Name "USER") "only dump USER's files"]

out :: Maybe String -> Flag
out Nothing  = Output "stdout"
out (Just o) = Output o

test :: ArgOrder Flag -> [String] -> String
test order cmdline = case getOpt order options cmdline of
                        (o,n,[]  ) -> "options=" ++ show o ++ "  args=" ++ show n ++ "\n"
                        (_,_,errs) -> concat errs ++ usageInfo header options
   where header = "Usage: foobar [OPTION...] files..."

-- example runs:
-- putStr (test RequireOrder ["foo","-v"])
--    ==> options=[]  args=["foo", "-v"]
-- putStr (test Permute ["foo","-v"])
--    ==> options=[Verbose]  args=["foo"]
-- putStr (test (ReturnInOrder Arg) ["foo","-v"])
--    ==> options=[Arg "foo", Verbose]  args=[]
-- putStr (test Permute ["foo","--","-v"])
--    ==> options=[]  args=["foo", "-v"]
-- putStr (test Permute ["-?o","--name","bar","--na=baz"])
--    ==> options=[Version, Output "stdout", Name "bar", Name "baz"]  args=[]
-- putStr (test Permute ["--ver","foo"])
--    ==> option `--ver' is ambiguous; could be one of:
--          -v      --verbose             verbosely list files
--          -V, -?  --version, --release  show version info
--        Usage: foobar [OPTION...] files...
--          -v        --verbose             verbosely list files
--          -V, -?    --version, --release  show version info
--          -o[FILE]  --output[=FILE]       use FILE for dump
--          -n USER   --name=USER           only dump USER's files
-----------------------------------------------------------------------------------------
-}

{- $example

To hopefully illuminate the role of the different data
structures, here\'s the command-line options for a (very simple)
compiler:

>    module Opts where
>
>    import Distribution.GetOpt
>    import Data.Maybe ( fromMaybe )
>
>    data Flag
>     = Verbose  | Version
>     | Input String | Output String | LibDir String
>       deriving Show
>
>    options :: [OptDescr Flag]
>    options =
>     [ Option ['v']     ["verbose"] (NoArg Verbose)       "chatty output on stderr"
>     , Option ['V','?'] ["version"] (NoArg Version)       "show version number"
>     , Option ['o']     ["output"]  (OptArg outp "FILE")  "output FILE"
>     , Option ['c']     []          (OptArg inp  "FILE")  "input FILE"
>     , Option ['L']     ["libdir"]  (ReqArg LibDir "DIR") "library directory"
>     ]
>
>    inp,outp :: Maybe String -> Flag
>    outp = Output . fromMaybe "stdout"
>    inp  = Input  . fromMaybe "stdin"
>
>    compilerOpts :: [String] -> IO ([Flag], [String])
>    compilerOpts argv =
>       case getOpt Permute options argv of
>          (o,n,[]  ) -> return (o,n)
>          (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
>      where header = "Usage: ic [OPTION...] files..."

-}
