HUnitText.lhs  --  text-based test controller

$Id: HUnitText.lhs,v 1.9 2002/02/21 16:50:27 heringto Exp $

> module HUnitText
> (
>   PutText(..),
>   putTextToHandle, putTextToShowS,
>   runTestText,
>   showPath, showCounts,
>   runTestTT
> )
> where

> import HUnitBase

> import Monad (when)
> import IO (Handle, stderr, hPutStr, hPutStrLn)


As the general text-based test controller (`runTestText`) executes a
test, it reports each test case start, error, and failure by
constructing a string and passing it to the function embodied in a
`PutText`.  A report string is known as a "line", although it includes
no line terminator; the function in a `PutText` is responsible for
terminating lines appropriately.  Besides the line, the function
receives a flag indicating the intended "persistence" of the line:
`True` indicates that the line should be part of the final overall
report; `False` indicates that the line merely indicates progress of
the test execution.  Each progress line shows the current values of
the cumulative test execution counts; a final, persistent line shows
the final count values.

The `PutText` function is also passed, and returns, an arbitrary state
value (called `st` here).  The initial state value is given in the
`PutText`; the final value is returned by `runTestText`.

> data PutText st = PutText (String -> Bool -> st -> IO st) st


Two reporting schemes are defined here.  `putTextToHandle` writes
report lines to a given handle.  `putTextToShowS` accumulates
persistent lines for return as a whole by `runTestText`.


`putTextToHandle` writes persistent lines to the given handle,
following each by a newline character.  In addition, if the given flag
is `True`, it writes progress lines to the handle as well.  A progress
line is written with no line termination, so that it can be
overwritten by the next report line.  As overwriting involves writing
carriage return and blank characters, its proper effect is usually
only obtained on terminal devices.

> putTextToHandle :: Handle -> Bool -> PutText Int
> putTextToHandle handle showProgress = PutText put initCnt
>  where
>   initCnt = if showProgress then 0 else -1
>   put line pers (-1) = do when pers (hPutStrLn handle line); return (-1)
>   put line True  cnt = do hPutStrLn handle (erase cnt ++ line); return 0
>   put line False cnt = do hPutStr handle ('\r' : line); return (length line)
>     -- The "erasing" strategy with a single '\r' relies on the fact that the
>     -- lengths of successive summary lines are monotonically nondecreasing.
>   erase cnt = if cnt == 0 then "" else "\r" ++ replicate cnt ' ' ++ "\r"


`putTextToShowS` accumulates persistent lines (dropping progess lines)
for return by `runTestText`.  The accumulated lines are represented by
a `ShowS` (`String -> String`) function whose first argument is the
string to be appended to the accumulated report lines.

> putTextToShowS :: PutText ShowS
> putTextToShowS = PutText put id
>  where put line pers f = return (if pers then acc f line else f)
>        acc f line tail = f (line ++ '\n' : tail)


`runTestText` executes a test, processing each report line according
to the given reporting scheme.  The reporting scheme's state is
threaded through calls to the reporting scheme's function and finally
returned, along with final count values.

> runTestText :: PutText st -> Test -> IO (Counts, st)
> runTestText (PutText put us) t = do
>   (counts, us') <- performTest reportStart reportError reportFailure us t
>   us'' <- put (showCounts counts) True us'
>   return (counts, us'')
>  where
>   reportStart ss us = put (showCounts (counts ss)) False us
>   reportError   = reportProblem "Error:"   "Error in:   "
>   reportFailure = reportProblem "Failure:" "Failure in: "
>   reportProblem p0 p1 msg ss us = put line True us
>    where line  = "### " ++ kind ++ path' ++ '\n' : msg
>          kind  = if null path' then p0 else p1
>          path' = showPath (path ss)


`showCounts` converts test execution counts to a string.

> showCounts :: Counts -> String
> showCounts Counts{ cases = cases, tried = tried,
>                    errors = errors, failures = failures } =
>   "Cases: " ++ show cases ++ "  Tried: " ++ show tried ++
>   "  Errors: " ++ show errors ++ "  Failures: " ++ show failures


`showPath` converts a test case path to a string, separating adjacent
elements by ':'.  An element of the path is quoted (as with `show`)
when there is potential ambiguity.

> showPath :: Path -> String
> showPath [] = ""
> showPath nodes = foldl1 f (map showNode nodes)
>  where f b a = a ++ ":" ++ b
>        showNode (ListItem n) = show n
>        showNode (Label label) = safe label (show label)
>        safe s ss = if ':' `elem` s || "\"" ++ s ++ "\"" /= ss then ss else s


`runTestTT` provides the "standard" text-based test controller.
Reporting is made to standard error, and progress reports are
included.  For possible programmatic use, the final counts are
returned.  The "TT" in the name suggests "Text-based reporting to the
Terminal".

> runTestTT :: Test -> IO Counts
> runTestTT t = do (counts, 0) <- runTestText (putTextToHandle stderr True) t
>                  return counts
