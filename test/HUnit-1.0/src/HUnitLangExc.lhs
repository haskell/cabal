HUnitLangExc.lhs  --  HUnit language support, using `Exception` type

Note: The Haskell system you use needs to find this file when looking
for module `HUnitLang`.

$Id: HUnitLangExc.lhs,v 1.1 2002/02/14 14:54:34 heringto Exp $

> module HUnitLang
> (
>   Assertion,
>   assertFailure,
>   performTestCase
> )
> where


When adapting this module for other Haskell language systems, change
the imports and the implementations but not the interfaces.



Imports
-------

> import List (isPrefixOf)
> import qualified Exception (try)



Interfaces
----------

An assertion is an `IO` computation with trivial result.

> type Assertion = IO ()

`assertFailure` signals an assertion failure with a given message.

> assertFailure :: String -> Assertion

`performTestCase` performs a single test case.  The meaning of the
result is as follows:
  Nothing               test case success
  Just (True,  msg)     test case failure with the given message
  Just (False, msg)     test case error with the given message

> performTestCase :: Assertion -> IO (Maybe (Bool, String))


Implementations
---------------

> hunitPrefix = "HUnit:"

> hugsPrefix  = "IO Error: User error\nReason: "
> -- GHC prepends no prefix to the user-supplied string.

> assertFailure msg = ioError (userError (hunitPrefix ++ msg))

> performTestCase action = do r <- Exception.try action
>                             case r of Right () -> return Nothing
>                                       Left  e  -> return (Just (decode e))
>  where
>   decode e = let s0 = show e
>                  (_, s1) = dropPrefix hugsPrefix  s0
>              in            dropPrefix hunitPrefix s1
>   dropPrefix pref str = if pref `isPrefixOf` str
>                           then (True, drop (length pref) str)
>                           else (False, str)
