HUnitBase.lhs  --  basic definitions

$Id: HUnitBase.lhs,v 1.12 2002/02/14 19:31:57 heringto Exp $

> module HUnitBase
> (
>   {- from HUnitLang: -} Assertion, assertFailure,
>   assertString, assertBool, assertEqual,
>   Assertable(..), ListAssertable(..),
>   AssertionPredicate, AssertionPredicable(..),
>   (@?), (@=?), (@?=),
>   Test(..), Node(..), Path,
>   testCaseCount,
>   Testable(..),
>   (~?), (~=?), (~?=), (~:),
>   Counts(..), State(..),
>   ReportStart, ReportProblem,
>   testCasePaths,
>   performTest
> )
> where

> import Monad (unless, foldM)


Assertion Definition
====================

> import HUnitLang


Conditional Assertion Functions
-------------------------------

> assertBool :: String -> Bool -> Assertion
> assertBool msg b = unless b (assertFailure msg)

> assertString :: String -> Assertion
> assertString s = unless (null s) (assertFailure s)

> assertEqual :: (Eq a, Show a) => String -> a -> a -> Assertion
> assertEqual preface expected actual =
>   unless (actual == expected) (assertFailure msg)
>  where msg = (if null preface then "" else preface ++ "\n") ++
>              "expected: " ++ show expected ++ "\n but got: " ++ show actual


Overloaded `assert` Function
----------------------------

> class Assertable t
>  where assert :: t -> Assertion

> instance Assertable ()
>  where assert = return

> instance Assertable Bool
>  where assert = assertBool ""

> instance (ListAssertable t) => Assertable [t]
>  where assert = listAssert

> instance (Assertable t) => Assertable (IO t)
>  where assert = (>>= assert)

We define the assertability of `[Char]` (that is, `String`) and leave
other types of list to possible user extension.

> class ListAssertable t
>  where listAssert :: [t] -> Assertion

> instance ListAssertable Char
>  where listAssert = assertString


Overloaded `assertionPredicate` Function
----------------------------------------

> type AssertionPredicate = IO Bool

> class AssertionPredicable t
>  where assertionPredicate :: t -> AssertionPredicate

> instance AssertionPredicable Bool
>  where assertionPredicate = return

> instance (AssertionPredicable t) => AssertionPredicable (IO t)
>  where assertionPredicate = (>>= assertionPredicate)


Assertion Construction Operators
--------------------------------

> infix  1 @?, @=?, @?=

> (@?) :: (AssertionPredicable t) => t -> String -> Assertion
> pred @? msg = assertionPredicate pred >>= assertBool msg

> (@=?) :: (Eq a, Show a) => a -> a -> Assertion
> expected @=? actual = assertEqual "" expected actual

> (@?=) :: (Eq a, Show a) => a -> a -> Assertion
> actual @?= expected = assertEqual "" expected actual



Test Definition
===============

> data Test = TestCase Assertion
>           | TestList [Test]
>           | TestLabel String Test

> instance Show Test where
>   showsPrec p (TestCase _)    = showString "TestCase _"
>   showsPrec p (TestList ts)   = showString "TestList " . showList ts
>   showsPrec p (TestLabel l t) = showString "TestLabel " . showString l
>                                 . showChar ' ' . showsPrec p t

> testCaseCount :: Test -> Int
> testCaseCount (TestCase _)    = 1
> testCaseCount (TestList ts)   = sum (map testCaseCount ts)
> testCaseCount (TestLabel _ t) = testCaseCount t


> data Node  = ListItem Int | Label String
>   deriving (Eq, Show, Read)

> type Path = [Node]    -- Node order is from test case to root.


> testCasePaths :: Test -> [Path]
> testCasePaths t = tcp t []
>  where tcp (TestCase _) p = [p]
>        tcp (TestList ts) p =
>          concat [ tcp t (ListItem n : p) | (t,n) <- zip ts [0..] ]
>        tcp (TestLabel l t) p = tcp t (Label l : p)


Overloaded `test` Function
--------------------------

> class Testable t
>  where test :: t -> Test

> instance Testable Test
>  where test = id

> instance (Assertable t) => Testable (IO t)
>  where test = TestCase . assert

> instance (Testable t) => Testable [t]
>  where test = TestList . map test


Test Construction Operators
---------------------------

> infix  1 ~?, ~=?, ~?=
> infixr 0 ~:

> (~?) :: (AssertionPredicable t) => t -> String -> Test
> pred ~? msg = TestCase (pred @? msg)

> (~=?) :: (Eq a, Show a) => a -> a -> Test
> expected ~=? actual = TestCase (expected @=? actual)

> (~?=) :: (Eq a, Show a) => a -> a -> Test
> actual ~?= expected = TestCase (actual @?= expected)

> (~:) :: (Testable t) => String -> t -> Test
> label ~: t = TestLabel label (test t)



Test Execution
==============

> data Counts = Counts { cases, tried, errors, failures :: Int }
>   deriving (Eq, Show, Read)

> data State = State { path :: Path, counts :: Counts }
>   deriving (Eq, Show, Read)

> type ReportStart us = State -> us -> IO us

> type ReportProblem us = String -> State -> us -> IO us


Note that the counts in a start report do not include the test case
being started, whereas the counts in a problem report do include the
test case just finished.  The principle is that the counts are sampled
only between test case executions.  As a result, the number of test
case successes always equals the difference of test cases tried and
the sum of test case errors and failures.


> performTest :: ReportStart us -> ReportProblem us -> ReportProblem us
>                  -> us -> Test -> IO (Counts, us)
> performTest reportStart reportError reportFailure us t = do
>   (ss', us') <- pt initState us t
>   unless (null (path ss')) $ error "performTest: Final path is nonnull"
>   return (counts ss', us')
>  where
>   initState  = State{ path = [], counts = initCounts }
>   initCounts = Counts{ cases = testCaseCount t, tried = 0,
>                        errors = 0, failures = 0}

>   pt ss us (TestCase a) = do
>     us' <- reportStart ss us
>     r <- performTestCase a
>     case r of Nothing         -> do return (ss', us')
>               Just (True,  m) -> do usF <- reportFailure m ssF us'
>                                     return (ssF, usF)
>               Just (False, m) -> do usE <- reportError   m ssE us'
>                                     return (ssE, usE)
>    where c@Counts{ tried = t } = counts ss
>          ss' = ss{ counts = c{ tried = t + 1 } }
>          ssF = ss{ counts = c{ tried = t + 1, failures = failures c + 1 } }
>          ssE = ss{ counts = c{ tried = t + 1, errors   = errors   c + 1 } }

>   pt ss us (TestList ts) = foldM f (ss, us) (zip ts [0..])
>    where f (ss, us) (t, n) = withNode (ListItem n) ss us t

>   pt ss us (TestLabel label t) = withNode (Label label) ss us t

>   withNode node ss0 us0 t = do (ss2, us1) <- pt ss1 us0 t
>                                return (ss2{ path = path0 }, us1)
>    where path0 = path ss0
>          ss1 = ss0{ path = node : path0 }
