> -- |
> -- Module      :  Distribution.Compat.MSem
> -- Copyright   :  (c) Chris Kuklewicz 2011
> -- License     :  3 clause BSD-style (see the file LICENSE)
> --
> -- Maintainer  :  haskell@list.mightyreason.com
> -- Stability   :  experimental
> -- Portability :  non-portable (concurrency)
> --
> -- This is a literate haskell version of Control.Concurrent.MSem for increased clarity.
> --
> -- A semaphore in which operations may 'wait' for or 'signal' single units of value.  This modules
> -- is intended to improve on "Control.Concurrent.QSem".
> --
> -- This semaphore gracefully handles threads which die while blocked waiting.  The fairness
> -- guarantee is that blocked threads are servied in a FIFO order.
> --
> -- If 'with' is used to guard a critical section then no quantity of the semaphore will be lost if
> -- the activity throws an exception or if this thread is killed by the rest of the program.
> --
> -- 'new' can initialize the semaphore to negative, zero, or positive quantity.
> -- 'wait' always leaves the 'MSem' with non-negative quantity.
> -- 'signal' alawys adds one to the quantity.
> --
> -- The functions below are generic in (Integral i) with specialization to Int, Word, and Integer.
> --
> -- Overflow warning: These operations do not check for overflow errors.  If the Integral type is too
> -- small to accept the new total then the behavior of 'signal' is undefined.  Using (MSem
> -- Integer) prevents the possibility of an overflow error.  [ A version of 'signal' that checks the upper
> -- bound could be added, but how would it report failure and how would you use this sanely? ]
> --
>
> module Distribution.Compat.MSem
>     (MSem       -- do not export the constructor, kept abstract
>     , new       -- :: Integral i => i -> IO (MSem i)
>     , with      -- :: Integral i => MSem i -> IO a -> IO a
>     , wait      -- :: Integral i => MSem i -> IO ()
>     , signal    -- :: Integral i => MSem i -> IO ()
>     , peekAvail -- :: Integral i => MSem i -> IO i
>     ) where

The above export list shows the API.

The amount of value in the orignal QSem is always of type Int.  This module
generalizes the type to any Integral, where comparison (<) to 'fromIntegral 0'
and 'pred' and 'succ' are employed.

The 'new', 'wait', and 'signal' operations mimic the QSem API.  The peekAvail
query is also provided, primarily for monitoring or debugging purposes.  The
with combinator is used to safely and conveniently bracket operations.

> import Prelude( Integral,Eq,IO,Int,Integer,Maybe(Just,Nothing)
>               , seq,pred,succ,return
>               , (.),(<),($),($!) )
> import Control.Concurrent.MVar( MVar
>                               , withMVar,modifyMVar,modifyMVar_,tryPutMVar
>                               , newMVar,newEmptyMVar,putMVar,takeMVar,tryTakeMVar)
> import Control.Exception(bracket_,uninterruptibleMask_,mask_)
> import Control.Monad(join)
> import Data.Word(Word)

The import list shows that most of the power of MVar's will be exploited, and
that the rather dangerous uninterruptibleMask_ will be employed (in 'signal').

A new semaphore is created with a specified avaiable quantity.  The mutable
available quantity will be called the value of the semaphore for brevity's
sake.

The use of a semaphore involves multiple threads executing 'wait' and 'signal'
commands.  This stream of wait and 'signal' commands will be executed as if
they arrive in some sequential, non-overlapping, order which is an interleaving
of the commands from each thread.

From the local perspective of a single thread the semantics are simple to
specify. The 'signal' command will find the MSem to have a value and mutate
this to add one to the value. The 'wait' command will find the MSem to have a
value and if this is greater than zero it will mutate this to be one less and
finish, otherwise the value is negative or zero and the execution of the 'wait'
thread will block.  Eventually another thread executes 'signal' and raises the
value to be positive, at this point the blocked 'wait' thread will reduce the
value by one and finish executing the 'wait' command.

From a broader perspective there is a question of precedence and starvation.
If there is a blocked wait thread and a second 'wait' command starts to execute
then will the second thread "find the MSem to have a value" before or after the
orignal blocked thread has finished?  If there are several blocked 'wait'
threads and a 'signal' arrives then which blocked thread has priority to take
the quatity and finish waiting?  Are there any fairness guarantees or might a
blocked thread never get priority over its bretheren leading to starvation?

I have designed this module to provide a fair semaphore: multiple 'wait'
threads are serviced in FIFO order.  All 'signal' operations, while they may
block, are individually quick.

There are precisely three components, all MVars alloced by 'new': queueWait,
quantityStore, and headWait.

1) The 'wait' operations are forced into a FIFO queue by taking an (MVar ())
called queueWait during their operation.  The thread holding this token is the
"head" waiter.

2) The 'signal' operations are forced into a FIFO queue by taking the MVar
called quantityStore which holds an integral value.

3) The logical value stored in the semaphore might be represented by one of two
different states of the semaphore data structure, depending on whether
'headWait :: MVar ()' is empty or full.  In this module a full headWait
reprents a single unit of value stored in the semaphore.

> -- | A 'MSem' is a semaphore in which the available quantity can be added and removed in single
> --  units, and which can start with positive, zero, or negative value.
> data MSem i = MSem { quantityStore :: !(MVar i)  -- ^ Used to lock access to state of semaphore quantity. Never updated.
>                    , queueWait :: !(MVar ()) -- ^ Used as FIFO queue for waiter, held by head of queue.  Never updated.
>                    , headWait :: !(MVar ())  -- ^ The head of the waiter queue blocks on headWait. Never updated.
>                    }
>   deriving (Eq)
>
> -- |'new' allows positive, zero, and negative initial values.  The initial value is forced here to
> -- better localize errors.
> --
> -- The only way to achieve a negative value with MSem is to start negative with 'new'.  Once a negative quantity becomes non-negative
> -- by use of 'signal' it will never later be negative.
> new :: Integral i => i -> IO (MSem i)
> {-# SPECIALIZE new :: Int -> IO (MSem Int) #-}
> {-# SPECIALIZE new :: Word -> IO (MSem Word) #-}
> {-# SPECIALIZE new :: Integer -> IO (MSem Integer) #-}
> new initial = do
>   newQuantityStore <- newMVar $! initial
>   newQueueWait <- newMVar ()
>   newHeadWait <- newEmptyMVar
>   return (MSem { quantityStore = newQuantityStore
>                , queueWait = newQueueWait
>                , headWait = newHeadWait })
>

Note that the only MVars that get allocated are all by these three commands in
'new'.  The other commands change the stored values but do not allocate new
mutable storage.  None of these three MVars can be simply replaced by an IORef
because the possibility of blocking on each of them is used in the design.  A
design with two MVar is possible but I think it would have more contention
between threads and be more complex to ensure thread safety.

There are four operations on the semaphore leading to two possible states for
headWait:

1) If the most recent operation to finish was 'new' then headWait is definitely
empty and the value of the MSem is the quantity in quantityStore.

2) If the most recent operation to finish was 'wait' then headWait is
definitely empty and the value of the MSem is the quantity in quantityStore.

3) If the most recent operation to finish was a 'signal' and the new value is
positive then headWait is definitely full and the value of the MSem is the
quantity in quantityStore PLUS ONE.

4) If the most recent operation to finish was a 'signal' and the new value is
non-positive then headWait is definitely empty and the value of the MSem is the
quantity in quantityStore.

If the "head" 'wait' thread finds a non-positive value then it will need to
sleep until being awakened by a future 'signal'.  This sleeping is accomplished
by the head waiter taking an empty headWait.

All uses of the semaphore API to guard execution of an action should use 'with'
to simplify ensuring exceptions are safely handled.  Other uses should use
still try and use combinators in Control.Exception to ensure that no 'signal'
commands get lost so that no quantity of the semaphore leaks when exceptions
occur.

> -- | 'with' takes a unit of value from the semaphore to hold while performing the provided
> -- operation.  'with' ensures the quantity of the sempahore cannot be lost if there are exceptions or
> -- if killThread is used.
> --
> -- 'with' uses 'bracket_' to ensure 'wait' and 'signal' get called correctly.
> with :: Integral i => MSem i -> IO a -> IO a
> {-# SPECIALIZE with :: MSem Int -> IO a -> IO a #-}
> {-# SPECIALIZE with :: MSem Word -> IO a -> IO a #-}
> {-# SPECIALIZE with :: MSem Integer -> IO a -> IO a #-}
> with m = bracket_ (wait m)  (signal m)

> -- |'wait' will take one unit of value from the sempahore, but will block if the quantity available
> -- is not positive.
> --
> -- If 'wait' returns normally (not interrupted) then it left the 'MSem' with a remaining quantity that was
> -- greater than or equal to zero.  If 'wait' is interrupted then no quantity is lost.  If 'wait'
> -- returns without interruption then it is known that each earlier waiter has definitely either been
> -- interrupted or has retured without interruption (the FIFO guarantee).
> wait :: Integral i => MSem i -> IO ()
> {-# SPECIALIZE wait :: MSem Int -> IO () #-}
> {-# SPECIALIZE wait :: MSem Word -> IO () #-}
> {-# SPECIALIZE wait :: MSem Integer -> IO () #-}
> wait m = mask_ . withMVar (queueWait m) $ \ () -> do
>   join . modifyMVar (quantityStore m) $ \ quantity -> do
>     mayGrab <- tryTakeMVar (headWait m) -- First try optimistic grab on (headWait w)
>     case mayGrab of
>       Just () -> return (quantity,return ())  -- Took unit of value, done
>       Nothing -> if 0 < quantity              -- Did not take unit of value, check quantity
>                    then let quantity' = pred quantity -- quantity' is never negative
>                         in seq quantity' $ return (quantity', return ())
>                    else return (quantity, takeMVar (headWait m)) -- go to sleep

The needed invariant is that 'wait' takes a unit of value iff it returns
normally (i.e. it is not interrupted).  The 'mask_' is needed above because we
may decrement 'headWait' with 'tryTakeMVar' and must then finished the
'withMVar' without being interrupted.  Under the 'mask_' the 'wait' might block
and then be interruptable at one or more of

1) 'withMVar (queueWait m)' : the 'wait' dies before becoming head waiter while
blocked by previous 'wait'.

2) 'modifyMVar (quantityStore m)' : the 'wait' dies as head waiter while
blocked by previous 'signal'.

3) 'takeMVar (headWait m)' from 'join' : the 'wait' dies as head waiter while
sleeping on 'headWait'.

All three of those are safe places to die.  The unsafe possibilities would be
to die after a 'tryTakeMVar (headWait m)' returns 'Just ()' or after
'modifyMVar' puts the decremented quantity into (quantityStore m).  These are
prevented by the 'mask_'.

Note that the head waiter must also get to the front of the FIFO queue of
signals to get the value of 'quantityStore'.  Only the head waiter competes
with the 'signal' & peek threads for obtaining 'quantityStore'.

> -- | 'signal' adds one unit to the sempahore.  Overflow is not checked.
> --
> -- 'signal' may block, but it cannot be interrupted, which allows it to dependably restore value to
> -- the 'MSem'.  All 'signal', 'peekAvail', and the head waiter may momentarily block in a fair FIFO
> -- manner.
> signal :: Integral i => MSem i -> IO ()
> {-# SPECIALIZE signal :: MSem Int -> IO () #-}
> {-# SPECIALIZE signal :: MSem Word -> IO () #-}
> {-# SPECIALIZE signal :: MSem Integer -> IO () #-}
> signal m = uninterruptibleMask_ . modifyMVar_ (quantityStore m) $ \ quantity -> do
>   if quantity < 0
>     then return $! succ quantity
>     else do
>       didPlace <- tryPutMVar (headWait m) ()  -- quantity is never negative
>       if didPlace
>         then return quantity
>         else return $! succ quantity

The 'signal' operation first has the FIFO grab of (quantityStore m).  If
'tryPutMVar' returns True then a currently sleeping head waiter will be woken
up.

The 'modifyMVar_' will block until prior 'signal' and 'peek' threads and
perhaps a prior head 'wait' finish.  This is the only point that may block.
Thus 'uninterruptibleMask_' only differs from 'mask_' in that once 'signal'
starts executing it cannot be interrupted before returning the unit of value to
the MSem.  All the operations 'signal' would be waiting for are quick and are
themselves non-blocking, so the uninterruptible operation here should finish
without arbitrary delay.

Consider 'with m act = bracket_ (wait m) (signal m) act', refer to
http://www.haskell.org/ghc/docs/latest/html/libraries/base/src/Control-Exception-Base.html#bracket_
for the details.  Specifically a killThread arrives at one of these points:

1) during (wait m) the exception is masked by both 'bracket' and 'wait' so this
occurs at one of the blocking points mentioned above.  This does not affect the
MSe, and aborts the 'bracket_' without calling act or (signal m).

2) during (restore act) the `onException` in the definition of 'bracket' will
shift control to (signal m).

3) during (signal m) regardless of how act exited.  Here we know (wait m)
exited normally and thus took a unit of value from the MSem.  The mask_ of
'bracket' ensures that the uninterruptibleMask_ in 'signal' ensures that the
unit of value is returned to MSem even if 'signal' blocks on 'modifyMVar_
(quantityStore m)'.

4) Outside of any of the above the mask_ in 'bracket' prevents the killThread
from being recognized until one of the above or until the 'bracket' finishes.

If 'signal' did not use 'uninterruptibleMask_' then point (3) could be
interrupted without returning the value to the MSem.  Avoiding losing quantity
is the primary design criterion for this semaphore library, and I think it
requires this apparantly safe use of uninterruptibleMask_ to ensure that
'signal' can and will succeed.

> -- | 'peekAvail' skips the queue of any blocked 'wait' threads, but may momentarily block on
> -- 'signal', other 'peekAvail', and the head waiter. This returns the amount of value available to
> -- be taken.  Using this value without producing unwanted race conditions is left up to the
> -- programmer.
> --
> -- Note that "Control.Concurrent.MSemN" offers a more powerful API for making decisions based on the
> -- available amount.
> peekAvail :: Integral i => MSem i -> IO i
> {-# SPECIALIZE peekAvail :: MSem Int -> IO Int #-}
> {-# SPECIALIZE peekAvail :: MSem Word -> IO Word #-}
> {-# SPECIALIZE peekAvail :: MSem Integer -> IO Integer #-}
> peekAvail m = mask_ $ withMVar (quantityStore m) $ \ quantity -> do
>   extraFlag <- tryTakeMVar (headWait m)
>   case extraFlag of
>     Nothing -> return quantity
>     Just () -> do putMVar (headWait m) () -- cannot block
>                   return $! succ quantity

The implementaion of peekAvail is slightly complicated by the interplay of
tryTakeMVar and putMVar.  Only this thread will be holding the lock on
quantityStore and the putMVar only runs to put a () just taken from headWait.
Thus the putMVar will never block.  The 'mask_' ensures that there can be no
external interruption between a tryTakeMVar and putMVar.
