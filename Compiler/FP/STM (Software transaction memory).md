#Haskell #functional-programming #concurrency #transaction


# Locks & Conditional Variables Do Not Compose

suppose a get function
```
Item get() { atomic (n_items > 0) {...remove item...} }
```

>How could we take two **consecutive** items? We cannot call `get(); get()`, because another thread might perform an intervening get.
>
>We could try wrapping two calls to get in a nested atomic block, but the semantics of this are unclear unless the outer block checks there are two items in the buffer. This is a disaster for abstraction, because the client (who wants to get the two items) has to know about the internal details of the implementation.[^2]

# Primitives


## `IO` Monad, `IORef`

```haskell
-- Data.IORef

-- | `IORef a` is a mutable storage cell 
--   which can hold values of type a
data IORef a 

newIORef   :: a -> IO (IORef a) 
readIORef  :: IORef a -> IO a 
writeIORef :: IORef a -> a -> IO ()
```
A value of type `IORef t` should be thought of as a pointer, or reference, to a mutable location containing a value of type `t`, a bit like the type `(t *)` in C.

> primitives of `IORef a` do not guarantee run in the order that they are used


##  Transactions:  `STM` Monad and `TVar`

> An `STM` action is like an `IO` action, in that it can have side effects, but the range of side effects for `STM` actions is much smaller. The main thing you can do in an `STM` action is to read or write a **transactional variable**, of type (`TVar a`), much as we could read or write `IORef`s in an `IO` action

```haskell
data STM a

-- | atomicity, this function try perform the transaction 
--   represented by the imput atomically.
--   N.B. this is the only way to escape STM
atomically :: STM a -> IO a
-- | if a retry action is performed, 
--   the current/whole transaction is abandoned 
--   and retried at some later time.
retry :: STM a
check :: Bool -> STM ()
check True = return ()
check False = retry
-- | If the first action completes without retrying
--   then it forms the result of the orElse. 
--
--   Otherwise, if the first action retries, 
--   then the second action is tried in its place. 
--
--   If both actions retry then the orElse as a whole retries.
orElse :: STM a -> STM a -> STM a


-- Transactional variables 
data TVar a 

newTVar   :: a -> STM (TVar a) 
readTVar  :: TVar a -> STM a 
writeTVar :: TVar a -> a -> STM ()
```


## Threading

```haskell
forkIO :: IO () 
       -- ^ action to run on the forked thread
       -> IO ThreadId
```


# An Bank Transaction Example

The example[^1] can be stated as writing an bank account system supporting
```haskell
transfer :: Account -> Account -> Money
--          ^ from     ^ to       ^ amount
```
such that _no intermediate state can be observed_:

> No thread should be able to observe a state in which the money has left one account, but not arrived in the other (or vice versa)

or  transactions must be _atomic_.





[^1]: Jones, S. P. (2007). Beautiful concurrency. _Beautiful Code: Leading Programmers Explain How They Think_, 385-406.
[^2]: Harris, T., Marlow, S., Peyton-Jones, S., & Herlihy, M. (2005, June). Composable memory transactions. In _Proceedings of the tenth ACM SIGPLAN symposium on Principles and practice of parallel programming_ (pp. 48-60).