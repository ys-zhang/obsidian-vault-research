#functional-programming #Haskell #Monad #concurrency 

This paper is about how to handle **_variable mutability_** _(updatable state)_ in Haskell.

**_Parametric polymorphism_** is the key to ensure safe encapsulation of state.

“A stateful computation is a **state transformer**, that is, a function from an initial state to a final state. It is like a script” (Launchbury and Peyton Jones, 1994, p. 1)

“because we guarantee that the state is used in a single threaded way, the final state can be constructed by modifying the input state _in-place_.” (Launchbury and Peyton Jones, 1994, p. 1)

# State transformer

The type `ST s a` (ST stands for state transformer) represents a computation which transforms a state indexed by type `s` and produces a value of type `a`.
![[Pasted image 20230223153159.png]]

A state transformer can be parameterised, i.e. have inputs; and have multiple results

```haskell
Int -> Int -> ST s (Int, Bool)
```
![[Pasted image 20230223153429.png]]


# Mutable References

```haskell
import Controll.Monad.ST
import Data.STRef

{-| `newVar` takes an init value of type `a` to
    a state transformer, which allocates a fresh 
    reference in the input state -}
newSTRef :: a -> ST s (STRef s a)
{-| `readSTRef` takes a STRef to produce a state transformer 
    which leaves the state untouched but read the memory out -}
readSTRef :: STRef s a -> ST s a
{-| `readSTRef` takes a STRef and a value to produce a state transformer 
    which modifies the correspondence memory of the input state -}
writeSTRef :: STRef s a -> a -> ST s ()
modifySTRef :: STRef s a -> (a -> a) -> ST s ()
```

Like references `newArray` creates state transformation which allocates a new array 

```haskell
-- array references
import Data.Array.ST

data STArray s i e

runSTArray :: (forall s. ST s (STArray s i e)) -> Array i e

-- methods are from type class MArray
newArray :: Ix i => (i, i) -> elt -> ST s (STArray s i elt)
readArray :: Ix i => STArray s i e -> i -> ST s e
writeArray :: Ix i => STArray s i e -> i -> e -> ST s ()
-- | freeze converts a mutable array to an unmutable array
freeze :: Ix i => STArray s i e -> ST s (Array i e)
```


# Thread Safety and the type of `runST`

the bind method in `ST s` monad binding state transformers in a sequential order 

![[Pasted image 20230223172005.png]]

```haskell

instance Monad (ST s) where 
  return a = ST $ \s -> (a, s)  -- ^ leaves state untouched
  m >>= k = ST $ \s -> 
    let (a, s1) = runST m s
        (b, s2) = runST (k a) s1 
    in (b, s2)
```

>[!note]
>We often refer the sequence of chained state transformers as a **thread**, invoking a series of primitive stateful operations "threaded together" by a state passed from one to the next

To assure thread safety (prevent race condition, and maintain pure) we need to guarantee `RefST` allocated in some thread should not be accessed from other threads.

```haskell

-- WE NEED TO MAKE THE FOLLOWING ILEGAL
let 
  v :: STRef s a
  v = runST (newSTRef True) -- STRef created in thread A
in 
  runST $ readSTRef v       -- but read in thread B
```

this is done by making 

```haskell

-- this is not thread safe
unsafeRunST :: forall a . forall s . ST s a -> a

-- Notice this is not equiv to ST s a -> a
runST :: forall a . (forall s . ST s a) -> a
```

to see the second type is thread safe
1. `runST $ readSTRef v`  is not type correct, during type checking the type of `readSTRef v` will depend on the type of `v`  
  $$
    \{\dots, \text{v: STRef s Bool}\} \vdash \text{readSTRef v : ST s Bool}
 $$
    thus we cannot generalise `s`. (`s` is constraint through `v`)
2. `newSTRef True :: ST s (STRef s Bool)`  where the 2 `s` are identical thus cannot pass type checking


# `IO`, `RealWorld` and `ST` 

```haskell
type IO a = ST RealWorld a
```

There is only one primitive I/O operation, `ccall` which is part of the language but not a function and allows Haskell to call any C procedure.

```haskell
putChar :: Char -> IO ()
putChar c = ccall "putchar" c >>= \_ -> return ()

getChar :: IO Char
getChar = ccall "getchar"
```

> [!note]
> `IO` monad is not escapable since `IO a = ST RealWord a` does not match the type `forall s . ST s a` 
 
