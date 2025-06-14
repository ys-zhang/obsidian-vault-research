#Haskell  #concurrency #parallel

# GHC Config

compile with the `-threaded` GHC option:
```
ghc --make -threaded ParallelMain.hs
```
run with `-N[num-of-native-threads]` run time option 
```
# use 3 native threads
ParallelMain +RTS -N3
```

>[!note] Haskell logical thread
> Haskell threads are logical threads, or green threads

# Semi-explicit Parallelism 

There are 2 primitive functions (`par` and `pseq`) that generate the whole world of parallel programming in Haskell

```haskell
par :: a -> b -> b
pseq :: a -> b -> b
```
## `par` for parallel 

> The function `par` _indicates_ to the Haskell run-time system that it _may be_ beneficial to evaluate the first argument in parallel with the second argument.
> 
> We call such programs **semi-explicitly parallel** because the programmer has provided a hint about the appropriate level of _granularity for parallel operations_ and the system implicitly creates threads to implement the concurrency.


consider the expression `par a b`, if evaluation of `b` depends on `a` then `b`'s evaluation will pause when try to evaluate `a`, i.e., a _barrier is created_ at the position of evaluation dependency.  

>[!warning] Lazy future
>The Haskell run-time system _does not necessarily create a thread_ to compute the value of the expression `a`.
> 
>Instead, the run-time system creates a **spark** which has the _potential to be executed on a different thread_ from the parent thread. 
>
>A **sparked computation** expresses the possibility of performing some speculative evaluation. Since a thread is not necessarily created to compute the value of a this approach has some similarities with the notion of a lazy future

## `pseq`: force order of evaluation

to understand `pseq` we first consider `seq`:
```
seq :: a -> b -> b
```

> The value of `seq a b` is bottom if `a` is bottom, and otherwise equal to `b`. In other words, it evaluates the first argument `a` to weak head normal form (WHNF). `seq` is usually introduced to improve performance by avoiding unneeded laziness.
> 
> A note on evaluation order: the expression `seq a b` does _not_ guarantee that `a` will be evaluated before `b`. The only guarantee given by `seq` is that the both `a` and `b` will be evaluated before `seq` returns a value. In particular, this means that `b` may be evaluated before `a`. If you need to guarantee a specific order of evaluation, you must use the function `pseq` from the "parallel" package.

thus `pseq a b` is like `seq a b`, it evaluates `a` to WHNF, the difference is that its guarantees `a` is evaluated before `b`
```haskell
-- parallel computes f and e then add the 2 results
f :: Double -- some expensive computation
e :: Double -- some other expensive computation

-- avoiding evaluate `f` first in `f+e`, which forces the 
-- the parent thread to wait on child thread
par f (e `pseq` (f + e))
-- ^ f is evaluated in child thread
--          ^ in parent thread, try evaluate `e` first in `f + e`
```
![[Pasted image 20230516162241.png|center]]

# Explicit Parallelism

```haskell
forkIO :: IO () -> IO ThreadId
MVar     -- mutable var
TVar     -- transactional var
```

# Data Parallelism

Some references:
- [Haskell-Wiki on data parallel](https://wiki.haskell.org/GHC/Data_Parallel_Haskell)
- [GHC Language extension on parallel & concurrency](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/parallel.html)

Modules and Packages:
- `Data.Array.Parallel`
- [repa](https://hackage.haskell.org/package/repa)


## Flat Data Parallelism

Apply the same sequential function `f`, in parallel, to every element of a large collection of values a. Not only is `f` sequential, but it has a similar run-time for each element of the collection.


## Nested Data Parallelism

Apply the same function f, in parallel, to every element of a large collection of values a. However, f may itself be a (nested) data-parallel function, and does not need to have a similar run-time for each element of a.








