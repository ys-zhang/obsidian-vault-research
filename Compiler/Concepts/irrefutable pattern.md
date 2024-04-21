#compiler #functional-programming 

_irrefutable pattern_ is related to solve the problem of transpiling _let(rec) expressions_ to expression in simple lambda calculus.

> refute means disprove, i.e., prove a statement or theory is wrong

# The conformality problem 

> conformality (共形) refers to the property of a collection of objects having the same shape 

_Not all patterns can on the LHS of let binding_
```haskell
let (x:xs) = someList 
in  ...
```
pattern in the above let expression will not match if `someList` evaluates to `[]`.

# Definition

_Irrefutable patterns_ consist of arbitrarily nested product constructors with variables at the leaves. _These patterns cannot fail to match in a type-checked implementation_.

>[!def] irrefutable pattern
> A pattern $p$ is **irrefutable** if it is
> 1. either a _variable_ $v$
> 2. or a _product pattern_ of form $(t \; p_1\; \dots \; p_r)$ where $p_1$, ..., $p_r$ are _irrefutable patterns_.
>
> where _sum constructors_ are **not** _product patterns_ 

>[!example] refutable patterns
> - constant patterns
> - sum constructors

# Lazy patterns
#Haskell 

In Haskell in default _patten matching_ is **strict**, actually all pattern matching is compiled to _case expression_ in _Core_ level which forces the inspected expression to WHNF.

You can avoid _strict_ pattern matching by using _lazy patterns_ also dubbed as _irrefutable patterns_, which in Haskell is a pattern _prefixed by `~`_.  

>[!note] 
>the _lazy patterns_ are called _irrefutable patterns_ because these patterns must be irrefutable.
> If they are not, then its possible the matching fails, thus the compiler has to evaluate the expression under inspection to choose which case branch to execute, making the pattern matching strict.

For instance
```haskell
f1 :: Either e Int -> Int
f1 ~(Right 1) = 42

f1' :: Either e Int -> Int
f1' (Right 1) = 42

-- >>> f1 (Right 2)
-- 42
-- >>> f1 (Left 1)
-- 42
-- >>> f1 (Right $ error "Will not panic")
-- 42
-- >>> f1 (error "Will not panic")
-- 42
```

Recall the definition of strict,

>[!note] lazy & strict
> A function $f$ is strict iff
> $$ f(\bot) = \bot $$ 
>  while _lazy_ means evaluation only happens if the input is demanded.

The pattern matching of `f1` lazy, as `f1 undefined = 42`; on the other hand, `f1'` is strict as `f1' undefined = undefined`.

## Lazy and Strict `State` monad

functor instance:
```haskell
-- lazy state monad
instance (Functor m) => Functor (StateT s m) where 
  fmap f m = StateT $ \ s -> 
    fmap (\ ~(a, s') -> (f a, s')) $ runStateT m s 
-- strick state monad
instance (Functor m) => Functor (StateT s m) where 
  fmap f m = StateT $ \ s -> 
    fmap (\(a, s') -> (f a, s')) $ runStateT m s 
```

monad instance:
```haskell
-- lazy monad
instance (Functor m, Monad m) => Applicative (StateT s m) where 
  pure a = StateT $ \ s -> return (a, s) 
  StateT mf <*> StateT mx = StateT $ \ s -> do 
    ~(f, s') <- mf s 
    ~(x, s'') <- mx s' 
    return (f x, s'')

-- strict monad
instance (Functor m, Monad m) => Applicative (StateT s m) where 
  pure a = StateT $ \ s -> return (a, s) 
  StateT mf <*> StateT mx = StateT $ \ s -> do 
    (f, s') <- mf s 
    (x, s'') <- mx s' 
    return (f x, s'')
```
