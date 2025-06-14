#Haskell #category-theory #academic-paper #applicative-functor
#selective-functor

# Applicative programming with effects

>[!tldr]
> the basic idea behind the `Applicative` type class is that there is a common pattern in programming of **applying pure function to effective arguments**.


Any expression built from the Applicative combinators can be transformed to a _canonical form_ in which a single pure function is ‘applied’ to the _effectful parts_ in **depth-first order**:

```haskell
pure f <*> u1 <*> u2 <*> ... <*> uN
```

## Difference with `Monad`.

Intuitively, the `(>>=) :: m a -> (a -> m b) -> m b` of some Monad `m` allows the value returned by one computation to influence the choice of another, whereas `(<*>)` keeps the structure of a computation fixed, just sequencing the effects.

>[!note] 
>In `mb = ma >>= mf` the effect of `mf` can be controlled by the value of `a`;
>while in `mb = mf <*> ma` , the effect of `mf` is independent of the value of `a`

An example could be 
```haskell

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM cond true false = do 
  b <- cond 
  if b then true else false

ifA_Bad :: Applicative m => m Bool -> m a -> m a -> m a
ifA_Bad cond true false = 
  pure (\b t f -> if b then t else f)
    <*> true 
    <*> false
```

the applicative version `ifA_Bad` is bad because no matter what the `cond` evaluates to, both `true` and `false`'s effect will be performed.

## Composing Applicative functors

>[!tldr]
> `Applicative` is closed under composition

Unlike monad, any pair applicative functor can compose into a new applicative function

```haskell
newtype Compose f g a = Compose (f (g a)) 

instance (Applicative f, Applicative g) 
  => (Applicative Compose f g) where
  pure = Compose . pure . pure 
  
  -- pure (<*>) :: f (g (a -> b) -> g a -> g b)
  -- h :: f (g (a -> b))
  (Compose h) <*> (Compose x) = Compose (pure (<*>) <*> h <*> y)
```


# Selective Applicative Functors

>[!tip] Idea
>Find an intermediate abstraction between applicative functors and monads, which requires us to declare all possible effects statically, but allows us to select which effects to execute dynamically.

```haskell
class Applicative f => Selective f where
  select :: f (Either a b) -> f (a -> b) -> f b
```

The _first_ computation is used to _select what happens next_:
- ﻿﻿`Left a`: you _must execute_ the second computation to produce a `b`;
- ﻿﻿`Right b`: you _may skip_ the second computation and return the `b`.

>[!note] speculative execution
> `Seletive` supports the so-called _speculative execution_, which allows _parallel executes the 1st and 2nd computation and cancel the 2nd if/when the 1st one evaluates to_ `Right b`.
>
> for instance the [`select` statement](https://go.dev/tour/concurrency/5) in Golang
# References

1. Mcbride, C., & Paterson, R. (2008). Applicative programming with effects. _Journal of Functional Programming_, _18_(1), 1–13. [https://doi.org/10.1017/S0956796807006326](https://doi.org/10.1017/S0956796807006326)
2. Mokhov, A., Lukyanov, G., Marlow, S., & Dimino, J. (2019). Selective applicative functors. _Proceedings of the ACM on Programming Languages_, _3_(ICFP), 90:1-90:29. [https://doi.org/10.1145/3341694](https://doi.org/10.1145/3341694)

