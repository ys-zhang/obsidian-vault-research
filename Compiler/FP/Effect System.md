#Haskell #category-theory 

# 1 Model

At the level of the syntax we distinguish _algebraic operations_, determined by their signatures^[which are functors], from _computations_: a recursive structure over these operations via the free monad.

_Algebraic effects_ & _handlers_ use a _free monad_ and an _interpreter_ to separate the syntax of effects from their semantics.

## 1.1 effectful operation: functor

We model _effectful operations_^[called actions in Polysemy] by their _signature_ $\sigma$. Effect signatures are modelled as functors.
```haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b
```

## 1.2 effectful computation: free monad

```haskell
data Free (f :: * -> *) a where
  Var :: a            -> Free f a  -- ^ pure
  Op  :: f (Free f a) -> Free f a  -- ^ join

instance Functor f => Monad (Free f) where
  return = Var
  (Var x) >>= k = k x
  (Op  t) >>= k = Op (fmap (>>= k) t)
```

![[Pasted image 20241213144043.png]]

```haskell
data FFree (f :: * -> *) a where
  Pure   :: a   -> FFree f a
  Impure :: f x -> (x -> FFree f a) -> FFree f a

instance Monad (FFree f a) where
  return = Pure
  (Pure a) >>= k = k a
  (Impure x kx) k = Impure x (kx >>> k)
```

>[!note]
>Notice that in `FFree` we no longer require the signature to be a functor and the signature do not need to contain a continuation.

## 1.3 interpretation: fold

```haskell
fold_alg :: Functor f => (a -> b) 
                      -> (f b -> b)   -- ^ f-algebra
                      -> Free f a 
                      -> b
fold_alg gen _   (Var x) = gen x
fold_alg gen alg (Op op) = alg ( fold_alg gen alg <$> op )
```

interpreting an effectful operation `f` requires `f` to be an [[F-algebra]] with `b` as its carrier. 

# 2 Non-algebraic & higher order effects

>[!def] algebraic effect
>Effects are algebraic if they satisfy the algebraicity property, which says that algebraic computations commute with sequencing:
> ```haskell
> (>>= k) . Op = Op . fmap (>>= k)
> ``` 

There exists effects that are not algebraic. For instance _scoped effects_, _parallel effects_ and _latent effect_.

>[!note] observation
> All of these non-algebraic effect _have an internal computation_ in there computation.

>[!def] high order effect
> _Higher-order effects_ denotes an effect constructor that uses the monad `m` in its parameters, allowing it to store an entire region for evaluation in an interpreter.

```haskell
{-# LANGUAGE QuantifiedConstraints #-}

class (forall f . Functor f => Functor (k f)) => HFunctor k where
  hmap :: (Functor f, Functor g) 
       => (forall a . f a   ->   g a)
       -- ^ natural transformation btw the 2 functors
       -> (forall a . k f a -> k g a)

data HFree k a where
  HVar :: a                       -> HFree k a
  HOp  :: k (HFree k) (HFree k a) -> HFree k a

instance HFunctor k => Monad (HFree k) where
  return = HVar
  HVar x >>= k = k x
  HOp op >>= k = HOp (fmap (>>= k) op)
```
``
![[Pasted image 20241226103946.png]]

## 2.1 Interpretation

```haskell
class Functor g => Pointed g where
  eta :: a -> g a

hFold :: (HFunctor k, Pointed g) 
      => (a -> g b) 
      -> (forall x . k g (g x) -> g x)
      -> HFree k a 
      -> g b
hFold gen alg (HVar x) = gen x
hFold gen alg (HOp op) = alg $ hmap natural op'
 where
  op' :: k (HFree k) (g b) 
  op' = fmap (hFold gen alg) op
  natural :: HFree k a -> g a
  natural (HVar x) = eta x
  natural (HOp  t) = alg $ hmap natural (fmap natural t)
```

## 2.2 Scoped effect


# 3 References

1. Kiselyov, O., & Ishii, H. (2015). Freer monads, more extensible effects. _SIGPLAN Not._, _50_(12), 94–105. [https://doi.org/10.1145/2887747.2804319](https://doi.org/10.1145/2887747.2804319)
2. van den Berg, B., & Schrijvers, T. (2024). A framework for higher-order effects & handlers. _Sci. Comput. Program._, _234_(C). [https://doi.org/10.1016/j.scico.2024.103086](https://doi.org/10.1016/j.scico.2024.103086)
3. [Talk: (PADL'24) Modular Higher-Order Effects](https://www.youtube.com/watch?v=UseIDeSCsf0).
