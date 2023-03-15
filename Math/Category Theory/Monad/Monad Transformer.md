#category-theory  #functional-programming  #Haskell 



# Implementation

the [`mtl`](https://hackage.haskell.org/package/mtl) (short for monad transformer library) has a common pattern for monads which include
1. a monad, each provides a context
2. a monad transformer
3. a monad class, identifies the functionality of the context

> [!quote]
> In the `Control.Monad.Foo` module, we'd find:
>
> -   A type class `MonadFoo` with the transformer operations.
> -   A data type `FooT` with instances for all monad transformer classes.
> -   Functions to run the transformed computation, e.g. `runFooT`. For the actual transformers, there are usually a number of useful runner functions.


taking `Reader`, `ReaderT` and `MonadReader`  for instance

| Symbol       | Pattern                                                              |
| ------------- | -------------------------------------------------------------------- |
| `Reader`      | the monad                                                            |
| `ReaderT`     | wrap a monad to construct a new monad supports `MonadReader`         |
| `MonadReader` | constraint on a monad to have the ability to read from its context | 

```haskell
-- | the monad 
class Monad m => MonadReader r m | m -> r where 
  ask :: m r
  local :: (r -> r) -> m a -> m a

instance (Monad m) => MonadReader r (ReaderT r m) where 
  ...

-- a monad reader get tranformed by stacking other monads on 
-- still a monad reader
instance (MonadReader r m) => MonadReader r (StateT s m) where
  ...
```

> the point is that in `mtl` all other monad wrappers (synonym for monad transformer) that wraps a `MonadReader` is also a `MonadReader`

```haskell
-- forall MonadFoo and BarT
instance (MonadFoo m) => MonadFoo (BarT m) where
  ...
```


> [!problem]   $n^2$ instance problem
> to extend the system by adding a new transformer, you have to instance for all other old monad classes
