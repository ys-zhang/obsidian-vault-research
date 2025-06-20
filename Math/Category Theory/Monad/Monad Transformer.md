#category-theory  #functional-programming  #Haskell 


# The `mtl` Library

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


# Examples

## Writer

A _writer_ `m a` represents an embellished value (type `a`) with actions write to some environment `w` determined by `m`, i.e., `m -> w`

`w` is also dubbed as the _output_ of the _action_ `m`.

```haskell
type Writer w a = (a, w)
```

```haskell
-- | The monad class
class (Monoid w, Monad m) => MonadWriter w m | m -> w where 
  writer :: (a, w) -> m a
  -- ^ constructor, create a writer with env w and value a
  tell   :: w -> m ()
  -- ^ constructor tell = writer . ((), ), tells the current env to the monad
  listen :: m a -> m (a, w)
  -- ^ get the env out
  pass   :: m (a, w -> w) -> m a

listens :: (MonadWriter w m) => (w -> b) -> m a -> m (a, b)
censor  :: (MonadWriter w m) => (w -> w) -> m a -> m a

-- The monad transformer
WriterT :: * 
        -- ^ The output type
        -> (* -> *) 
        -- ^ The inner monad
        -> * -> *
        -- ^ The output monad

-- | Constructor, just like `writer`
writerT :: (Functor m, Monoid w)
        => m (a, w)
        -> WriterT w m a

-- | Unwrap WriterT w, just like `listen`
runWriterT :: Monoid w
           => Writer w m a
           -> m (a, w)

-- | Unwrap WriterT w
execWriterT :: (Monad m, Monoid w) 
            => WriterT w m a
            -> m w

instance (Monoid w, Monad m) => MonadWriter w (WriterT w m) where 
  ...

```

## Reader

_Reader monad_ (also called the _Environment monad_). Represents a computation, which can read values from a shared environment, pass values from function to function, and execute sub-computations in a modified environment.

```haskell
type Reader r a = r -> a
```

the type `r` is also dubbed as _environment_

```haskell

class Monad m => MonadReader r m | m -> r where
  ask    :: m r
  -- ^ retrieve env
  local  :: (r -> r) -> m a -> m a
  -- ^ modifies environment
  local = withReaderT
  reader :: (r -> a) -> m a
  -- ^ retrieves a value from the environment

asks :: MonadReader r m 
     => (r -> a) -> m a


ReaderT :: * 
        -- ^ r, the environment
        -> (* -> *) 
        -- ^ m, the inner monad
        -> * -> *
        -- ^ the output monad

withReaderT :: (r' -> r) -> ReaderT r m a -> ReaderT r' m a

newtype ReaderT r m a = ReaderT { 
    runReaderT :: r -> m a 
    -- ^ unwrap ReaderT
  }
```


## State

| State `MonadState s m` | Writer `MonadWrite w m` | Reader `MonadRead r m` |
| ---------------------- | ----------------------- | ---------------------- |
| `get :: m s`           |                         | `ask :: m r`           |
| `put :: s -> m ()`     | `tell :: w -> m ()`     |                        |

```haskell
State s a = Writer s (Reader s a) = (s -> a, s)
```

```haskell
class Monad m => MonadState s m | m -> s where 
  get :: m s
  put :: s -> m ()
  state :: (s -> (a, s)) -> m a

modify :: MonadState s m => (s -> s) -> m ()
gets :: MonadState s m => (s -> a) -> m a
```

# Under the hood


>[!note] compose rule 
> 2 monads `m` and `n` can be composed iff, there exists a function 
> 
> ```haskell 
> swap :: forall a . n (m a) -> m (n a)
> ```
> to see this the only pain is to implement the `join` method of the composed structure
> ```haskell
> join :: m(n (m (n a))) -> m (n a)
> join = fmap . join . join . fmap swap
> ```
> the swap function basically is saying that the effects of `m` and `n` carrying do not change
> if we swap the order they are performed.
> $$
> \begin{align}
> swap \circ map_N \; (map_M \; f ) &= map_M \; (map_N \; f ) \circ swap \\
> swap \circ unit_N &= map_M \; unit_N \\
> swap \circ map_N \; unit_M &= unit_M \\
> prod \circ map_N \; dorp &= dorp \circ prod 
> \end{align}
> $$


The following implementation is from this paper[^1]

```haskell
{-# LANGUAGE ScopedTypeVariables -}

-- | forward composition of monads
--   1st m then n
data FComp m n a = FC { unFC :: n (m a) }

-- | backword composition of monads
--   1st n then m
data BComp m n a = BC { unBC :: m (n a) }

class Monad m => Into m where 
  into :: Monad n 
       => m (n a)
       -> n (m a)

class Monad m => OutOf m where 
  outof :: Monad n 
        => n (m a)
        -> m (n a)

instance (Into m, Monad n) => Monad (FComp m n) where 
  return = FC . return . return 
  (FC c) >>= f = 
    let f'   = unFC . f  
        -- ^ a -> n (m b)
        f''  = fmap f' 
        -- ^ m a -> m (n (m b))
        f''' = into . f''
        -- ^ m a -> n (m (m b))
    in FC ((fmap join . join . fmap f''') c)
    
instance (OutOf m, Monad n) => Monad (BComp m n) where 
  return = BC . return . return 
  (BC c) >>= f = 
    BC ((fmap join . join . fmap f’) c) 
    where f’ = outof . fmap (unBC . f)

-- ---------------------------------------------------
--              MONAD TRANSFORMERS
-- ---------------------------------------------------
type MaybeT    = FComp Maybe 
type ErrorT    = FComp Error 
type WriterT   = FComp Writer 
type ReaderT r = BComp (r ->)
```



[^1]: Jones, M. P. (1995). Functional programming with overloading and higher-order polymorphism. _Advanced Functional Programming: First International Spring School on Advanced Functional Programming Techniques Båstad, Sweden, May 24–30, 1995 Tutorial Text 1_, 97–136.