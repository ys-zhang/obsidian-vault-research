#functional-programming #generic-programming #Haskell 

# Generics in Haskell

There are 3 mechanisms to do generics in Haskell
1. _Typeable_:  runtime access to type info, everything is `Typable`
2. _Data_:   , you need to `deriving Data` manually
3. _Generics_

>[!note] 
>Generics is about a function handles different types in inhomogeneous ways, i.e., different calculation on different types.
>
>While there is a similar but totally different thing called `Dynamic` defined in module `Data.Dynamic` which provides interface for dynamic type, e.g., what `Python` adopts. `Dynamic` is like existential type.


## Typeable

`Typable` constraint allows _runtime_ type identification. 

By default Haskell erases type information in the compiled code, while `Typable` forces type info is reserved and accessible at runtime.

```haskell
import Type.Reflection 
-- Data.Typable also works but the interface it provides is staled

doSomeWhenInt :: Typable a
              -- ^ adds constraint that a's typeinfo 
              -- ^ is accessible at runtime
              => a -> a
doSomeWhenInt x 
  | Just HRefl <- typeOf x `eqTypeRep` typeRep @Int = x + 1
  | otherwise                                       = x
```


## Data

`Data` lets you peek/query into the type at run time

```haskell
{-# LANGUAGE DeriveDataTypable -}
import Data.Data (Data)
import Data.Generics (everywhere, mkT)

data SomeData = SomeData Int Double Int Bool
  deriving (Data)

add1 :: Data a => a -> a 
add1 = everywhere (mkT ((+1) :: Int -> Int))

-- >>> add1 $ SomeData 1 23 3 False
-- SomeData 2 23.0 4 False
```

## GHC Generics
peeking inside types at compiled type

# SYB: Scrap Your Boilerplate

>[!TLDR]
> Do CRUD on deeply nested ADTs using Generics (rank2types) 
> module `Data.Generics.Aliases`

>[!NOTE] Term -> Data
>the `Term` type class is implemented and named as `Data` in 
>GHC.

## Transformation

A (generic) transformation is something of type 

```haskell
type GenericT = forall a. Term a => a -> a
```

The `mkT` function lift an interesting monomorphic function into a generic transformation.

```haskell
{- LANGUAGE RankNTypes -}
import Data.Generics

cast :: (Typeable a, Typeable b) => a -> Maybe b

-- | make tranformation
mkT :: (Typeable a, Typeable b) => (b -> b) -> a -> a 
mkT f = fromMaybe id (cast f)

-- | The intended behaviour is this: 
-- gmapT takes a generic transformation (such as inc k)
-- and applies it to all the immediate children of the value.
class Typeable a => Term a where 
    gmapT :: (forall b. Term b => b -> b) -> a -> a

everywhere :: Term a
           => (forall b. Term b => b -> b) 
           -> a -> a
everywhere f a =  f (gmapT (everywhere f) a)
```

## Queries 

A generic query is something of the type

```haskell
newtype GenericQ r = MkQuery (forall a. Term a => a -> r)
```

Similar to `mkT` we have `mkQ`

```haskell
mkQ :: (Typeable a, Typeable b) 
    => r         -- default
    -> (b -> r)  -- interesting monomorphic query
    -> a -> r    -- generic query
(r `mkQ` q) = maybe r q . cast

class Typeable a => Term a where 
    gmapT :: (forall b. Term b => b -> b) -> a -> a
    gmapQ :: (forall b. Term b => b -> r) -> a -> [r]

-- Summarise all nodes in top-down, left-to-right
everything :: Term a 
           => (r -> r -> r). -- semigroup like
           -> (forall a. Term a => a -> r) 
           -> a -> r 
everything k f x = foldl k (f x) (gmapQ (everything k f) x)
```

## Monadic transformation

```haskell
type GenericM m = forall a. (Term a, Term (m a)) => a -> m a

mkM :: (Typeable a, Typeable b, 
        Typeable (m a), Typeable (m b),
        Monad m) 
    => (b -> m b) -> a -> m a
mkM f = fromMaybe return (cast f)

class Typeable a => Term a where 
    gmapT :: (forall b. Term b => b -> b) -> a -> a
    gmapQ :: (forall b. Term b => b -> r) -> a -> [r]
    gmapM :: Monad m
          => (forall b. Term b  => b -> m b) 
          -> a -> m a
```

## See also 

https://wiki.haskell.org/Scrap_your_boilerplate



