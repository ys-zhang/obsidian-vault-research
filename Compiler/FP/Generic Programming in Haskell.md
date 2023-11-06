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

# GHC Generics


_Peeking inside types_ at compiled type:

> Generics allow us to describe transformation that works in terms of not concrete types, but general combinators that describe shape and meta information about a data type.
> 
> A data type that is an instance of the `Generic` class can be processed by functions that work with a representation of data type, not the data type itself.

also from the package document: 

> Datatype-generic functions are based on the idea of converting values of a datatype `T` into corresponding values of a (nearly) **isomorphic type** `Rep T`. 

For detail see 
- [Haskell Generics Explained](https://markkarpov.com/tutorial/generics)
- [A Generic Journey](https://github.com/well-typed/gp-zurihac-2020/tree/master)

## Types 

```haskell
{-# LANGUAGE TypeFamilies #-}

-- | the type class `Generic` contains types that 
-- of kind `*` that is isomorphic to 
-- a sum of product representation
class Generic a where 
  type Rep a :: * -> * 
  from :: Rep a dummy -> a
  to :: a -> Rep a dummy

-- | `Generic1` represents any type `f` of kind `k -> *`
-- such that `forall a :: k`, `f a` isomorphic to
-- sum of product `Rep1 f a`
class Generic1 (f :: k -> *) where 
  type Rep1 f :: k -> *
  from1 :: forall (a :: k) . f a -> Rep1 f a 
  to1 :: forall (a :: k) . Rep1 f a -> f a
```

>[!Note] Why `dummy`
> the reason to include the `dummy` type variable is to make 
> `Rep` reuses `Rep1`'s building brick types, i.e., they share the same representation.
>
> `Rep1` , which represents a HKT (higher kind type) , needs a type variable in the representation.

`Rep a dummy` and `Rep1 f a` are limited and can only be "sum of product" types, i.e., composed by the following types:

### SOP Operators
```haskell
-- | 2-sum 
type (:+:) :: (k -> Type) -> (k -> Type) -> k -> Type
data (:+:) f g a = L1 (f a) | L2 (g a)
-- | 2-prod
type (:*:) :: (k -> Type) -> (k -> Type) -> k -> Type
data (:*:) f g a = (f a) :*: (g a)
```

### Brick Types for SOP

```haskell
-- ====== atoms for build up sop ====== 

-- | V1 where "V" for Void and 
-- "1" for 1 type parameter
data V1 a
-- | U for Unit
data U1 a = U1


-- | Rec stand for recursive, used to represent 
-- recursive types, for instanced
-- `List a = Nil | Cons a (List a)` represented as
-- `U1 :+: (Par1 :*: (Rec1 List))`
data Rec1 f a = Rec1 {unRec1 :: f a}
-- | equivalent to 
-- type Rec0 :: Type -> k -> Type
-- data Rec0 c a = Rec0 { unRec0 :: c }
-- 
-- This type is used in 2 cases
--  1. (Recusive type): `data Nat = Zero | Succ Nat`
--  2. (Single Field Constructer): `newtype Id = MkId UUID` 
-- 
-- N.B. `Par0` was once used to deal with the second case 
--      but now deprecated
type Rec0 = K1 R
newtype K1 i c p = K1 { unK1 :: c } -- c is the value
--         ^   ^
--         |   |
--         |   +-------- dummy p
--         |
-- type-level tag, R or P

-- | Par stands for parameter
-- It is used to inject the occurance of 
-- a type parameter of the original type
-- For instance `a` in `Maybe a`
-- we can represent `Maybe` as `U1 :+: Par1`
data Par1 a  = Par1 {unPar1 :: a}
```

Examples
1. `Rep (Maybe Int) dummy` -> `(U1 :+: Rec0 Int) dummy`
2. `Rep1 Maybe` -> `U1 :+: Par1`

### Meta-data Wrapper

All meta-data is attached using the same wrapper

```haskell
-- type params:
-- i: Marker indicates what kind of metadata 
--    the wrapper type embeds
type M1 :: Type -> Meta -> (k -> Type) -> k -> Type
newtype M1 i (c :: Meta) (f :: k -> Type) (p :: k) 
  = M1 { unM1 :: f p } 
```

>[!NOTE] 
> Note the meta data only embedded in the type level, which will be erase at runtime.

Type _marker type parameter_ `i` can be one of the following options:
- `D` for data type, type constructor
- `C` for constructor, data constructor
- `S` for selector, record selector

```haskell
data D; data C; data S;
type D1 = M1 D
type C1 = M1 C
type S1 = M1 S
```

Meta info:
```haskell
data Meta 
  -- | for type constructor
  = MetaData Symbol  -- n: type name
             Symbol  -- m: module name
             Symbol  -- p: package name 
             Bool    -- nt: `'True` if is `newtype`
  -- | for data constructor
  | MetaCons Symbol  -- n: constructor name
             FixityI -- f: fixity (assoc and precedence)
             Bool    -- s: `'True` if has record selector
  -- | for field of a data constructor 
  | MetaSel  (Maybe Symbol)     -- mn: `Just name`
             SourceUnpackedness -- su: whether {-# UNPACK #-}
             SourceStrickness   -- ss: strickness
             DecidedStrictness  -- ds: GHC infered strickness
```

- _(type constructors):_ The entire representation is wrapped in `D1`, which provides datatype-level information: _datatype name_, _module name_, and whether it’s a `newtype`.
- _(data constructors):_ Every constructor is wrapped in `C1`, which provides information about the constructor such as its _constructor name_, _fixity_, and whether it’s a record.
- _(fields):_ Every argument of a constructor is wrapped with `S1` (even if it’s not actually a record selector), which tells us the selector name.

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


# `generics-sop`

This package is an upgrade of `GHC.Generics`

