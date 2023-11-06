#Haskell #type-level-programming

Defunctionalisation is a technique that use a `Func` structure to represent a function and a `eval` to evaluate the function.

# Problem of currying type level function

GHC requires all type families shall be saturated, i.e. cannot partial apply a type family.

For instance 
```haskell
type family XOr (a :: Bool) (b :: Bool) :: Bool where 
  XOr 'False 'False = 'False
  XOr 'True 'True = 'False
  XOr _ _ = 'True 
```

the following will not type check
```haskell
type Not = XOr 'True 
-- error: 
--   type family 'Xor' should have 2 arguments but has seen 1

type Add1 = (+) 1 
-- error: 
--   type family '+' should have 2 arguments but has seen 1 
```

however the following type checks 
```haskell
type Not a = XOr 'True a

type Add1 (n :: Nat) = 1 + n 
```

it seems that we have a work around, however actually it just delays the problem, consider the following lift of `map`

```haskell
-- | f is a type level function of kind a -> b
type family Map (f :: a -> b) (xs :: [a]) :: [b] where 
  Map f '[] = []'
  Map f (x ': xs) = (f x) ': (Map f xs)

```

try

```
λ> :kind! Map Add1 '[1, 2, 3]
<interactive>:1:1: error:
  The type synonym ‘Add1’ should have 1 argument, but has been given none
λ> :kind! Map Not '['True, 'False, 'False]
<interactive>:1:1: error:
  The type synonym ‘Not’ should have 1 argument, but has been given none
```

the true problem here is that the first argument of type family `Map`, which is  `f :: a -> b`, has the kind `a -> b` which is not saturated and thus no instances is applicable at the position.

# Solution

since the first argument's kind of `Map :: (a -> b) -> [a] -> [b]` has no saturated instances, we need to find a kind that is both saturated and able to represent a type level function from `a` to `b`.

>[!Question]
> How to define `?`
>```haskell
>type family Map (f :: ? a b) (xs : [a]) :: [b] where 
>  ...
>```


Following defunctionalisation, we use a lifted functional type to define the kind of type function

```haskell
-- | represents a type level function from a to b
-- N.B. we need TyFun to be poly kind since at least
--      we need (TyFun a (TyFun b c))
type TyFun :: k -> k1 -> Type
data TyFun a b

-- >>> :kind! TyFun 
-- TyFun :: k1 -> k2 -> *
-- = TyFun
```

use `TyFun` to define `Map`, 
```haskell
type family Map (f :: TyFun a b) -> (xs :: [a]) :: [b] where
  Map _ '[] = '[]
  Map f (x ': xs) = ? ': Map f xs 
```

some new problem emerges: 
1. how to use `f` to map a type of kind `a` to a type of kind `b`?
2. how to construct a `TyFun a b`

the 1st problem is easy, follow defunctionalisation we have define a apply type level function

```haskell
type family Apply (f :: TyFun a b) (x :: a) :: b
```

for the 2nd problem we need a type constructor for the kind `TyFun a b` which can only be lifted from a data constructor.

```haskell
data TyFun a b = MkTyFun Symbol (Proxy a) (Proxy b)
```

we add a `Symbol` field to the data constructor to make sure the lifted type construct `'MkTyFun` can create multiple instances for kind `TyFun a b`.

>[!NOTE] 
>it seems `MkTyFun (a -> b)` can be a solution, however; the arrow has kind `(->) :: Type -> Type` forces `a` and `b` to be `types`.

Let's rewrite `Not`, `XOr`
```haskell
type Not :: TyFun Bool Bool
type Not = 'MkTyFun "not" 'Proxy 'Proxy 

type instance Apply Not 'True = 'False
type instance Apply Not 'False = 'True

type XOr :: TyFun Bool (TyFun Bool Bool)
type XOr = 'MkTyFun "xor" 'Proxy 'Proxy

type instance Apply XOr 'True = Not
type instance Apply XOr 'False = Id

type Id :: TyFun a a
type Id = 'MkTyFun "id" 'Proxy 'Proxy
type instance Apply Id a = a
```

# Prettify and Rewrite `Map`

1. replace the type constructor `TyFun` with `~>`, make `~>` right associative; 
2. replace the data constructor `MkTyFun` with `TyFun`;
3. add new data constructor `NatTyFun` to `~>` for representing `Nat` functions
3. replace the type family `Apply` with `@@`;
4. rewrite `XOr` to kind `Bool -> Bool ~> Bool` (notice the second arrow is not `->`).

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Defunctionalisation where

import Data.Kind
import Data.Proxy
import GHC.TypeLits

data (~>) (a :: k1) (b :: k2) 
  = TyFun Symbol (Proxy a) (Proxy b)
  | NatTyFun Nat Symbol (Proxy a) (Proxy b)
infixr 0 ~>

type family (@@) (f :: a ~> b) (x :: a) :: b
infixl 9 @@

type Id :: a ~> a
type Id = 'TyFun "id" 'Proxy 'Proxy
type instance Id @@ a = a

-- ====================== Bool ====================== 

-- type family Not' :: TyFun Bool Bool where
--     Not' = 'MkTyFun "Not" 'Proxy 'Proxy
type Not :: Bool ~> Bool
type Not = 'TyFun "not" 'Proxy 'Proxy
type instance Not @@ 'True = 'False
type instance Not @@ 'False = 'True

type XOr :: Bool -> Bool ~> Bool
type family XOr (a :: Bool) :: Bool ~> Bool where 
  XOr 'True = Not
  XOr 'False = Id


-- ====================== Nat ====================== 

type NatAdd :: Nat -> Nat ~> Nat
type family NatAdd (n :: Nat) where
    NatAdd n = 'NatTyFun "add" n 'Proxy 'Proxy

type NatSub :: Nat -> Nat ~> Nat
type family NatSub (n :: Nat) where
    NatSub n = 'NatTyFun "sub" n 'Proxy 'Proxy

type instance ('NatTyFun "add" (n :: Nat) 'Proxy 'Proxy) @@ (m :: Nat) 
  = n + m
type instance ('NatTyFun "sub" (n :: Nat) 'Proxy 'Proxy) @@ (m :: Nat) 
  = n - m

-- ====================== Map ====================== 

type family Map (f :: a ~> b) (xs :: [a]) :: [b] where
    Map _ '[] = '[]
    Map f (x ': xs) = (f @@ x) ': Map f xs
```

- use `Map` on `Nat`
  ```
  λ> :kind! Map (NatAdd 1) [32, 192, 123] 
  Map (NatAdd 1) [32, 192, 123] :: [Natural]
  = [33, 193, 124]
  
  λ> :kind! Map (NatAdd 1) [32, 192, 123, 0]                                 
  Map (NatAdd 1) [32, 192, 123, 0] :: [Natural]                            
  = [33, 193, 124, 1] 
  
  λ> :kind! Map (NatAdd 99) [32, 192, 123, 0]                            
  Map (NatAdd 99) [32, 192, 123, 0] :: [Natural]                           
  = [131, 291, 222, 99
  ```
- use `Map` on `Bool`
  ```
  λ> :kind! Map (XOr 'True) ['True, 'False, 'True]
  Map (XOr 'True) ['True, 'False, 'True] :: [Bool]
  = [False, True, False]
  ```

# The `singletons` package

The [`singletons`](https://hackage.haskell.org/package/singletons-3.0.2/docs/Data-Singletons.html#g:4) package implements _defunctionalisation_ to support **_promoting_**. 

```haskell
type TyFun :: Type -> Type -> Type    
-- ^ annotate the kind of TyFun as Type Constructor
data TyFun :: Type -> Type -> Type

-- | maps 2 types to a type
type (~>) :: Type -> Type -> Type
type a ~> b = (TyFun a b -> Type)
infixr 0 ~>
```

Note that kind `a ~> b` is a synonym of **kind** `TyFun a b -> Type`. 

we wish to `~>` is isomorphism to `->` , i.e. need to define
1. type level function `f :: (a -> b) -> (a ~> b)`
2. type level function `g :: (a ~> b) -> (a -> b)`

## From `->` to `~>` 

It seems a little odd to define `a ~> b = (TyFun a b -> Type)` and `type a ~> b = TyFun a b` a more natural option since it represent a "type function". 

However, the first option is more powerful when we try to construct types of kind `a ~> b`. 

```haskell
-- construct a type of kind `a ~> b` from a type of kind `a -> b`

-- we can creat convert -> to ~> by apply TyCon
-- such as `TyCon Maybe`
type TyCon1 :: (k1 -> k2) -> (k1 ~> k2)
-- ^ (equivently): 
--   type TyCon1 :: (k1 -> k2) -> (TyFun k1 k2) -> Type
data TyCon1 :: (k1 -> k2) -> (k1 ~> k2)


type TyCon2 :: (k1 -> k2 -> k3) -> (k1 ~> k2 ~> k3) 
data TyCon2 :: (k1 -> k2 -> k3) -> (k1 ~> k2 ~> k3) 

type TyCon3 :: (k1 -> k2 -> k3 -> k4) -> (k1 ~> k2 ~> k3 ~> k4) 
data TyCon3 :: (k1 -> k2 -> k3 -> k4) -> (k1 ~> k2 ~> k3 ~> k4) 

{-
NOTE the follow does not type check

    data TyCon' :: (k1 -> k2) -> TyFun k1 k2

The reason is keyword `data` creates a habitable type
  which can only be of kind `Type`

-}
```

thus we have 

```
if type `t` have kind `k1 -> k2` 
  then type `TyCon1 t` have kind `k1 ~> k2`

(t :: k1 -> k2) => (TyCon1 t :: k1 ~> k2)
```

## From `~>` to `->` 

```haskell
type Apply :: (k1 ~> k2) -> k1 -> k2
type family Apply (f :: k1 -> k2) (x :: k1) :: k2

type (@@) :: (k1 ~> k2) -> k1 -> k2
type f @@ a = Apply f a
infixl 9 @@


type instance Apply (TyCon1 f) a = f a 
```









