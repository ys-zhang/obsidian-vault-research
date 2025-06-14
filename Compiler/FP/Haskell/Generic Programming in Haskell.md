#functional-programming #generic-programming #Haskell 

# 1 Generics in Haskell

There are 3 mechanisms to do generics in Haskell
1. _Typeable_:  runtime access to type info, everything is `Typable`
2. _Data_:   , you need to `deriving Data` manually
3. _Generics_

>[!note] 
>Generics is about a function handles different types in inhomogeneous ways, i.e., different calculation on different types.
>
>While there is a similar but totally different thing called `Dynamic` defined in module `Data.Dynamic` which provides interface for dynamic type, e.g., what `Python` adopts. `Dynamic` is like existential type.


## 1.1 Typeable

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


## 1.2 Data

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

# 2 GHC Generics


_Peeking inside types_ at compiled type:

> Generics allow us to describe transformation that works in terms of not concrete types, but general combinators that describe shape and meta information about a data type.
> 
> A data type that is an instance of the `Generic` class can be processed by functions that work with a representation of data type, not the data type itself.

also from the package document: 

> Datatype-generic functions are based on the idea of converting values of a datatype `T` into corresponding values of a (nearly) **isomorphic type** `Rep T`. 

For detail see 
- [Haskell Generics Explained](https://markkarpov.com/tutorial/generics)
- [A Generic Journey](https://github.com/well-typed/gp-zurihac-2020/tree/master)

## 2.1 Types 

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

### 2.1.1 SOP Operators
```haskell
-- | 2-sum 
type (:+:) :: (k -> Type) -> (k -> Type) -> k -> Type
data (:+:) f g a = L1 (f a) | L2 (g a)
-- | 2-prod
type (:*:) :: (k -> Type) -> (k -> Type) -> k -> Type
data (:*:) f g a = (f a) :*: (g a)
```

### 2.1.2 Brick Types for SOP

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
-- | K stands for constant, not a type parameter
newtype K1 i c p = K1 { unK1 :: c } -- c is the value
--         ^   ^
--         |   |
--         |   +-------- dummy p
--         |
-- type-level tag, R or P

-- | Par stands for parameter
-- 
--  Par1 can be interpreted as 1st type parameter
--
-- It is used to inject the occurance of 
-- a type parameter of the original type
-- For instance `a` in `Maybe a`
-- we can represent `Maybe` as `U1 :+: Par1`
data Par1 a  = Par1 {unPar1 :: a}
```



Examples
1. `Rep (Maybe Int) dummy` -> `(U1 :+: Rec0 Int) dummy`
2. `Rep1 Maybe` -> `U1 :+: Par1`

### 2.1.3 Meta-data Wrapper

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
-- | Only intended to be used as promoted kind in type level.
-- Do not use instances of this type.
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

## 2.2 Util Functions

```haskell
GHC.TypeLits.symbolVal :: forall (n :: Symbol) proxy . KnownSymbol n
                       => proxy n 
                       -> String
GHC.TypeLits.natVal :: forall (n :: Nat) proxy . KnownNat n 
                    => proxy n
                    -> Integer
```

## 2.3 `Generically` and `DerivingVia`

The [DerivingVia](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/deriving_via.html) language extension allows to _derive_ a type class _via_ an _isomorphic_ type. 

The initiative is simple. Suppose type `a` is isomorphic to type `b` and `b` belongs to some type class `C` then `a` can also be an element of `C` by _push forward_ and _pull back_ using isomorphism btw `a` and `b`.

In reality, the requirement of _isomorphic_ is not enough, we need _coercible_, i.e., they must have the same _runtime representation_.


## 2.4 Performance 

Usually adding INLINE pragmas to each of your class’ methods is enough to optimise away all usage of GHC.Generics at compile-time.

The package [`inspection-testing`](https://hackage.haskell.org/package/inspection-testing) provides a plugin to GHC which allows us to make assertions about our generated code. 
We can use it to ensure GHC optimises away all of our usages of GHC.Generics, and generates the exact same code that we would have written by hand. 


# 3 SYB: Scrap Your Boilerplate

>[!TLDR]
> Do CRUD on deeply nested ADTs using Generics (rank2types) 
> module `Data.Generics`.
> 
> you can also use `Data.Data` from `base`

>[!NOTE] Term -> Data
>the `Term` type class is implemented and named as `Data` in 
>GHC.

## 3.1 Transformation

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
mkT :: (Typeable a, Typeable b) 
    => (b -> b) -> a -> a 
mkT f = fromMaybe id (cast f)

-- | attaching some action and some transformation 
-- to some type b
mkM :: (Typeable a, Typeable b, Typeable (m a), Typeable (m b), Monad m)
    => (b -> m b) -> a -> m a
mkM mf = fromMaybe return (cast mf)

everywhere :: Term a
           => (forall b. Term b => b -> b) 
           -> a -> a
everywhere f a =  f (gmapT (everywhere f) a)

everywhereM :: (Monad m, Term a)
            => (forall b. Term b => b -> m b) 
            -> a -> m a
everywhereM mf a = f =<< (gmapM (everywhereM mf) a)
 

-- | The intended behaviour is this: 
-- gmapT takes a generic transformation (such as inc k)
-- and applies it to all the immediate children of the value.
class Typeable a => Term a where 
    gmapT :: (forall b. Term b => b -> b) -> a -> a
    gmapM :: Monad m 
          => (forall b. Term b => b -> m b) -> a -> m a
```

>[!warning] `gmapT`
> `gmapT` only applies to its ***immediate*** children, in other words, no recursion and only unwrap one level of data constructor

## 3.2 Queries 

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

>[!Note] `foldl` or `foldr`
> Its always worth to be careful when `foldl` is used in a Haskell,
> taking `head` of `foldl` to get the first of the folded list will traverse the whole list.
> ```haskell
> foldl merge z (x:xs) = foldl merge (merge z x) xs
> foldr merge z (x:xs) = merge x (foldr merge z xs) 
> ```
> However It seems replacing `foldl` with `foldr` does not work either. Suppose `r` is some `[a]`, then `foldr` with force elements queried from the root data to appear in the tail of the total result, which means if we only needs 1 query result with force the algorithm to traverse to at least one of the leaf of the whole tree. 


## 3.3 Misc

1. stop (or cut-off) the traversal 
```haskell
everywhereBut :: GenericQ Bool -> GenericT -> GenericT
everywhereBut q f x 
  | q x       = x
  | otherwise = f (gmapT (everywhereBut q f) x)
  
everythingBut :: (r -> r -> r) 
              -- ^ merge operation
              -> GenericQ (Bool, r) 
              -- ^ query with an indicator to stop 
              -> GenericQ r
everythingBut merge q x 
  | stop      = r
  | otherwise = let rs = (gmapQ (everythingBut merger q) x)
                in  foldl merge r rs   
 where 
  (stop, r) = q x
```
2. compound type extension
```haskell
extQ :: (Typeable a, Typeable b) 
     => (a -> r) -> (b -> r) -> (a -> r)
q `extQ` f = \a -> case cast a of 
  Just b  -> f b
  Nothing -> q a
```

## 3.4 The `Data` type class implementation

### 3.4.1 Generalise generic functions

Till now we have the following types of "generic function":

```haskell
type GenericT   = forall a . Term a => a -> a
type GenericM m = forall a . (Term a) => a -> m a
type GenericQ r = forall a . Term a => a -> r
```

They share the same structure 

```haskell
type GenericG W = forall a . Term a => a -> W a
```

where $W$ is a type-level function

$$
\begin{align}
&\mathbf{GenericT} = \mathbf{GenericG} \; \mathit{Identity} \\
&\mathbf{GenericM} \; \mathit{m}  = \text{GenericG} \; \mathit{m} \\
&\mathbf{GenericQ} \; \mathit{r}  = \text{GenericG} \; (\mathit{Const} \; \mathit{r}) \\
\end{align}
$$

### 3.4.2 Implementing `gmapT`, `gmapM`, `gmapQ` with `gfoldl`

```haskell
class Term a where
  gfoldl :: (forall a b . Term a 
             => w (a -> b)        -- constructor
             -> a                 -- args to the constructor
             -> w b)       
         -> (forall f . f -> w f) -- apply to the data constructor
         -> a -> w a
         
  gmapT :: (forall b . Term b => b -> b) -> a -> a
  gmapT f = gfoldl (\c a -> c (f a)) id
  
  gmapM :: Monad m => (forall b. Term b => b -> m b) -> a -> m a
  gmapM mf = 
    gfold 
      (\mc a -> do {c <- mc; a' <- mf a; return c a'}) 
      return

  gmapQ :: (forall b . Term b => b -> r) -> a -> [r]
  gmapQ q = gfoldl 
```


>[!note] 
>The `gfoldl` is a function that folding the list `[dataConstructor, arg1, arg2, ...., ]`.

The `Term` type class is actually defined as `Data` in the module `Data.Data`. With an extra method `gunfold` which is used to create default data instances. Together with a default `gfoldl`

```haskell
class Data a where
  gunfold :: (forall b r . Data b => c (b -> r) -> c r)
          -- ^ providing default value for constructor args
          -> (forall r . r -> c r)
          -- ^ inject the data constructor function
          -> Constr 
          -- ^ a descriptor of the constructor
          -> c a
          -- ^ new data created by the data constructor
  
  gfoldl :: (forall a b . Term a 
             => w (a -> b)        -- constructor
             -> a                 -- args to the constructor
             -> w b)       
         -> (forall f . f -> w f) -- apply to the data constructor
         -> a -> w a
  gfoldl = const id
```

for detail see [[Haskell Tricks#Generic fold and unfold]].

> [!note]
>- **`gunfold` is used** in SYB for **value construction** (e.g., `fromConstr`).
>- **`gfoldl` is used** for **value deconstruction/traversal** (e.g., `gmapT`).
> - If your use case involves **building values generically** (e.g., parsing, generating default values), `gunfold` is essential. For traversal/querying, `gfoldl` suffices.
# 4 SYB with class: extensible generic functions

the method is implemented in the [constraints](https://hackage.haskell.org/package/constraints) package.

## 4.1 Problem 

the vanilla [[#SYB Scrap Your Boilerplate|SYB]] method is that the function `extQ` is _non-modular_, i.e. all special cases must be defined at the same site (where `extQ` is called).

## 4.2 Solutions

1. define a typeclass for each extensible generic function (`Size` for `gsize`) 
2. add a constraint context for the `Data` typeclass
3. define the default generic function using either 
  1. `OverlappingInstances` or 
  2. `DefaultSignatures`

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}

import Data.Typeable

class (Typeable a, cxt a) => Data' cxt a where
  gmapQ' :: (forall b. Data' cxt b => b -> r)
         -> a 
         -> [r]

class Size a where 
  gsize :: a -> Int

instance {-# OVERLAPABLE #-} Data' Size t => Size t where 
  gsize x = 1 + sum (gmapQ' @Size gsize x)
```
# 5 `uniplate`: focus on one single type

>[!quote] motivating observation
> Most traversals have value-specific behaviour for **just one type**.
> 
> The central idea is to exploit a common property of many traversals: _they only require value-specific behaviour for a single uniform type_.

## 5.1 API 

Two types of traversals:
1. query(`a -> [a]`): A query is a function that takes a value, and extracts some information of a different type. 
2. transformation(`a -> a`): A transformation takes a value, and returns a modified version of the original value.

![[uniplate-api.png]]


### 5.1.1 Query
```haskell
{- | The function children takes a value and 
returns all maximal proper substructures of the same type.
-}
children :: Uniplate a => a -> [a]
{- | This function takes a data structure, 
and returns a list of all structures of the 
same type found within it.-}
universe :: Uniplate a => a -> [a]
```

>[!NOTE] 
>the `universe` function returns a list orresponding to _pre-order traversal_

#### 5.1.1.1 Synthesise with paramorphism

the `para` synthesis information of type `r` in a _bottom-up order_ from value of type `a` 

```haskell
para :: Uniplate a 
     => ( a        -- ^ current node
          -> [r]   -- ^ children results
          -> r     -- ^ result of current node
        ) 
     -> a 
     -> r
```

>[!note] The original definition of **paramorphism**:
>
> ```haskell
> para  ::  (a -> [a] -> b -> b)  -> b -> [a] -> b  
> foldr :: (a ->        b -> b)  -> b -> [a] -> b  
>   
> para  c n (x : xs) = c x xs (para c n xs)  
> foldr c n (x : xs) = c x (foldr c n xs)  
> para  c n [] = n  
> foldr c n [] = n
> ```
### 5.1.2 Transform
```haskell
-- | bottom up transformation
transform :: Uniplate a => (a -> a) -> a -> a

-- | apply f to all immediate and only immediate children
-- in other words, no recursion
-- NOTE: this is exactly `gmapT` in syb
descend :: Uniplate a => (a -> a) -> a -> a
```

>[!Warning] Top-down transform
> top-down transformations are error-prone.
> the semantics of `top-down` transformation is to apply `f` to current value to get a new value and then apply f the _new value's_ children. 
>
> _The caveat here is that `f` never applies the the generated new value_ 


### 5.1.3 Rewrite

> [!quote]
> The idea of _rewrite rule_ is that a rule is applied exhaustively until a normal form is achieved. 
>
> For details see [Term Rewriting](https://inst.eecs.berkeley.edu/~cs294-260/sp24/2024-01-22-term-rewriting)

```haskell
rewrite :: Uniplate a 
        => (a -> Maybe a) 
        -- ^ A rewrite rule:
        -- return Nothing if we have reached the normal form 
        -- and terminates the rewrite process 
        -> a 
        -> a
rewrite r = transform g
 where 
  g x = maybe x (rewrite r) (r x)

propRewrite r x = all (isNothing . r) (universe (rewrite r x )) 

always :: (a -> Maybe a) -> a -> a
always r x = fromMaybe x (r x)
```

>[!warning] Performance warning of `rewrite`
>A disadvantage of rewrite is that it may check unchanged subexpressions repeatedly. Performance sensitive programmers might prefer to use an explicit transformation, and manage the rewriting themselves.

#### 5.1.3.1 Context

```haskell
contexts :: Uniplate a 
         => a          -- ^ a: the input
         -> [( a       -- ^ x: some dissidants of the input `a`
            ,  a -> a  -- ^ f: `f y` is what we get from `a` if we  
            )]         --       replace `x` with `y` in `a`

propUniverse x = universe x === map fst (contexts x ) 
propId x = all (=== x) [f a | (a, f) <- contexts x ]
```

>[!note]
>the returned contexts, like `universe`, corresponds to _pre-order traversal_.

the semantics of the `contexts` function is the same as _rewrite contexts_.

Recall rewrite rule application, 
> If we want to refer to a specific application of a rewrite, $l \to r$, applied to $t$, then we can use a the notions of a _substitution_ $\sigma$ and _context_ $c$. 
> - A _substitution_ $\sigma$ is a mapping from variables in the rewrite to sub-terms of $t$. We say that a substitution  _unifies_ two terms $s$ and $t$ if $s\sigma = t\sigma$. 
> - The _context_ allows us to refer to a specific location or sub-term of $t$. 
>  
> So applying rewrite $l \to r$ to $t$ means finding a substitution $sigma$ that unifies a sub-term of $t$ with $l$; in other words there is a a context $c$ such that $c[l\sigma] = t$, is rewritten to $c[r\sigma]$.

Thus, `contexts` can be used for applying re
### 5.1.4 Action(monadic) Transformation

```haskell
transformM :: (Uniplate a, Monad m) => (a -> m a) -> a -> m a
```


## 5.2 Implementation


```haskell
class Uniplate a where
  uniplate :: a -> ([a], [a] -> a)

-- | property must hold of children and context
propId x = x === context children
  where (children, context) = uniplate x
```

the intention of the `uniplate` function is to 
1. (_query_): get `children` of a value
2. (_rewrite_): reassemble new values from new children
thus:
1. `(fst . uniplate) :: a -> [a]` shall return _all maximal proper substructures of the same type `a`_;
2. `(snd . uniplate) :: [a] -> a` shall make a new value with a _different set of children_, the caller of this function must ensure that the length of the input list must equals the number of children.

```haskell
children   = fst . uniplate
universe x = x : concatMap universe (children x)

transform f x = let (cs, gen) = uniplate x 
                in  f (gen (transform f <$> cs))
descend f x = let (cs, gen) = uniplate x
              in  gen $ map f cs
```


## 5.3 Multi-type Traversal

this solves the problem of _intermediate layer types_, such as query `Expr` from a `Program`, while `Program` consists of `Stmt` which consists of `Expr`.

> [!note]
> The intuition for **biplate** is that given a structure of type $\beta$, the function should return the largest substructures in it of type $\alpha$.

```haskell
-- | b is the container type
--   a is the type of insterest
type BiplateType b a = b -> ([a], [a] -> b)

propId :: BiplateType b a -> (b -> Property)
propId biplate = \x -> 
  let (cs, gen) = biplate x
  in  x === gen cs

class Uniplate a => Biplate b a where
  biplate :: BiplateType b a


universeOn :: Uniplate a
           => BiplateType b a
           -> b 
           -> [a]
universeOn biplate x = cancatMap universe $ fst $ biplate x

transformOn :: Uniplate a
            => BiplateType b a
            -> (a -> a)
            -> b 
            -> b
transformOn biplate f x = 
  context $ map (transform f) children
 where
  (children, context) = biplate x
```

Note that `universeOn` and `tranformOn` is not recursive, which also means they will not recursively peek into sub containers of the container type `b`.

>[!warning] Caveat implementing `Biplate`
> the `biplate` function must return _all maximal proper substructures of the same type `a`_, which means all `a` that is not contained in some `a`.


# 6 `generics-sop`

This package is an upgrade of `GHC.Generics`

## 6.1 SOP Universe 

In `GHC.Generics`, `Rep a` could be any type, but only the ones defined in `GHC.Generics`(such as `:+:`, `:*:`, `V1`, `U1`, `Rec0`, `Rec1`, `Par1`) make sense.

`generics-sop` forces the representation to be in the universe of SOP.

```haskell
class (All SListI (Code a)) => Generic (a :: Type) where 
  -- map a generic type to kind `[[*]]`
  type Code :: Type -> [[Type]]
  type family Code a :: [[Type]]
  type family Rep a = SOP I (Code a)
  from :: a -> Rep a
  to   :: Rep a -> a

-- sum of product
type SOP :: (Type -> Type) -> [[Type]] -> Type
type SOP (f :: k -> Type) (xss :: [[k]]) = NS (NP f) xss  
-- product of product
type POP :: (Type -> Type) -> [[Type]] -> Type
type POP (f :: k -> Type) (xss :: [[k]]) = NP (NP f) xss  

type NP :: (k -> Type) -> [k] -> Type
data NP :: (k -> Type) -> [k] -> Type where 
  Nil  :: NP f xs
  (:*) :: f x ->  NP f xs -> NP f (x ': xs) 

-- | there is only pos in `xs` that is a `Z` the reason is 
-- if you have an instance of `NS f xs` you can only apply 
-- `S` to it
type NS :: (k -> Type) -> [k] -> Type
data NS :: (k -> Type) -> [k] -> Type where 
  -- | put a elem at the head pos
  Z :: f x -> NS f (x ': xs)     
  -- | shift elems forword
  S :: NS f xs -> NS f (x ': xs) 
```

>[!NOTE] 
>In `NP f xs`, `NS f xs`, `xs` can be think of a marker marks the info of the n-prod or n-sum.
>
>Info includes _dimension_ of the list/prod, what the type of each index/component of the list/prod
>
>And `xs` is actually part of the list type in `NP f xs`  

## 6.2 Util functions

`NP f xs` can be thought of Heterogeneous List `[f x1, f x2, ..., f xn]`, while `NS f xs` can be though of a generalised `Either`, some thing like `OneOf (f x1) (f x2) ... (f xn)`

we want to at least generalise the following functions
1. (product of arrow) `map :: (a -> b) -> [a] -> [b]`
2. (projection) `mapMaybe :: (a -> Maybe b) -> [a] -> [b]`, equivalent to `filter`
3. (sum of arrow) `either :: (a -> c) -> (b -> c) -> Either a b -> c`
4. (injection) `Left :: a -> Either a b` and `Right :: a -> Either b a`


```haskell
type (-.->) :: (k -> Type) -> (k -> Type) -> k -> Type
newtype (f -.-> g) (a::k) = Fn { apFn :: f a -> g a}

-- generalisation of `ap` in `Applicative` 
-- in the sence of `NP f xs` with `xs` fixed, 
-- and f the free variable
hap, ap_NP :: NP (f -.-> g) xs -> NP f xs -> NP g xs
hap, ap_NS :: NP (f -.-> g) xs -> NS f xs -> NS g xs
hap, ap_SOP :: POP (f -.-> g) xss -> SOP f xss -> SOP g xs
hap, ap_POP :: POP (f -.-> g) xss -> POP f xss -> POP g xss
hmap  :: (...) => (forall a. f a -> g a) -> h f xs -> h g xs
hcmap :: (...) => (forall a. c a => f a -> g a) -> h f xs -> h g xs

-- "H" stands for "Higher-kind"
class (Prod (Prod h) ~ h, HPure (Prod h)) => Hap (h :: (k -> Type) -> k1 -> Type) where 
  hap :: Prod h (f -.-> g) xs -> h f xs -> h g xs  

-- Prod takes the outmost NS to a NP
-- if the outmost is an NP then do nothing
type Prod :: ((k->Type) -> k1 -> Type)
          -> ((k->Type) -> k1 -> Type)
type family Prod    (h :: (k -> Type) -> k1 -> Type)
                 :: (k -> Type) -> k1 -> Type


hpure,  pure_NP   :: SListI  xs  => (forall a. f a) -> NP  f xs
hpure,  pure_POP  :: SListI2 xxs => (forall a. f a) -> POP f xss
hcpure, cpure_NP  :: All c xs
                  => proxy c 
                  -> (forall a. c a => f a) 
                  -> NP  f xs
hcpure, cpure_POP :: All2 c xss 
                  => proxy c
                  -> (forall a. c a => f a) 
                  -> POP f xss

type All :: (k -> Constraint) -> [k] -> Constraint
type family All (c :: k -> Constraint) (xs :: [k]) :: Constraint where 
  All c '[]       = ()
  All c (x ': xs) = (c x, All c xs)

-- ============ Injection to NS ============ 

-- counter-part of `hpure` in the case of sum type
type Injection :: (k -> Type) -> [k] -> k -> Type
type Injection f xs = f -.-> K (NS f xs)

apInjs_NP  :: SListI xs  => NP  f xs  -> [NS f xs]
apInjs_POP :: SListI xss => POP f xss -> [SOP f xss]

-- | get which variant the value is
-- generalisation of `isLeft`, `isRight`
hindex, index_NS  :: NS  f xs  -> Int
hindex, index_SOP :: SOP f xss -> Int
```


## 6.3 Embedding Meta-info

```haskell
class Generic a => HasDatatypeInfo a where 
  type family DatatypeInfoOf a :: DatatypeInfo
  datatypeInfo :: proxy a -> DatatypeInfo (Code a)
```

types for datatype info:

```haskell
data DatatypeInfo :: [[Type]] -> Type where 
  -- Standard algebraic datatype
  ADT :: ModuleName          -- string
      -> DatatypeName        -- string
      -> NP ConstructorInfo xss
      -> POP StrictnessInfo xss
      -> DatatypeInfo xss
  -- Newtype
  Newtype :: ModuleName      -- string
          -> DatatypeName    -- string
          -> ConstructorInfo '[x]
          -> DatatypeInfo '[ '[x] ]

data ConstructorInfo :: [Type] -> Type where
  Constructor :: SListI xs 
              => ConstructorName 
              -> ConstructorInfo xs
  Infix       :: ConstructorName 
              -> Associativity   -- right or left
              -> Fixity          -- Int, precedence
              -> ConstructorInfo '[x, y]
  Record      :: SListI xs
              => ConstructorName
              -> NP FieldInfo xs
              -> ConstructorInfo xs

data FieldInfo :: Type -> Type where
  FieldInfo :: FieldName -> FieldInfo a
```


## 6.4 Bridging `GHC.Generics`

The module `Generics.SOP.GGP` defines ways to derive SOP Generics from GHC Generics.

```haskell
import GHC.Generics (Generic(..)) as GHC
gfrom :: (GFrom a, GHC.Generic a) => a -> SOP I (GCode a) 
gto   :: (GTo a,   GHC.Generic a) => SOP I (GCode a) -> a
gdatatypeInfo :: GDatatypeInfo a 
              => proxy a -> DatatypeInfo (GCode a) 
```

types:

```haskell
type GFrom = GSumFrom (GHC.Rep a) 
class GSumFrom (a :: Type -> Type) where
  gSumFrom :: a x     -> proxy xss -> SOP I (ToSumCode a xss)
  gSumSkip :: proxy a -> SOP I xss -> SOP I (ToSumCode a xss)
  
type family ToSumCode (a :: Type -> Type) (xs :: [[Type]]) :: [[Type]]
type instance ToSumCode (a :+: b)   xs = ToSumCode a (ToSumCode b xs)

type family ToProductCode (a :: Type -> Type) (xs :: [Type]) :: [Type]
type instance ToProductCode (a :*: b)   xs = ToProductCode a (ToProductCode b xs)
```


# 7 `kind-generics`

`generic-sop` can only express types of kind `Type`, and `GHC.Generics` supports both `Type` and `k -> Type`. `kind-generics` further generalises representable kinds to all kinds.

Our problem is how to encoding type of following kinds:
```haskell
type f0 :: k 
type f1 :: k1 -> k
type f2 :: k1 -> k2 -> k
...
type fn :: k1 -> k2 -> ... -> kn -> k 
...
```

Given the example of the class `Generics.SOP.Generics :: Type -> Constraint`, we need to define the class `GenericK :: k -> Constraint` 

```haskell
class GenericK (f :: k) where 
  type family CodeK f :: [[?]]
  type family RepK f :: ? -> Type 
  fromK :: (? f) a -> (RepK f) a
  toK   :: (RepK f) a -> (? f) a
```

## 7.1 Merge type parameters

Recall the fundamental idea of datatype-generics is to represent a type `f :: k` using an isomorphic type lives a much smaller universe. The function pair `fromK/toK` are the two arrows of the isomorphism.

Writing down `fromK/toK`'s type is easy when `k` is known:
- if `k` == `Type` then `fromK :: f -> RepK f`;
- if `k` == `k1 -> Type`, then `fromK :: f a -> RepK f a`;
- if `k` == `k1 -> k2 -> Type`, then `fromK :: f a b -> RepK f a b`.
but we do not know `k` and the _arity_ of `f`, i.e., how many type parameters `f` needs to get a `Type`.

_**We need to merge all `f`'s type parameters into one single type parameter**_.

```haskell
fromK :: (? f) a -> (RepK f) a
```

We need to find a type `(? f) a` isomorphic to `f a1 a2 ... an`. 

> [!Idea] 
> Look carefully at `(? f) a ~ f a1 ... an`, the type level function `?` looks quite like `uncurry`. So we want `a` to be something like `(a, ...., an)`, or heterogeneous list, and `?` to be type level uncurry.

```haskell
-- | LoT stand for list of types
data LoT where 
  LoT0   :: LoT Type
  (:&&:) :: k -> LoT ks -> LoT (k -> ks)

type family (f :: k) :@@: (x :: LoT k) where 
  (f :: Type)    :@@: 'LoT0                         = f
  (f :: k -> ks) :@@: ((x::k) ':&&: (xs :: LoT ks)) = f x :@@: xs 
```
the type `LoT`(List of Types) is much like the `uncurry` function at type level and `:@@:` is like `curry`

```haskell
class Generic (f :: k) where
  type family CodeK f :: [[?]]
  type family RepK f :: LoT k -> Type = Interpret (CodeK f)
  fromK :: f :@@: x -> RepK f x
  toK   :: RepK f x -> f :@@: x 

type Interpret :: [[?]] -> LoT ? -> Type
```


## 7.2 Define `CodeK`

Let's focus on the related part of the type class `GenericK` and try to fill the blank `?` and `??`,
```haskell
class GenericK (f :: k) where 
  type family CodeK f :: [[?]]
  type family RepK  f :: LoT k -> Type = Interpret (CodeK f)
  fromK :: f :@@: x -> RepK f x
  toK   :: RepK f x -> f :@@: x

type Interpret :: [[?]] -> LoT ?? -> Type
```

 First observation is that `??` must be `k`, which requires `k` must show in `?` to guide the compiler to kind-check the type level function `Interpret`.

```haskell
type CodeK     :: k -> [[Field k]] 
type Interpret :: [[Field k]] -> LoT k -> Type
```

Recall in `generics-sop`, `Interpret = NS (NP I)`, in our case we need to replace `I` with something takes a `Field k` and a `LoT k`. 




# 8 References

1. https://wiki.haskell.org/Scrap_your_boilerplate
2. Lämmel, R., & Jones, S. P. (2003). Scrap Your Boilerplate: A Practical Design Pattern for Generic Programming. _SIGPLAN Not._, _38_(3), 26–37. [https://doi.org/10.1145/640136.604179](https://doi.org/10.1145/640136.604179)
3. de Vries, E., & Löh, A. (2014). True sums of products. _Proceedings of the 10th ACM SIGPLAN Workshop on Generic Programming_, 83–94. [https://doi.org/10.1145/2633628.2633634](https://doi.org/10.1145/2633628.2633634)
4. Löh, A. (2020). A Generic Journey—Design decisions about datatype-generic programming in Haskell. _A Generic Journey_. [https://github.com/well-typed/gp-zurihac-2020/tree/master](https://github.com/well-typed/gp-zurihac-2020/tree/master)
5. Serrano, A., & Miraldo, V. C. (2018). Generic programming of all kinds. _Proceedings of the 11th ACM SIGPLAN International Symposium on Haskell_, 41–54. [https://doi.org/10.1145/3242744.3242745](https://doi.org/10.1145/3242744.3242745)


