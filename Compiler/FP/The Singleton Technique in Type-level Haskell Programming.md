#Haskell #type-level-programming 
#dependent-type 

> Dependent types in Haskell can be approximated via **singletons**, which are to be understood as an _isomorphism between terms and values_.

# The Problem

Suppose we have a promoted kind `k` with a collection of promoted types `a :: k`.

```
'True ~ Sing 'True ~ STrue ~ True
```

Suppose we want to write a function $f : X \to Y$ whose return type $Y$ is determined by the value of its argument $x: X$. In _Dependent Type Theory_ $f$ belongs to the [[Dependent Type Theory#Dependent Product (dependent function)|dependent product]] 

$$f \in \prod_{x\in X} Y(x) = \forall x\in X,\; Y(x)$$
the problem is how to express the $f$ in Haskell.

## How to express `Y`?

The 1st problem is we need to write the type $Y(x)$ in Haskell, but $Y$ has the type (or kind) 

$$ Y : X \to \text{Type} $$
```haskell
{-# LANGUAGE KindSignature #-}
type Y :: ? -> *
```
The point here is there is nothing we can put at question mark position, because `X` is a type while `*` is a kind. 

The trick here is to lift $X$ to kind and use _type family_ to express $Y$

|                     | Unlifted | Lifted |
| ------------------- | -------- | ------ |
| Kind (compile time) | -        | $X$    |
| Type (compile time) | $X$      | $'x$   | 
| Value (runtime)     | $x$      |        |

```haskell
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignature #-}
{-# LANGUAGE DataKinds #-}


type Y :: X -> Type
type family Y x where 
  Y x1 = ...
  Y x2 = ...
```


## How to express `f`?

The first attempt to express `f` as 

```haskell
f :: forall x::X . x -> Y x
```

however there are 2 problems of this approach
1. the arrow type has the signature `(->) :: Type -> Type` but `x :: X` has the kind `X`, thus the above will not type check
2. In Haskell, only the kind `Type` has inhabitable types, which no `x :: X` has habitants/values. 

The solution is to have a _type level function_ 
$$\Sigma : X \to \text{Type}$$
to map kind `X` to kind `Type`, the result type $\Sigma('x)$ must be a singleton $\Sigma('x) = \{\sigma(x)\}$, since `x` itself the only value instance that make sense.


```haskell
f :: forall x::X. Simga x -> Y x  
```

>[!Note] Implicit Argument
> notice that the information of value `x` is contained in the type level signature `Y x`, thus we can recover the value of argument `Sigma x`.
> ```haskell
> class SingI (x :: X) where 
>   sing :: Sing a
>
> f  :: forall x :: X . Sing x -> Y x 
> 
> f' :: forall x::X . Y x
> f' = f sing
> ```
> Notice the signature of `f'` is exactly the same as the _dependent product_ $\forall x\in X, \; Y(x)$.
>  


There are several ways to define $\Sigma$

1. data family, or type family 
2. GADTs

```haskell
type Sigma :: X -> Type
data family Sigma x where
  Sigma 'x1 = S1
  Sigma 'x2 = S2
  ...

data Sigma x where 
  S1 :: Sigma x1
  S2 :: Sigma x2
  ...
```


## Three Isomorphisms

>[!NOTE] Isomorphism
> $$ x \; \simeq \; \sigma(x) \; \simeq \; \Sigma('x) \; \simeq \; 'x $$
> we have 3 isomorphisms: 
> 1. $\Sigma('x) \; \simeq \; 'x$
> 2. $\sigma(x) \; \simeq \; \Sigma('x)$ is directly expressed by the definition of $\Sigma$
> 3. $x \; \simeq \; \sigma(x)$ can be expressed by a pair of function at value level


```haskell
-- follow Coq's syntax
data Nat = Z | S Nat
```


1. implement the singleton type  

```haskell
-- | a type level function maps a kind 
-- to its Singleton Type Collection
type family Sing :: k -> Type 

type instance Sing = SNat
data SNat (n :: Nat) where 
  SZ :: SNat Z
  SS :: SNat n -> SNat (S n)
```


>[!NOTE] Type Family must be satuated
> the reason why `Sing` is not defined as `type family Sing (a :: k) :: Type` is to make partial application possible.
> ```haskell
> type family Or (a :: Bool) (b :: Bool) where 
>   Or 'True = 'False
>   Or 'False = 'True
>
> type family Map (f :: a -> b) (i :: [a]) :: [b] where 
>   Map f '[] = '[]
>   Map f (x ': xs) = (f x) ': Map f xs
>
> type MapOr = Map Or -- this will be a type error
> ```
> See _Chapter 10: First Class Families_ in Reference 1 


# `Data.Singletons`

[For more details see this](https://github.com/goldfirere/singletons/blob/master/README.md#definitions-used-to-support-singletons)

```haskell
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignature #-}

type Sing :: k -> Type 
type family Sing 

type SingI :: forall {k}. k -> Constraint
class SingI a where
  sing :: Sing a

type SomeSing :: Type -> Type
data SomeSing k where
  SomeSing :: Sing (a :: k) -> SomeSing k

-- | this is acturally a kind class, not type class
type SingKind :: Type -> Constraint
class SingKind k where
  type Demote k :: *
  fromSing :: Sing (a :: k) -> Demote k
  toSing   :: Demote k -> SomeSing k

type SingInstance :: k -> Type
data SingInstance a where
  SingInstance :: SingI a => SingInstance a
singInstance :: Sing a -> SingInstance a

type SingI1 :: forall {k1 k2}. (k1 -> k2) -> Constraint
class (forall x. SingI x => SingI (f x)) => SingI1 f where
  liftSing :: Sing x -> Sing (f x)

type SingI2 :: forall {k1 k2 k3}. (k1 -> k2 -> k3) -> Constraint
class (forall x y. (SingI x, SingI y) => SingI (f x y)) => SingI2 f where
  liftSing2 :: Sing x -> Sing y -> Sing (f x y)
```


## Generate Singletons




# References

1. MAGUIRE, S. (2019). _Thinking with Types_. Leanpub. (Chapter 15. Dependent Types)
2. Eisenberg, R. A., & Weirich, S. (2012). Dependently typed programming with singletons. _SIGPLAN Not._, _47_(12), 117–130. [https://doi.org/10.1145/2430532.2364522](https://doi.org/10.1145/2430532.2364522)
4. Eisenberg, R. A., & Stolarek, J. (2014). Promoting functions to type families in haskell. _SIGPLAN Not._, _49_(12), 95–106. [https://doi.org/10.1145/2775050.2633361](https://doi.org/10.1145/2775050.2633361)
5. https://blog.jle.im/entry/introduction-to-singletons-1.html
6. https://github.com/goldfirere/singletons
