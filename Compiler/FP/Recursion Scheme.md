#catamorphism

Recursion scheme is a collection of tools for writing functions dealing with recursive data types, which can be used to replace pattern matching in functional programming just like "while/for" replacing "goto" in imperative programming.

Given a recursive data type $A\star$, 
- The recursive structure of this definition is employed when writing functions $\in A\star \to B$ that destruct a list; these have been called **catamorphisms** (from the greek preposition $\kappa\alpha\tau\alpha$ meaning _“downwards”_ as in _"catastrophe”_).  
- **anamorphisms** are functions $\in B \to A\star$ (from the greek preposition $\alpha\nu\alpha$ meaning _"upwards”_" as in _"anabolism”_[^anabolism]) that generate a list of type $A\star$ from a seed from $B$. 
- Functions of type $A \to B$ whose **call-tree** has the shape of a cons-list are called **hylomorphisms** (from the Aristotelian philosophy that form and matter are one. $\upsilon\lambda o \sigma$ meaning "dust" or "matter”).

[^anabolism]: (Biochemistry) the synthesis of complex molecules in living organisms from simpler ones together with the storage of energy; constructive metabolism

>[!remark] generic fold and unfold
> _catamorphism_ and _anamorphism_ are also called _generic fold_ and _generic unfold_.

>[!remark] hylo is the composition of cata and ana
>A hylomorphism corresponds to the composition of an anamorphism that builds the call-tree as an explicit data structure and a catamorphism that reduces this data object into the required value.


# 1 Recursive scheme on List

1. catamorphism (`foldr`)
```haskell
cataList :: b -> (a -> b -> b) -> [a] -> b
cataList z _ []     = z 
cataList z k (a:as) = k a $ cataList z k as 
```
2. anamorphism (`unfold`)
```haskell
anaList :: (b -> Bool) -> (b -> (a, b)) -> b -> [a]
anaList p g b 
  | p b       = []
  | otherwise = a: anaList p g b'
 where 
  (a, b') = g b

zip :: ([a], [b]) -> [(a, b)]
zip (as, bs) = anaList stop gen
 where
  stop (as, bs)     = null as || null bs
  gen  (a:as, b:bs) = ((a, b), (as, bs))

iterate :: (a -> a) -> a -> [a]
iterate f = anaList (const False) (\a -> (a, f a))
```
3. hylomorphism
```haskell
hyloList :: c 
         -> (b -> c -> c) 
         -> (a -> (b, a)) 
         -> (a -> Bool)
         -> a -> c 
hyloList c k g p a 
  | p a       = c
  | otherwise = let (b, a') = g a
                     c'     = hyloList c k g p a'
                in   k b c

-- | factorial
fact :: Int -> Int
fact = hylo 1 (*) (\n -> (n, n - 1)) (==0)
```

>[!note]
>The hylomorphism definition of the factorial maybe correct but is unsatisfactory from a theoretic point of view since it is not inductively defined on the data type `{haskell} data Nat = O | S Nat`. 
>
>There is however no 'simple' `g` such that `fac = cata g`. 
>
>The problem with the factorial is that it _"eats its argument and keeps it too (dynamic programming)”_. The brute force catamorphic solution would therefore have `fac` return a pair $(n, n!)$ to be able to compute $(n + 1)!$.

4. paramorphism 
```haskell
paraList :: b 
         -> (a -> ([a], b) -> b) 
         -> [a] -> b
paraList z k []     = z
paraList z k (a:as) = k a (as, paraList z k as)
```

# Generalise to any recursive types

>[!thm] fix point
> Let $F$ be an endo-functor whose operation on morphisms(functions) are _continuous_, then there exists an object(Type) `{haskell} L` and $2$ strict functions
> 1. `{haskell}inF  :: F L -> L`
> 2. `{haskell}outF :: L -> F L`
>
> which are each other's _inverse_, in other words, $L$ is a _fix point_ of functor $F$, we denote the function $\mu$ that maps $F$ to $(inF, L)$.

In Haskell, $\mu$ is implemented as the `Fix` type constructor.
```haskell
data Fix (f :: Type -> Type) = Fix { unFix :: f (Fix f) }
```


# 3 References

1. Meijer, E., Hughes, J., Fokkinga, M. M., & Paterson, R. (1991, August). _Functional Programming with Bananas, Lenses, Envelopes and Barbed Wire_. 5th ACM Conference on Functional Programming Languages and Computer Architecture (FPCA 1991). [https://doi.org/10.1007/3540543961_7](https://doi.org/10.1007/3540543961_7)
2. [[F-algebra]]
3. Catamorphism as fold [[Haskell Tricks#1.3 Generic fold and unfold]]

