#Haskell #fold

# 1 Fold Tricks

Here are some misc tricks about programming in Haskell

## 1.1 Composing left folds

The [foldl](https://hackage.haskell.org/package/foldl) library supports composing left folds in an _applicative_ style.

>[!warning]
> use `foldl'` with strict types, in other words, do not forget the **bang!!!**

### 1.1.1 Problem of isolated left folds

>[!error]
> Run multiple folds on the **same** variable can lead to _space leak_.

 The reason behind is that `foldl'` is usually used to exploit the benefits of _stream_, which is constant space complexity. However, if the "stream" is used for multiple time, GHC is tending to cache the "stream" which will leads to space leak. 

Thus we need a way to compose multiple `foldl'` into one and run only once.

### 1.1.2 Solution

The solution is quite classical, we create a new data type representing the computation of folding and make the data type composable, and we add a `run` function to actually execute the computation.
```haskell
-- | a computation of fold a collection of `a` to a `b`
data Fold a b = forall x . Fold (x -> a -> x) x (x -> b)
```
this is an existential type over `x` which is introduced to make `Fold a` and applicative.

```haskell
-- strick tuple
data STuple a b = STuple !a !b 

instance Functor (Fold a) where
  fmap (Fold step begin done) = Fold step begin (f . done)
  
instance Applicative (Fold a) where
  pure x = Fold const () (const x)
  mf <*> mx = 
    let (Fold fstep fbegin fdone) = mf
        (Fold xstep xbegin xdone) = mx
    in Fold 
        (\(STuple x x') a -> STuple (fstep x a) (xstep x' a))
        (STuple fbegin xbegin)
        (\(STuple x x') -> fdone x (xdone x'))

fold :: Foldable f => Fold a b -> f a -> b
fold (Fold step begin done) fa = done $ foldl' step begin fa
```

### 1.1.3 Common Practice 

```haskell
data Stats = 
  Stats { statA :: A 
        , statB :: B } 
main :: IO () 
main = do 
  stream <- readSteam ...
  let foldA = Fold stepA beginA id
      foldB = Fold stepB beginB id 
      foldStats = Stats <$> foldA <*> foldB
  return $ fold foldStats stream
```

## 1.2 The foldr-build Pattern

Both `foldr` and `build` is in `Prelude`.

```haskell
foldr :: (a -> r -> r) -> r -> [a]  -> r
foldr cons nil = go 
 where 
  go [] = nil
  go (x:xs) = cons x $ go xs

build :: (forall r . (a -> r -> r)    -- ^ inductive constructor
                   -> r               -- ^ base constructor
                   -> r) 
      -> [a]
build builder = builder (:) []
```   

>[!tip] observation 
>`foldr` and `build` cancels each other
> ```haskell
> foldr cons nil (build $ \cons' nil' -> 1 `cons'` 2 `cons'` nil') 
>  = 1 `cons` (2 `cons` nil)
> ```
> The idea is that we can get rid of the intermediate list by _optimise out the call of packing and unpacking List constructors_

### 1.2.1 the foldr-build optimisation

> the optimisation comes from the paper [A short cut to deforestation](https://dl.acm.org/doi/10.1145/165180.165214)

Define a rewrite rule using [rewrite rule pragma](https://downloads.haskell.org/~ghc/7.0.1/docs/html/users_guide/rewrite-rules.html):

```haskell
{-# RULES
"foldr/builder" 
  forall nil 
         cons 
         (builder :: forall r. (a -> r -> r) -> r -> r) 
  . 
  foldr cons nil (build builder) = builder cons nil
#-}
```

>[!warning]
>the above rewrite rule requires the `build` function to be not inlined before it is triggered, need to add `{-# NOINLINE [1] build #-}` to force no-inline before phase 1.
>
>GHC uses a _countdown phase number_, in other words, smaller number phases are executed later

>[!Note] check rules that fired 
> #ghc-options
>
> the GHC option `-ddump-rule-firings` tells the compiler to print the rules fired during compilation when _applied to the main module_
>
> _You also need `-O` to use with ghc`_

### 1.2.2 the foldr-build trick

the trick is to convert all list computation into `foldr`, to see how to do that we try to write `foldl'` using `foldr`

```haskell
foldl' :: (r -> a -> r) -> r -> [a] -> r
foldl' comb init = go init
 where 
  go !acc [] = acc
  go !acc (x:xs) = go (comb acc x) xs

-- GOAL:
foldl'_goal :: (r -> a -> r) -> r -> [a] -> r
foldl'_goal comb init xs = 
  foldr cons nil xs init
 where
  cons :: (a -> ? -> ?)
  nil :: ?
```

```haskell
foldl'_phase1 :: (r -> a -> r) -> r -> [a] -> r
foldl'_phase1 comb init xs = go xs init
 where 
  go [] !acc = acc           
  go (x:xs) !acc = go (comb acc x) xs

foldl'_phase2 :: (r -> a -> r) -> r -> [a] -> r
foldl'_phase2 comb init xs = go xs init
 where 
  go [] = \!acc -> acc           
  go (x:xs) = \!acc -> go (comb acc x) xs

foldl'_goal :: forall r a 
             . (r -> a -> r) -> r -> [a] -> r
foldl'_goal comb init xs = 
  foldr cons nil xs init
 where
  nil  :: r -> r
  cons :: a -> (r -> r) -> (r -> r)
  nil       = \go -> go
  cons x go = \!r -> go (comb r x)
```

example for map 
```haskell
map' :: (a -> b) -> [a] -> [b]
map' f xs = go xs [] 
 go [] = \acc -> acc
 go (x:xs) = \acc -> f x: acc 
 
map'' :: (a -> b) -> [a] -> [b]
map'' f as = 
  foldr 
    (\a go -> \bs -> f a : go bs)  -- go :: [b] -> [b]
    id 
    as 
    []  -- [] :: [b]


```
### 1.2.3 References

1. How does foldr/build elimination work? (2024, January 24). _How Does Foldr/Build Elimination Work?_ [https://tylerhou.com/posts/foldr-build-elimination/](https://tylerhou.com/posts/foldr-build-elimination/)
2. <iframe width="560" height="315" src="https://www.youtube.com/embed/C-GahictORU?si=z9OxSKnqSVBYjDKL" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" referrerpolicy="strict-origin-when-cross-origin" allowfullscreen></iframe>

## 1.3 Generic fold and unfold

this is highly related to [[F-algebra]] and Coalgebra

### 1.3.1 Recursive functors

```haskell
-- | a type class for Recursive types, for instane List, Tree
class Functor (RecF a) => Rec a where
  -- | the "F" stands for "Functor"
  -- all recursive fields shall be replaced with r
  data RecF a :: Type -> Type
  pull :: a -> RecF a a
  push :: RecF a a -> a

instance Rec [a] where
  data instance RecF [a] r = NilF | ConsF a r 
    deriving (Functor)
  pull [] = NilF
  pull (x:xs) = ConsF x xs
  push NilF = []
  push (ConsF x xs) = x:xs

instance Rec (Tree a) where
  data instance RecF (Tree a) r = NodeF a [r]
    deriving Functor
  pull (Node x ts) = NodeF x ts 
  push (NodeF x ts) = Node x ts

```

### 1.3.2 generic fold

```haskell
-- | we name it as `gfoldr` to indicate its a generalisation of 
-- `foldr` (not `foldl`) in the case of list
gfoldr :: forall t r . Rec t => (RecF t r -> r) -> t -> r
gfoldr algebra = go 
 where 
  go :: t -> r
  go = algebra . fmap go . pull -- fmap go :: RecT t t -> RecT t r
```

```haskell
foldr' :: (a -> r -> r) -> r -> [a] -> r
foldr' cons nil = gfoldr $ \case 
  NilF -> nil
  ConsF a r -> cons a r
```


### 1.3.3 generic unfold

```haskelle
-- | gunfold is a generalisation of the @build@ function 
gunfold :: forall t r . Rec t => (r -> RecF t r) -> r -> t
gunfold coalgebra = go . coalgebra
 where 
  go :: RecF t r -> t
  go = push . fmap (gunfold coalgebra) 
```

the following actually cannot type check in current GHC (9.10)

```haskell
build' :: (forall r . (a -> r -> r) -> r -> r) -> [a]
build' = gunfold g 
 where
  g :: (forall r . (a -> r -> r) -> r -> r) 
     -> RecF [a] (forall r . (a -> r -> r) -> r -> r)
  g f = f (_ . ConsF) NilF

ConF :: a -> r -> Rec [a] r
```

### The `recursive-shcemes` package

```haskell
-- this is the RecF 
type family Base t :: * -> *
```

### 1.3.4 References

1. [Hackage: recursion-schemes](https://hackage.haskell.org/package/recursion-schemes)
2. SHEARD, T., & PASALIC, E. (2004). Two-level types and parameterized modules. _Journal of Functional Programming_, _14_(5), 547–587. [https://doi.org/10.1017/S095679680300488X](https://doi.org/10.1017/S095679680300488X)


# 2 Variable arity functions

variable arity function is used in the `lucid2` template.

## 2.1 An example of `sum`

```haskell
-- a sum example 
-- >>> sum :: Int
-- 0
-- >>> sum 1 :: Int
-- 1
-- >>> sum 1 2 :: Int
-- 3

sum :: Sum a => a
sum = sum' 0

class Sum a where
  sum' :: Int 
       -- ^ the accumulated value 
       -> a
       -- ^ the result

instance Sum Int where
  sum' acc = acc

instance (b ~ Int, Sum a) => Sum (b -> a) where
  sum' :: Int -> b -> a
  sum' acc i = sum' (acc + i)
```

## 2.2 Collecting variable arity args to list

collect a specific type

```haskell
class Collect a where
  collect' :: [Int] -> a 

instance Collect [Int] where 
  collect' = reverse

instance (b ~ Int, Collect a) => Collect (b -> a) where
  collect' :: [Int] -> b -> a 
  collect' xs x = collect' (x:xs) 

collect :: Collect a => a
collect = collect' []
```

collecting heterogeneous list

```haskell
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds #-}

data HList a where 
  HNil :: HList '[]
  HCons :: a -> HList as -> HList (a ': as)
  
class HCollect xs a  | a -> xs where
  hcollect' :: HList xs -> a 

instance Collect xs (HList xs) where 
  hcollect' = id

instance  Collect (x:xs) a => Collect xs (x -> a) where
  hcollect' :: HList xs -> b -> a 
  hcollect' xs x = hcollect' (HCons x xs) 

hcollect :: HCollect '[] a => a
hcollect = hcollect' HNil

isHList :: HList xs -> HList xs
isHList = id
```


# Open recursion & OOP

the trick is about how to simulate class^[https://well-typed.com/blog/2018/03/oop-in-haskell/] in Haskell.

```haskell


```



