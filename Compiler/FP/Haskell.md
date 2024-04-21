
# Tricks

Here are some misc tricks about programming in Haskell

## Composing left folds

The [foldl](https://hackage.haskell.org/package/foldl) library supports composing left folds in an _applicative_ style.

>[!warning]
> use `foldl'` with strict types, in other words, do not forget the **bang!!!**

### Problem of isolated left folds

>[!error]
> Run multiple folds on the **same** variable can lead to _space leak_.

 The reason behind is that `foldl'` is usually used to exploit the benefits of _stream_, which is constant space complexity. However, if the "stream" is used for multiple time, GHC is tending to cache the "stream" which will leads to space leak. 

Thus we need a way to compose multiple `foldl'` into one and run only once.

### Solution

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

### Common Practice 

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

## The foldr-build Pattern

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
>  = 1 `cons` 2 `cons` nil
> ```
> The idea is that we can get rid of the intermediate list by _optimise out the call of packing and unpacking List constructors_

### the foldr-build optimisation

```haskell
-- rewrite rule pragma
{-# RULES
"foldr/builder" 
  forall nil cons (builder :: forall r. (a -> r -> r) -> r -> r).
  Prelude.foldr cons nil (build builder) = builder cons nil
#-}
```

>[!warning]
>the above rewrite rule requires the `build` function to be not inlined before it is triggered, need to add `{-# NOINLINE [1] build #-}` to force no-inline before phase 1.

the optimisation comes from the paper [A short cut to deforestation](https://dl.acm.org/doi/10.1145/165180.165214)

### the foldr-build trick

1. try convert the implementation using `foldr`


```haskell
sum :: Num a => [a] -> a
```

### References

How does foldr/build elimination work? (2024, January 24). _How Does Foldr/Build Elimination Work?_ [https://tylerhou.com/posts/foldr-build-elimination/](https://tylerhou.com/posts/foldr-build-elimination/)

