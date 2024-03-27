Here are some misc tricks about programming in Haskell


# Composing left folds

The [foldl](https://hackage.haskell.org/package/foldl) library supports composing left folds in an _applicative_ style.

>[!warning]
> use `foldl'` with strict types, in other words, do not forget the **bang!!!**

## Problem of isolated left folds

>[!error]
> Run multiple folds on the **same** variable can lead to _space leak_.

 The reason behind is that `foldl'` is usually used to exploit the benefits of _stream_, which is constant space complexity. However, if the "stream" is used for multiple time, GHC is tending to cache the "stream" which will leads to space leak. 

Thus we need a way to compose multiple `foldl'` into one and run only once.

## Solution

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

## Common Practice 

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


   