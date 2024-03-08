#property-based-testing  #Haskell 

# Manual Shrinking

The following is an simplified version of [[QuickCheck]]'s shrinker (which is actually the `Arbitrary` typeclass).

```haskell
data Gen a = MkGen 
  { gen :: StdGen -> a 
  , shrink :: a -> [a]
  }
```
the problem of this structure is that it is even not a functor.

# Integrated Shrinking

> Instead of manually pairing each generator with a shrinker, in integrated shrinking the generator itself produces a value and all possible ways to shrink that value.


```haskell
type Gen a = StdGen -> Tree a

data Tree a = Node a [Tree a]

shrink :: (a -> Bool) -> Tree a -> a
shrink p (Node x xs)
  | p a       = error "not a counter example"
  | otherwise = case xs of
      []  -> a
      _   -> shrink p $ head [x | x@(Node a _) <- xs, not (p a)]
```

1. `Gen` is a Functor
2. `Gen` is a Applicative

## Monadic composition

> Monadic composition is required when the behaviour of a generator depends on previously generated values.

# Internal shrinking

```haskell
data Gen a = PRNG -> (a, PRNG)
```
