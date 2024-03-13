#property-based-testing  #Haskell 

# Recall manual and integrated shrinking

## Manual Shrinking

The following is a simplified version of [[QuickCheck]]'s shrinker (which is actually the `Arbitrary` typeclass).

```haskell
data Gen a = MkGen 
  { gen :: StdGen -> a 
  , shrink :: a -> [a]
  }
```
the problem of this structure is that it is even not a functor.

## Integrated Shrinking

The following is a simplified version of [[Hedgehog and Integrated shrinking|Hedgehog]]

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


# Internal shrinking

The problem of _manual shrinking_ is that its not a `Monad` even not an `Applicative` or `Functor`. _Integrated shrinking_ only provides a **partial solution** in the sense that it is a `Functor` and an `Applicative`, even a `Monad`, but the `Monad` interface is **unsafe**, in most cases, the derived shrinker _cannot_ reach the _minimal_ counter-example.


```haskell
data Gen a = PRNG -> (a, PRNG)
```
