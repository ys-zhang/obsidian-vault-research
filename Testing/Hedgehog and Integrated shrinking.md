#property-based-testing #program-test 
#Haskell 

In [[QuickCheck]] one have to write the shrinking method manually, so _can we automatically deriving the shrinking function?_

_Hedgehog_ uses a method called _integrated shrinking_ to generate shrink functions automatically.

>[!tldr]
> one can automatically get a good and _safe_ shrinker through its `Applicative` interface.
> there is _unsafe_ automatically derived shrinker through the `Monad` interface, where _unsafe_ means it does not guarantee to reach the minimal counter-example (in fact, it is quite rare to reach the minimal)


# Integrated shrinking

ref: [Integrated versus Manual Shrinking](https://www.well-typed.com/blog/2019/05/integrated-shrinking/)

in [[QuickCheck]] shrinking and generation is defined separately

```haskell
class Arbitrary a where 
  arbitrary :: QCGen -> Size -> a
  shrink :: a -> [a]
  
type Size = Int
```

while in _Hedgehog_

```haskell
newtype Integrated a = Integrated { StdGen -> Tree a }
data Tree a = Node { root :: a, subTrees :: [a] }

mkIntegrated :: (StdGen -> a)  -- generator
             -> (a -> [a])     -- shrinker
             -> Integrated a   -- integrated 
mkIntegrated g s = Integrated \r -> unfoldTree s (g r)

unfoldTree :: (a -> [a]) -> a -> Tree a 
unfoldTree gen initial = 
  Node initial (unfoldTree gen <$> gen initial)
```

>[!tip] key idea
>The key idea is straight-forward enough: instead of having the generator producing a single value, it will instead produce a _tree_ of values. The root of the tree will correspond to the original value produced, the immediate children of the root correspond to the immediate shrink steps from the root, and so on.

It is easy to see that `Integrated` is both `Functor` and `Applicative`.

```haskell
instance Applicative Integrated where 
  pure x = Integrated $ const (Node x [])
  (Integrated mf) <*> (Integrated mx) = 
    Integrated $ \r -> 
      let (r1, r2) = Data.Random.split r
      in  interleave (mf r1) (mx r2)
   where
    -- try shrink left tree greedly then shrink right
    interleave :: Tree (a -> b) -> Tree a -> Tree b
    interleave l@(Node f ls) r@(Node x rs) =
      Node 
        (f x) 
        ([interleave l' r | l' <- ls] ++ [interleave l r' | r' <- rs]) 
```

>[!important]
> Its critical to keep one of left and right tree untouched, this insures _no matter how deep we goes down one side of the tree, we are still able to go back to the original outer state and shrinking the outer tree from there_.

# Monad instance for integrated shrinking

Trouble with implementing `Monad` for `Integrated`.

> Monadic composition is required when the behaviour of a generator depends on previously generated values.

the problem is how to implement `join`, one choice is 

```haskell
-- | prefers to shrink inner first and never go back to start
-- shrinking the outer
join :: Tree (Tree a) -> Tree a
join (Node (Node x xs) xss) = Node x (xs ++ map join xss)

-- | prefers to shrink outer first and never go back to start
-- shrinking the inner
join' :: Tree (Tree a) -> Tree a
join' (Node (Node x xs) xss) = Node x (map join xss ++ xs)
```

>[!error]
> Neither `join` or `join'` satisfies out intension.
> Once we start shrinking the _inner_ `Tree` tree, we will **never** be able to shrink the _outer_ tree again

# Toolbox for deriving integrated generators

```haskell
freeze :: Integrated a -> Gen (Tree a)
freeze (Integrated f) = Gen f

unfreeze :: Gen (Tree a) -> Integrated a
unfreeze (Gen f) = Integrated f 

don'tShrink :: Integrated a -> Gen a
don'tShrink (Integerted f) = Gen (root . f)
```

by turning an integrated generator to a generator of trees, we forget about how to shrink the value.

```haskell
class AutoShrink f where
  genAux :: Integrated a -> Gen (f (Tree a))
  interleave :: f (Tree a) -> Tree (f a)
  
liftAutoShrink :: AutoShrink f => Integrated a -> Integrated (f a)
liftAutoShrink genA = unfreeze $ do 
  f_tree_a <- genAux genA
  return $ interleve f_tree_a
```

## generate from list
```haskell
iList :: Integrated Word -- gen list length
      -> Integrated a    -- gen elements
      -> Integrated [a]
iWord :: Word            -- upper bound
      -> Integrated Word
      
fromList :: [a] -> f a
genFromList :: Integrated a -> Integrated (f a)
genFromList genA = fromList <$> iList (iWord ...) genA
```