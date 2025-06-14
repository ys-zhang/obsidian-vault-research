#property-based-testing  #Haskell #program-test 


_falsify_ combines _internal shrinking_ and _integrated shrinking_ by defining a generator as a parser of a random tree

```haskell
data STree 
  = MkSTree Sample STree STree
  | Minimal
  
-- | 2 different constructors is for
-- recording whether a sample has been shrunk
data Sample 
  = Sample   { value :: Word }
  -- ^ have been shrunk
  | Unshrunk { value :: Word }
  -- ^ have not been shrunk

newtype Gen a = Gen { gen :: STree -> (a, [STree]) }
```

>[!rmk]
>The list of `STree` returned from the generator is options for the shrinking step.
>
>``` haskell
> shrinkOptions :: Gen a -> STree -> [a]
> shrinkOptions g seed = 
>   let (_, ss) = gen g seed
>   in  (fst . g) <$> ss
>```

>[!rmk]
>The intention of `STree` is that we either
>- use the sample or 
>- both of the subtrees (corresponding to a split PRNG).
>
> The point of using a tree instead of a choice sequence is that
> _different fields consumes difference sub-trees which have no intersection_, thus shrink one field will not affect the others.  

>[!rmk]
> Conceptually, a sample tree is always _infinite_; `Minimal` corresponds to the tree that is zero everywhere.
> 
> `Minimal` shall be mapped to the _minimum in shrink order_ by all generators 

```haskell
-- pattern synonym for STree
{-# LANGUAGE PatternSynonyms #-}

view :: STree -> (Sample, STree, STree)
view (MkSTree s l r) = (s, l, r)
view Minimal = (Sample 0, Minimal, Minimal)

-- | Inf short for infinite
pattern Inf :: Sample -> STree -> STree -> STree
pattern Inf s l r <- (view -> s l r)
```

# Generator as a Monad

```haskell
instance Monad Gen where
  return x = Gen $ const (x, [])
  ga >>= f = Gen $ \(Inf s l r) -> 
    let (a, ls) = gen ga l
        (b, rs) = f a r
    in  (b, combine s l r ls rs)
  
combine :: Sample -> STree -> STree -> [STree] -> [STree] -> [STree]
combine s l r ls rs = shortcut $ 
     [STree s l' r | l' <- unlessMinimal l ls] 
  ++ [STree s l r' | r' <- unlessMinimal r rs] 
 where 
  unlessMinimal :: STree → [a] → [a] 
  unlessMinimal Minimal _ = [] 
  unlessMinimal _ xs = xs 
  -- shortcut makes the shrinker first take an attempt
  -- to greedly shrink to minimal 
  shortcut :: [STree] → [STree] 
  shortcut [] = [] 
  shortcut ts = Minimal : ts
```

To understand the `combine` function, first note that 
- `ls` corresponding to shrinking the _prior_ $a$;
- `rs` corresponding to shrinking the _conditional_ $b|a$[^1].

We put `ls` on the front of the result list as we'd like first greedily shrinks the prior. It's relative easy to see that _`rs` must be combined with `l`_, since
- the combined result will be run by the _same_ generator;
- `rs` shrinks $b|a$ which requires we keep $a$ unchanged; and 
- `l` generates $a$.

[^1]: forgive me for abusing the notation, actually it is $b$ however its better to make the dependency clear.`

>[!note]
> This `Monad` instance first shrinks the prior (left) as much as possible and then shrinks conditional (right). Moreover, it can _go back to shrinking the prior after starting shrinking the conditional_.
>
> The reason here that we can go back is that in internal shrinking once we have shrinking options, we need to rerun the generator to get the real sample which will also generates a new list of shrinking options containing ones that shrinks the prior; however, in integrated shrinking we have no opportunity to rerun the generator.
>
> _the shrinking result of the right sub-tree is not lost when go back shrinking the left sub-tree_

>[!warning] pitfall
> There is one pitfall when writing generator for internal shrinking. Each bit or word of the choice sequence or sample tree corresponding to 
> - some field of some type
> - choice of data constructor
> - control flow like `if then else`
>
> It is favourable if we can assure these correspondences **do not change** during shrinking. 
>
> It is not recommended to introduce random on `if`'s _condition expression and at least one of its branches_, as when the condition shrinks, some bits can be used to generate difference fields. A solution is generate both branches and use a if to choose. 

Comparing with [[Hypothesis and Internal shrinking]], the main advantage is that it supports generating and shrinking functions.

## selective shrinking

In some cases we want to skip shrinking part of the sub-tree if it is unused in the result. recall [[Applicative Functor#Selective Applicative Functors]], which supports _speculative execution_,

```haskell
class Applicative f => Selective f where
  select :: f (Either a b) -> f (a -> b) -> f b

instance Selective Gen where
  select e f = Gen $ \(Inf s l r) -> 
    let (e', ls) = gen e l
    in  case e' of 
          Left a  -> 
            let (f', rs) = gen f r
            in  (f' a, combine s l r ls rs)       
          Right b -> 
            -- skip shrinking the right subtree
            (b, combine s l r ls []) 
```

# Practices

```haskell

prim :: Gen Word
prim = Gen $ \(Inf s l r) -> 
  let word = value s 
      opts = [STree (Sample w) l r | w <- shrinkWord word]
  in  (word, opts)

-- shrinks towards False
bool :: Gen Bool
bool = do 
  w <- prim
  return $ w >= (maxBound `div` 2) 

-- [0, 1]
fraction :: Gen Double
fraction = frac <$> prim
 where 
  frac x = fromIntegral x / range
  range = fromIntegral (maxBound :: Word)

-- [-1, 1]
signedFraction :: Gen Double
signedFraction = aux <$> fraction <*> fraction 
 where 
  aux x y | x <= y    = x
          | otherwise = negate y

-- n -> [0, n]
below :: Word -> Gen Word 
below n = (round . ( * fromIntegral n)) <$> fraction

{- The following impl is buggy 
  choose :: Gen a -> Gen a -> Gen a 
  choose gl gr = do 
    l <- gl
    r <- gr
    cond <- bool
    return if cond then l else r
suppose `bool` generates `True`, then gr will not be used
and the right sub-tree will definitely be shrank to `Minimal`
and be kept
then `bool` shrinks to `False`, we loose the chance to shrink
the right sub-tree correctly

in the correct impl, if the left sub-tree is acturally use,
the right sub-tree shrinking will not be included 
in the shrinking options, and verse-visa is the same.
-}
choose :: Gen a -> Gen a -> Gen a    
choose = ifS bool
```

manual shrinking, from QuickCheck:
```haskell
-- make a new generator from the given one 
-- by changing its shrinker
shrinkWith :: (a -> [a]) -> Gen a -> Gen a
shrinkWith f gen = do 
  x <- initial gen
  fromShrinkTree $ unfoldTree (\x' -> (x', f x')) x

-- make a new generator from the given one that
-- it does not shrink
initial :: Gen a -> Gen a
initial (Gen g) = Gen $ second (const []) . g 

fromShrinkTree :: Tree a  -- a rose tree
               -> Gen a
fromShrinkTree (Node x xs) = do 
  next <- Nothing `shrinkTo` map Just xs
  case next of 
    Nothing -> return x
    Just x' -> fromShrinkTree x'

shrinkTo :: a -> [a] -> Gen a 
shrinkTo x xs = Gen $ \(Inf s l r) ->
  let setNext s' _ = STree (Sample s') l r 
  in  case s of 
        Unshrunk _ -> 
          (x, zipWith setNext [0..] xs) 
        Sample i -> 
          (index i xs, []) 
 where 
  index :: Word -> [a] -> a 
  index _ [] = x 
  index _ [y] = y 
  index 0 (y:_) = y 
  index n (_:ys) = index (n - 1) ys
```

