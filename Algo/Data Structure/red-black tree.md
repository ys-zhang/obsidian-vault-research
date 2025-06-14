>[!definition] red-black tree
>every red-black tree satisfy the following two balance invariants:
> 1. No red node has a red child.
> 2. Every path from the root to an empty node contains the same number of black nodes.

```haskell
data Color = Red | Black
  deriving (Show, Eq)

data RbTree a 
  = T {-# UNPACK #-} !Color (RbTree a) a (RbTree a) 
  | E
  deriving (Show, Eq)

pattern R :: (RbTree a) -> a -> (RbTree a) -> a
pattern R l v r = T Red l v r

pattern B :: (RbTree a) -> a -> (RbTree a) -> a
pattern B l v r = T Black l v r

{-# COMPLETE R, B, E #-}

prop_inv_1 :: RbTree a -> Bool
prop_inv_1 E = True
prop_inv_1 (B l _ r) = prop_inv_1 l && prop_inv_1 r
prop_inv_1 (R l _ r) = notRed l && notRed r
                         && prop_inv_1 l 
                         && prop_inv_1 r
 where
  notRed = \case R _ _ _ -> False
                 _       -> True

prop_inv_2 :: RbTree a -> Bool
prop_inv_2 = isJust . go
 where
  go :: RbTree a -> Int
  go E           = Just 0
  go (T c _ l r) = do 
    nl <- go l,
    nr <- go r, 
    unless (nl == nr) 
           (fail "different number of black nodes")
    pure (nr + countBlack c)
  countBlack = \case B -> 1
                     R -> 0

-- ==================================================
-- Operations
-- ==================================================

insert :: Ord a => a -> RbTree a -> RbTree a
insert a = force_black . go 
 where
  force_black (R l v r) = B l v r
  force_black t         = t
  go E = R E a E
  go t@(T c l a' r) 
    | a < a'    = balance $ T c (go l) a' r
    | a > a'    = balance $ T c l      a' (go r)
    | otherwise = t
  
```

![[Pasted image 20241115195901.png]]