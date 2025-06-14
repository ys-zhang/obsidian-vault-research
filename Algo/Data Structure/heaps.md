#data-structure 

# 1 Leftist Heap

>[!definition] leftist property
>For every node, the rank of its left child is greater or equal than the rank of its right child.

>[!definition] rank
> the rank of a binary tree is defined to be the length of its right spine

```haskell
import Data.Maybe (listToMaybe)

data Heap a 
  = E
  | T {-# UNPACK #-} !Int     
      -- ^ the rank cache
      !a !(Heap a) !(Heap a)
  deriving (Show, Eq)

mkT :: a -> Heap a -> Heap a -> Heap a
mkT v a b | rank a >= rank b = T (1 + rank a) v a b
          | otherwise        = T (1 + rank b) v b a

singleton :: a -> Heap a
singleton a = T 1 a E E

prop_heap :: Ord a => Heap a -> Bool
prop_heap E           = True 
prop_heap (T _ a l r) =  maybe True (a <=) (top l) 
                      && maybe True (a <=) (top r) 
                      && prop_heap l 
                      && prop_heap r

prop_leftist :: Heap a -> Bool
prop_leftist = fst . go
 where
  go E           = (True, 0)
  go (T _ _ l r) = let (bl, rl) = go l 
                       (br, rr) = go r
                   in  (bl && br && (rl >= rr), rl + 1)

prop_correct_rank :: Heap a -> Bool
prop_correct_rank E = True 
prop_correct_rank (T rk _ l r) = rk == 1 + rank r 
                                 && prop_correct_rank l
                                 && prop_correct_rank r
                                
-- ====================================================
-- Operations
-- ====================================================

top :: Heap a -> Maybe a
top = listToMaybe . rightSpine

rightSpine :: Heap a -> [a]
rightSpine E           = []
rightSpine (T _ a l r) = a: rightSpine r

rank :: Heap a -> Int
rank E            = 0
rank (T rk _ _ _) = rk
```

The key insight behind leftist heaps is that two heaps can be merged by merging their right spines as you would merge two sorted lists, and then swapping the children of nodes along this path as necessary to restore the leftist property.

```haskell
merge :: Ord a => Heap a -> Heap a -> Heap a
merge E h = h
merge h E = h
merge h1@(T _ x xl, xr) h2@(T _ y _ _) 
  | x <= y    = mkT x xl (merge xr h2) 
  | otherwise = merge h2 h1

insert :: Ord a => a -> Heap a -> Heap a
insert a = merge (singleton a) 

pop :: Heap a -> Maybe (a, Heap a)
pop E           = Nothing
pop (T _ a l r) = Just (a, merge l r)

-- | O(n)
fromList :: Ord a => [a] -> Heap a
fromList = go . map singleton
  go [] = E
  go [x] = singleton x 
  go xs  = go . map merge $ brk xs
  brk = \case []       -> []
              [x]      -> [(x, E)]
              (x:y:xs) -> (x,y) : brk xs
```


# 2 Binomial Heap

Binomial trees^[it is a sprecial rose tree] are inductively defined as follows:
- A binomial tree of rank $0$ is a singleton node.
- A binomial tree of rank $r + 1$ is formed by linking two binomial trees of rank $r$, making one tree the _leftmost child_ of the other.

Equivalently, we have the following definition 

>[!definition] binomial tree
>a binomial tree of rank $r$ contains $2^r$ nodes and is a node with $r$ children $t_1, \dots, t_r$ with each $t_i$ of rank $r-i$

![Binomial tree](data:image/png;base64,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)

```haskell
-- | Binomial Tree
data BinTree a 
  = Node { rank     :: {-# UNPACK #-} !Int 
         , value    :: a 
         , children :: [BinTree a] 
         }
  deriving (Show, Eq)

singleton :: a -> BinTree a
singleton a = Node 0 a []

prop_binomial_tree :: Eq a => BinTree a -> Bool
prop_binomial_tree (Node r a cs) = 
  r == length cs
    && all (map prop_binomial_tree cs) 
    && all (map ((>=a) . value) cs)
    && all (zipWith (\c i -> rank c == r-i) cs [1..])

-- | combine 2 equal ranked binomial tree
link :: Ord a => BinTree a -> BinTree a -> BinTree a
link t1@(Node r1 a1 c1s) t2@(Node r2 a2 c2s)
  | r1 /= r2  = error "link binomial trees of unequal ranks"
  | a1 <= a2  = Node (r1+1) a1 (t2:c1s)
  | otherwise = Node (r2+1) a2 (t1:c2s)
```

>[!definition] binomial heap
> a binomial heap is a collection of heap-ordered binomial trees in which no two trees have the same rank. This collection is represented as a list of trees in increasing order of rank.

```haskell
type BinHeap a = [BinTree a]

prop_binomial_heap :: Eq a => BinHeap a -> Bool
prop_binomial_heap ts = 
  all (map prop_binomial_tree ts)
    && sorted (map rank ts)
 where
  sorted []       = True
  sorted [_]      = True
  sorted (x:y:ys) = x<y && sorted (y:ys)

insertTree :: Ord a => BinTree a -> BinHeap a -> BinHeap a
insertTree t []     = [t]
insertTree t hp@(x:xs) 
  | rank t < rank x = t:hp
  | rank t > rank x = x: insertTree t xs
  | otherwise       = insertTree (link t x) xs
  
-- | O(log n)
insert :: Ord a => a -> BinHeap a -> BinHeap a
insert a = insertTree (singleton a)

merge :: BinHeap a -> BinHeap a -> BinHeap a
merge ts [] = ts
merge [] ts = ts
merge h1@(x:xs) h2@(y:ys) 
  | rank x < rank y = x : merge xs h2
  | rank x > rank y = y : merge h1 ys
  | otherwise       = insertTree (link x y) (merge xs ys)

-- | O(log n)
popMinTree :: Ord a => BinHeap a -> Maybe (BinTree a, BinHeap a)
popMinTree []     = Nothing
popMinTree (t:ts) = case popMinTree ts of 
  Nothing -> Just (t, [])
  Just (t', ts') | value t <= value t' -> Just (t, ts)
                 | otherwise           -> Just (t', t:ts')

findMin :: Ord a => BinHeap a -> Maybe a
findMin = fmap (root . fst) . popMinTree

popMin :: Ord a => BinHeap a -> Maybe (a, BinHeap a)
popMin hp = do (Node _ a cs, ts) <- popMinTree hp
               pure (a, merge (reverse cs) ts)
```




