A _binary tree_  stores information about _intervals_ (segments).

support:

1. a segment tree for a set $\mathcal I$ of $N$ _intervals_, build time is $O(N\log N)$, with storage $O(N\log N)$.
2. query _intervals_ that contains a given point, time complexity $O(k + \log N)$, where $k$ is the number of result intervals.

# Concepts

Let $\mathcal I$ be a set of _intervals_ with _endpoints_ 
$$
  -\infty < p_1 < p_2 < \dots < p_n < +\infty
$$

1. _elementary partition_
    $$
    (-\infty, p_1), [p_1, p_1], ( p_1, p_2), [p_2,p_2], \dots, (p_{n-1}, p_n), [p_n,p_n], (p_n, +\infty)
  $$
 2. _elementary interval_ 
     - $[p_i, p_i]$
     - $(p_i,p_{i+1})$


**Segment tree** $T$ satisfies:

1. $T$ is a _binary_ tree;
2. _Leaves_ of $T$ are _elementary intervals_ and are **ordered**.
3. _Internal nodes_ are _union of the leaves_ of the subtree rooted at it. 
4. Each node stores intervals that in the original interval set $\mathcal I$ such that it covers the node but cannot cover the node's parent.


# Code

```haskell
type SegTree = SegTreeNode

data SegTreeNode = 
  SegTreeNode {
    left     :: Option<SegTreeNode>;  -- left child
    right    :: Option<SegTreeNode>;  -- right child
    interval :: Interval;
    -- all intervals in the original set of intervals
    -- that covers the node but does not covers it's parant
    covers :: [Interval]      
  }

data Interval = Interval EndPoint EndPoint
data EndPoint = EndPoint { val::Float, incl::Bool }


buildSegTree :: [Interval] -> SegTree
buildSegTree ints = 
  let 
    elemInts = buildElemInts ints -- endpoints are sorted
    balanceTree = buildBalancedTree elemInts
  in 
    fold ( \(tr, int) -> decorate tr int ) 
      balancedTree ints

decorate :: SegTreeNode -> Interval -> SegTree
decorate node int
  if covers int nodeInt  then 
    insertToCovers int node
  else case intersect of 
    (True, True) ->  setRight (setLeft node (decorate left int)) 
                              ((decorate right int))
    (True, False) -> setLeft node (decorate left int)
    (False, True) -> setRight node (decorate right int)
  where 
    nodeInt     = interval node
    left        = left node
    right       = right node
    nodeIntLeft = interval left  
    nodeIntLeft = interval right 
    intersect   = (interset int nodeIntLeft, interset int nodeIntRight)

query :: SegTree -> Float -> [Interval]
query tree x 
  if isLeaf tree then 
    if contains tree x then [(interval tree)] else []
  else
    case (xInLeft, xInRight) of 
      (False, False) -> []
      (True, False)  -> ( covers tree ) ++ (query (left tree) x)
      (False, True)  -> ( covers tree ) ++ (query (right tree) x)
      _              -> panic!
  where
    xInLeft  = ..
    xInRight = ..
```