#data-structure 

前缀树
![[Pasted image 20210828132131.png]]

可以用来替代hash实现map/set
- 空间可能更小
- 动态查询


# Generalised Trie

>[!question] Can Trie support types other than String? 
> _Trie_ can seen as an implementation of abstract data type `Map String v`, with `String` as the type of keys.
>
> Can we have a similar implementation of `Map k v` for some other key type `k`?
>
> The answer is true if `k` is some _algebraic data type_.


```haskell

class HasTrie k where
  data Trie k :: * -> *
  lookup :: k -> Trie k v -> Maybe v

instance HasTrie String where
  data Trie String v = 
    TrieStr 
      (Maybe v)     
      -- ^ corresponds to the data contructor `[]`
      (Map Char (Trie String v))
      -- ^ corresponds to the data contructor `:`
  lookup [] (TrieStr nil _) = nil
  lookup (x:xs) (TrieStr _ cons) = 
    do 
      trie <- Map.lookup x cons
      lookup xs trie
```

the string Trie gives a clue of generalising to ADT:
1. sum types are integrated using product
2. product types are integrated using `Map`

```haskell
instance HasTrie () where
  data Trie () v = TrieUnit (Maybe v)
  lookup _ trie = coerce trie 

instance (HasTrie a, HasTrie b) => HasTrie (Either a b) where
  data Trie (Either a b) v = TrieSum (Trie a v) (Trie b v)
  lookup (Left a) (TrieSum trie _) = lookup a trie
  lookup (Right b) (TrieSum _ trie) = lookup b trie


instance (HasTrie a, HasTrie b) => HasTrie (a, b) where
  data Trie (a, b) v 
    = TrieProd (Trie a (Trie b v))
  lookup (a, b) (TrieProd inner) = do
    bTrie <- lookup a inner
    lookup b bTrie

-- if the first can does not have trie

newtype HasNoTrie a = HasNoTrie a

instance {-# OVERLAPPING #-} HasTrie b => HasTrie (HasNoTrie a, b) where
  data Trie (HasNoTrie a, b) v 
    = TrieProd' (HasNoTrie a -> Maybe (Trie b v))
  lookup (a, b) (TrieProd' f) = do 
    trie <- f a
    lookup b trie
```

Thus we can use [[Generic Programming in Haskell#GHC Generics|GHC.Generics]] to derive `HasTrie` for _monomorphic data types_ automatically.  

Theory behind:

$$
\begin{align}
\mathbf{1} \to a &\simeq a \\
(a + b) \to c &\simeq (a\to c) \times (b \to c) \\
(a \times b) \to c &\simeq a \to (b \to c)
\end{align}
$$

# Funcional Memo Trie

#memoization #functional-programming 


http://conal.net/blog/tag/memoization
## Basic idea

```haskell
class HasTrie a where
  data (~>) a :: * -> *
  trie   :: (a -> b) -> (a ~> b)
  untrie :: (a ~> b) -> (a -> b)

infixr 9 ~>
```
with rules:
1. `trie . untrie = id`
2. `untrie . trie = id`
where `id` only means identical _semantically_.

Since Haskell is a lazy language we can write the memoize function as 

```haskell
memo :: (a -> b) -> (a -> b)
memo = untrie . trie
```

## Generics implementation

```haskell
import GHC.Generics

instance HasTrie (U1 p) where
  data (U1 p) ~> v = UnitTrie v
  trie f = UnitTrie $ f U1
  untrie (UnitTrie v) = const v

instance (HasTrie (f p), HasTrie (g p)) => HasTrie ((f :+: g) p) where 
  data (f :+: g) p ~> v = SumTrie (f p ~> v) (g p -> v)
  trie f = SumTrie (trie $ f . L1) (trie $ f . R1)
  untrie (SumTrie ltrie rtrie) = \case 
    L1 f -> untrie ltrie f
    R1 g -> untrie rtrie g

instance (HasTrie (f p), HasTrie (g p)) => HasTrie ((f :*: g) p) where
  data (f :*: g) p ~> = ProdTrie (f p ~> (g p ~> v)) 
  trie func = ProdTrie . trie $ \f -> trie $ \g -> func (f :*: g)
  untrie (ProdTrie pTrie) = \f g -> untrie (untrie pTrie f) g
```