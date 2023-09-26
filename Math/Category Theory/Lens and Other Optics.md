#category-theory #Haskell 

# Library

- [Lens in Haskell](https://hackage.haskell.org/package/lens)
- [lens-rs](https://crates.io/crates/lens-rs)

## A Talk From Haskell Community

<iframe width="100%" style="aspect-ratio: 560 / 315" src="https://www.youtube.com/embed/cefnmjtAolY" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>


## Diagram

![[Pasted image 20221223154402.png]]


# Optics as Modular Data Accessors

## Problem

It is not obvious how to combine data accessors, in such a way that data accessors for a compound data structure are composed out of smaller data accessors for the parts of that structure. Data accessors are traditionally not first-class citizens, combinable in their own right.

## Lens & Prism as Interface: a bad representation that cannot compose

![[Screenshot 2022-12-23 at 10.32.09 am.png]]

```haskell
data Lens a b s t = Lens {
    view   :: s → a,        -- getter
    update :: b × s → t     -- setter
  }

data Prism a b s t = Prism {
    match :: s → t + a,     -- downcast
    build :: b → t          -- constructor
  }
```

> Prism is the dual of Lens, in the sense of reversing all arrows. 
>   (`build` is the dual of `view`, and `match` is the dual of `update`)

>[!Problem]
> `update` and `match`  cannot compose naturally

## Adaptor / Iso

![[Pasted image 20230511185646.png]]

```haskell
data Adaptor a b s t = Adaptor {
  from :: s -> a 
  to   :: b -> t
}
```

> Although adapters look like rather trivial data accessors, they are very useful as ‘plumbing’ combinators, converting between representations.

## Traversal

> Traversal can be seen as a **generalisation** of lenses and of prisms, providing access not just to a single component within a whole structure but onto an entire sequence of such components.

```haskell
-- A Traversal is much like a Traversable see 
--   https://en.wikibooks.org/wiki/Haskell/Traversable
class Traversal a b s t where
  traverse :: (Applicative f, Functor f) => (a -> f b) -> s -> f t

class TraversalIntuition a b = TraversalIntuition {
    toListOf :: a -> [b],
    over     :: (b -> b) -> (a -> a)
  }
```

The intuitions here are
1. see `s` and `t` as container types which contains type `a` and `b` respectively;
2. see `f` as a class of effects where
    - `f a` as the type of "_computation that may have effects of type `f` and will yield a result of `a`_"
    - `a -> f b` as "_effectful function from `a` to `b`, having effects modelled by `f`_"
3. For an **applicative functor** `f`, a **traversal** takes an effectful operation of type `a -> f b` on the elements of a container, and lifts this to an effectful computation of type `s -> f t` over the whole container, applying the operation to each element in turn.

> [!Composable]
> Traversal is naturally composable


### An example of a traversal
```haskell

inc :: a -> State Integer a 
inc a = State { run = \count -> (a, count+1) }

countOdd :: Integer -> State Integer Bool
countOdd num = if even num then pure False else inc True
countOddInTree :: Tree Integer -> State Integer (Tree Bool)
countOddInTree = traverse countOdd

-- type definitions

data Tree a = Node (Tree a) a (Tree a) | Empty

instance Traversal a b (Tree a) (Tree b) where
  traverse :: (Applicative f, Functor f) => (a -> f b) -> Tree a -> f (Tree b)
  traverse m Empty = 
    pure Empty 
  -- inorder traverse of the tree
  traverse m (Node left val right) =  
    -- <*> is left associative
    (pure Node) <*> (traverse m left) <*> (m val) <*> (traverse m right) 

-- state is a stateful computation yield `a` with effect changes `s`
data State s a = State { run :: s -> a × s }
instance Functor (State s) where 
  fmap :: (a -> b) -> State s a -> State s b
  fmap f m = State { run = \s -> let (a, next_s) = run m s in (f a, next_s) }
instance Applicative (State s) where 
  pure :: a -> State s a
  pure a = State { run = \s -> (a, s) } -- keep state untouched
  (<*>) :: State s (a -> b) -> State s a -> State s b
  mf <*> n = State { 
      run = \s -> let (f, s1) = run mf s, 
                      (a, s2) = run n s
                  in
                      (f a, s2) 
    }
```

### Connection with a Container

>[!THEOREM] 
>Fix the type `s` for each  suppose `s` satisfies `Traversal a b s t`, 
>then for each applicative functor `f`, the `traverse` function implies an isomorphism 
>  $$
>  s \cong_{f} \exists n \in \mathbb N, \; (a^n \times (b^n \to t) )
> $$

Proof:
1. The LHS is a sum type which can be represented in Haskell as 
  ```haskell
  data FunList a b t = Done t
                     | More a (FunList a b (b → t)) -- (a, (a^{n-1}, b^{n-1}) -> (b -> t)) 
  ```
  Just need to notice 
  $$
  a^n \times b^n \to t \cong  a \times (a^{n-1}\times (b^{n-1} \to (b \to t)))
  $$
  this isomorphism and representation can be proven by induction on $n$.
2. RHS is isomorphic (implied by applicative function `f`) to `FunList a b t`
    - Notice `FunList a b` is a [[Category Theory#Functor|functor]] similar to $\mathbf{Hom}(A^n\times B^n, -)$
    - `FunList a b` is also an `Applicative` similar to $\mathbf{Hom}(A^n\times B^n, -)$

```haskell
single :: a  -> FunList a b b 
single a = More a (Done id)

-- recall, the type of taverse and 
-- `FunList a b` is an Applicative Functor
traverse :: a -> (FunList a b) b -> s -> (FunList a b) t 
-- then we have the first direction of the isomorphism
extract :: s -> FunList a b t
extract = traverse single

fuse :: FunList b b t -> t
fuse (Done t)   = t
fuse (More b l) = (fuse l) b
```

> [!NOTE]
> the `extract` function converts a Traversal `s` to something with type $A^n \times (B^n \to T)$ which
> is the _equivalent to the product of a getter and a setter_.

```haskell
-- a concrete Traversal
data Traversal a b s t = Traversal {extract :: s → FunList a b t}
```

![[Screenshot 2022-12-23 at 5.48.09 pm.png]]


# Optics as map between profunctors


### Profunctor Brush-up 


![[Category Theory#Profunctor]]


for all functor `f`  the type `a -> f b` is a profunctor.

```haskell
data UpStar f a b = UpStar {unUpStar :: a → f b}
instance Functor f ⇒ Profunctor (UpStar f) where 
  dimap f g (UpStar h) = UpStar(fmap g · h · f )
```


## Optics

```haskell
type Optic p a b s t = p a b -> p s t
```

Informally, when $S$ is a composite type with some component of type $A$, and $T$ similarly a composite type in which that component has type $B$, and $P$ is some notion of transformer, then we can think of a data accessor of type `Optic p a s b t` as lifting a component transformer of type `p a b` to a whole-structure transformer of type `p s t`.

We will retrieve equivalents of our original definitions of _lens_, _prism_, and so on by placing various constraints on $P$, starting with requiring $P$ to be a [[#Profunctor Brush-up|profunctor]]. 

Crucially, different varieties of optic all now have the same form—in particular, they are all simply functions—and so they will compose straightforwardly; they may involve different constraints on P, but those constraints simply conjoin.


### Adapter

```haskell

data Adapter a b s t = Adapter {
  from :: s -> a,
  to   :: b -> t 
}

-- is a morphism satisfies forall profunctor p it can be viewed
-- as an optic
type AdpterP a b s t = (forall . Profunctor p) => Optics p a b s t;
```


### Lens

```haskell
-- Profunctor representation
type LensP a b s t = (forall p . Cartesian p) => Optic p a b s t

-- recall
data Lens a b s t = Lens {
  view :: s -> a
  update :: b × s -> t
}

class Profunctor p => Cartesian p where 
  first  :: p a b -> p (a × c) (b × c)
  second :: p a b -> p (c × a) (c × b)
  -- (unit rule):          dimap runit runit′ h = first h 
  -- (associativity rule): dimap assoc assoc′ (first (first h)) = first h
  -- 
  -- where:
  --   runit  :: (a, ()) -> a 
  --   runit' :: a -> (a, ())
  --   assoc  :: (a, (b, c)) -> ((a, b), c)
  --   assoc' :: ((a, b), c) -> (a, (b, c))
  

instance Profunctor (Lens a b) where 
  dimap :: (s' -> s) -> (t -> t') -> (Lens a b s t) -> (Lens a b s' t')
  dimap f g (Lens v u) = Lens (v . f) (g . u . (cross id f))

instance Cartesian (Lens a b) where
  -- fork f g x = (f x, g x)
  first  (Lens v u) = Lens (v . fst) (fork (u . cross id fst) (snd . snd)) 
  second (Lens v u) = Lens (v . snd) (fork (fst . snd) (u . cross id snd))
```

  
```tikz
\usepackage{tikz-cd}

\begin{document}

\begin{tikzcd}[sep=huge] 
  {p\;a\;b} &&& {p\;s\;t} \\ \\ \\ 
  {p\;(a\times s)\;(b\times s)} &&& {p\;(a\times s)\; t}
  \arrow["{first}"', from=1-1, to=4-1]
  \arrow["{Optic\;a\;b\;s\;t}", Rightarrow, from=1-1, to=1-4] 
  \arrow["{dimap\; id\; update}"', from=4-1, to=4-4] 
  \arrow["{dimap\;(view \times id)\;id}"', from=4-4, to=1-4] 
  \arrow["dimap"{marking}, from=4-1, to=1-4]
\end{tikzcd}
  
\end{document}
```


```haskell

viewP :: LensP a b s t 
      -- ^    forall p . Cartesian p => p a b -> p s t
      -- ^ or forall f . Functor f   => (a -> f b) -> (s -> f t)       
      -> s -> a
viewP lens = let getter = lens $ \a -> Const { getConst = a } 
             -- ^ getter :: s -> Const a t
             in getConst . getter

updateP :: LensP a b s t
        -> s -> b -> t
updateP lens = let setter = lens $ counst id
               -- ^ setter :: s -> (b -> t)
               in setter
```


### Prism

```haskell
-- Profunctor representation
type PrismP a b s t = (forall p . Cocartesian p) => Optic p a b s t

data Prism a b s t {
  match :: s -> t + a
  build :: b -> t
}

class (Profunctor p) => Cocartesian p where 
  left  :: p a b -> p (a+c) (b+c)
  right :: p a b -> p (c+a) (c+b)
  -- (unit rule):          dimap rzero rzero′ h = left h 
  -- (associativity rule): dimap coassoc coassoc′ (left (left h)) = left h
  -- 
  -- where:
  --   rzero    :: a + 0 -> a 
  --   rzero'   :: a -> a + 0
  --   coassoc  :: a + (b + c) -> (a + b) + c
  --   coassoc' :: (a + b) + c -> a + (b + c)

instance Profunctor (Prism a b) where 
  dimap :: (s' -> s) -> (t -> t') -> (Prism a b s t) -> (Prism a b s' t')
  dimap f g (Prism m b) = Prism (plus (m . f) id) (g . b)

instance Cocartesian (Prism a b) where 
  left  (Prism m b) = Prism (either (plus Left id . m) (Left . Right)) (Left . b) 
  right (Prism m b) = Prism (either (Left . Left) (plus Right id . m)) (Right . b)
```


```tikz
\usepackage{tikz-cd}
\begin{document}
\begin{tikzcd}[sep=huge] 
  {p\;a\;b} &&& {p\;s\;t} \\ \\ \\ 
  {p\;(a + t)\;(b + t)} &&& {p\;(a + t)\; t}
  \arrow["{left}"', from=1-1, to=4-1]
  \arrow["{Optic\;a\;b\;s\;t}", Rightarrow, from=1-1, to=1-4] 
  \arrow["{dimap\; id\; (build + id)}"', from=4-1, to=4-4] 
  \arrow["{dimap\;match\;id}"', from=4-4, to=1-4] 
  \arrow["dimap"{marking}, from=4-1, to=1-4]
\end{tikzcd}
\end{document}
```




### Traversal

```haskell
-- profunctor representation
type TraversalP a b s t = forall p . (Cartesian p, Cocartesian p, Monoidal p) => Optic p a b s t



```
