#category-theory 

 The _Yoneda lemma_ tells us that 
 1. All _$\mathbf{Set}$-valued functors_ can be obtained from _hom-functors_ through natural transformations, and it explicitly enumerates all such transformations.
 2. A natural transformation between a _hom-functor_ and any other functor $F$ is completely determined by specifying the value of its single component at just one point.

There is a one-to-one correspondence between natural transformations from $\mathbf{Hom}_{\mathscr C}(a, -)$ to $F$ and elements of $F a$. 
$$
\mathbf{Nat}(\mathbf{Hom}_{\mathscr C}(a, -), F) \cong Fa
$$
Let $[\mathscr C, \mathbf{Set}]$ be the _category of functors_ from $\mathscr C$ to $\mathbf{Set}$. And denote 
$$
 \mathbf C(a, b) := \mathbf{Hom}_{\mathscr C}(a, b)
$$
Since arrows between functors are natural morphisms, the _Yoneda lemma_ become
$$
[\mathbf C, \mathbf{Set}](\mathbf C(a, -), F) \cong Fa
$$

# Proof

![[Pasted image 20220214105248.png]]
![[Pasted image 20220214110900.png]]
$\forall y \in \mathbf C, f\in \mathbf C(a, y)$
$$
 \alpha_y(f)= \alpha_y(f \circ id_a) = (Ff) (\alpha_a (id_a)) 
$$

1. Given an element $\alpha_a (id_a)\in Fa$, the equation defines the natural transform $\alpha_y$ 
2. Given a natural transform $\alpha$,  the object $\alpha_a (id_a)$ is the element in $Fa$.

# Yoneda Embedding

the map 
$$
  \mathbf{C}(-,=): a \to \mathbf{C}(a, -)
$$
maps $\mathbf C$ to the category of functors $[\mathbf C, \mathbf{Set}]$.

The map is a _contravariant functor_. for each arrow $f: a \to b$, $f\in C(a, b)$ , according to Yoneda Lemma, 
$$
[\mathbf C, \mathbf{Set}](\mathbf C(b, -), \mathbf C(a, -)) \cong \mathbf C(a, b)
$$
moreover its a _fully faithful_ functor, i.e., it is isomorphic on each hom-set.

This _fully faithful contravariant functor_ is called the **Yoneda Embedding**.

 # Haskell

```haskell
-- The Hom functor
type Reader a x = a -> x
instance Functor (Reader a) where
  fmap :: (x -> y) -> Reader a x -> Reader a y
  fmap f rx = f . rx 

-- a natural transform is a polimorphic function
alpha :: (Functor f) => forall x . (a -> x) -> f x
-- the Yoneda Lemma says each Natural transform identifies an
-- element in F a
phi :: (Functor f) => (forall x. (a -> x) -> f x) -> f a
phi alpha = alpha id
-- and each element in F a generates a Natural transform
psi :: (Functor f) => (f a) -> (a -> x) -> f x
psi fa h = (fmap h) fa
```

