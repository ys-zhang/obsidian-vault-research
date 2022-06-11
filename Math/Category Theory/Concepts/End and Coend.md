#category-theory #todo 

# Universal Construction

An **end** can be defined using universal construction of a **wedge**.

Recall Wedge:
![[Wedge.excalidraw]]

Given a [[Category Theory#Profunctor|profunctor]] $P$, let $e$ be the end of $P$, which is also a dinatural transformation, then for all wedge $\alpha$ based on $P$, we have the universal property:

$\exists!h\in \mathbf{Hom}(\alpha, e), \; \text{s.t.}\; \forall a \in  \mathbf{Obj}(\mathscr C)$ we have:
$$
 \alpha_a = \pi_a \circ h
$$
where $\pi_a$ is the component of $e$ as dinatural transformation at object $a$.

![[Pasted image 20220603192004.png]]

##  Dependent Type Interpretation

In dependent type theory, the end is characterised by the projection:
$$
  \pi: \Pi_{a\in \text{Type}}P(a,a)
$$
## Haskell


```haskell

typeclass Profunctor p where
  dimap :: a -> b -> (c -> d) -> p b c -> p a d

-- projection / components of dinatural transformation
pi :: Profunctor p => a -> p a a

-- wedge condition
-- suppose f :: a -> b
(dimap id f) . pi = (dimap f id) . pi  -- pi is the equalizer 

```






# Relation with Equalizer

Recall:
![[Equalizer & Coequalizer.excalidraw]]


# Idea of Calculus (Differential & Integral)

Denote **the apex of the end** of a profunctor $P$ as 
$$
\int_{\mathscr C} P(x, x)
$$
and the edges/projection denoted as
$$
\pi_a : \int_{\mathscr C} P(x, x) \to P(a, a)
$$
which is also a component of the end as a dinatural transformation.
