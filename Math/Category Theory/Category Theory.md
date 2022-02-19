A _Category_ is a collection of _objects_ with _morphisms_ (arrows) between objects.

Let $\mathscr C$ be a category,
- $Obj(\mathscr C)$ denotes the collection of objects;
- $\forall X, Y \in Obj(\mathscr C)$, $Hom_{\mathscr C}(X, Y)$ denotes the _set_ of morphism from $X \to Y$.
    - (_identity arrow_) $\forall X \in Obj(\mathscr C)$, $\exists \; id_X \in Hom_{\mathscr C}(X, X)$, s.t. $\forall Y \in Obj(\mathscr C)$, $\forall g\in Hom_{\mathscr C}(X, Y), h \in Hom_{\mathscr C}(Y, X)$, we have $$g \circ id_X = g \; \textrm{and}\; id_X \circ h = h$$
    - (_composition_) $\forall X, Y, X \in Obj(\mathscr C), f \in Hom_{\mathscr C}(X, Y), g \in Hom_{\mathscr C}(Y, X)$,  we have $$g\circ f \in Hom_{\mathscr C}(X, Z)$$
    - (_associativity_) $$(f\circ g) \circ h = f \circ (g \circ h)$$ 
```ad-note
1. _Morphism_ always forms a set while _objects_ does not;
2. If _objects_ forms a _set_, then the category is said to be _small_;
3. Internal structure of objects are "forgotten", i.e. _objects_ are atoms of a _category_.
```

# Isomorphism

For $X, Y \in Obj(\mathscr C)$, $f\in Hom_{\mathscr C}(X, Y)$ is an _isomorphism_ if $\exists g \in Hom_{\mathscr C}(Y, X)$, if
$$
\begin{align}
g \circ f = id_X \\
f \circ g = id_Y
\end{align}
$$

# Objects

the _initial object_ and the _terminal object_
- For all objects in the category, _initial object_ have one unique arrow to it. _Initial object_ is unique. This object is $\emptyset$  or `Void` in [[The Hask Category]];
- For all objects in the category, there is one unique arrow from it to the _terminal object_. _Terminal object_ is unique up to isomorphism. This object is `Unit` in [[The Hask Category]].

# Opposite Category

The _opposite category_ of $\mathscr C$ , denoted by $\mathscr{C}^{op}$, has the same objects as $\mathscr C$  , but reversed arrows, i.e., 
$$ 
\begin{align}
Hom_{\mathscr{C}^{op}}(X, Y) &= Hom_{\mathscr C}(Y, X) \\
g^{op} \circ f^{op} &:= (f \circ g)^{op} 
\end{align}
$$

# Products of Category

## Cartesian product
Cartesian product can be described by _projections_. A function $p$  is a projection, if and only if, 
$$ p \circ p = p $$
and $p_1, p_2$ forms a Cartesian project if $p_1$ and $p_2$ are projections and 
$$
 p_1 \circ p_2 = p_2 \circ p_1 = (\forall x \to e) 
$$

## Categorical product
The _product_ of 2 objects $P, Q$ in category $\mathscr C$ is an object $C$ with a pair of morphism $p: C \to P$ and $q: C \to Q$ ; and, $\forall (Z', p': Z' \to X, q': Z' \to Y)$ , $\exists! m: Z' \to Z$, such that 
$p' = p \circ m$ and $q' = q \circ m$.

![[Product (category).excalidraw]]


```ad-note
It's like the _product_ $C$ is the _closest_ object to $P$ and $Q$ that can maps to both $P$, and $Q$. In other words, it neither too big nor too small.
```

## Coproduct (categorical sum)

![[Coproduct (category).excalidraw]]

```ad-note
- Coproduct is product in the opposite category.
- Coproduct is $Set$ is disjoint union (taged union) of two sets.
```

## Product of category

let $\mathscr C$ and $\mathscr D$  be two small category. the product category, with the object set be cartesian product of  the 2 object sets, and morphism set be the cartesian product of the 2 morphism sets.

## ADT (algebraic data type)

1. **Product** and **Coproduct** is _commutative_ and _associative_ (up to isomorphism).
2. **Product** with `Void` is a [[monoid]].
3. **Coproduct** with `Unit` is [[monoid]].
4. **Product** and **Coproduct** together form a __Rig__ (_ring with out negative, or semi-ring_)

- `Bool = Unit + Unit`
- `Maybe a = Unit + a`
- parameterised types are solution of equation, `l(a) = 1 + a * l(a)` result in `List a`. 

# Functor

![[Functor.excalidraw]]

**Functor** keeps internal structure (_morphism_) of _category_.
1. (id): $F(id_X) = id_{FX}$;
2. (_covariant_): $F: \mathbf{Hom}(X, Y) \to \mathbf{Hom}(FX, FY)$;
3. (commute): $F(g \circ f) = F(g) \circ F(f)$;

An **endofunctor** is a functor that the image category lies in the source category.

A **Monoidal Category** is a category in which there exists a **endo-bifunctor**.

```ad-note
A functor “_embeds_” one category in another. It may collapse multiple things into one, but it never breaks connections. 
One way of thinking about it is that with a functor we are _modelling one category inside another_. The source category serves as a model, a blueprint, for some structure that’s part of the target category.
```

```ad-note
1. each pair of related object in source category is maped to a related object in image category.
2. it may squash objects or arrows, i.e., it may not be injective or surjective.
3. A **faithfull functor** is _injective_ on _hom-sets_.
4. A **full functor** is _surjective_ on _hom-sets_.
5. `Functor`s in [[Haskell]] are parameterised types.
```

see [Haskell package `Data.Functor`](https://hackage.haskell.org/package/base-4.16.0.0/docs/Data-Functor.html)

## Contravariant functor
$$
 F^{op}: \mathscr C \to \mathscr D
$$
is a **Contravariant functor** if it is a functor from $\mathscr C^{op}$  to $\mathscr D$. i.e. _contravariant_:
$$
F^{op}: \mathbf{Hom}^{op}(X, Y) = \mathbf{Hom}(Y, X) \to \mathbf{Hom}(FX, FY) 
$$
and
$$
F(g \circ f) = F(f) \circ F(g)
$$
or a functor from $\mathscr C$ to $\mathscr D$ naturally introduce a cofunctor from $\mathscr C^{op}$ to $\mathscr D$.

```ad-note
**functors** are sead to be _covariant_, while **Contravariant functor** are _contravariant_.
```

```haskell
class Contravariant f where
  contramap :: (b -> a) -> f a -> f b
```



## Bifunctor

A **bifunctor** is a functor maps a category product to a category, in [[Haskell]] it is a type constructor with 2 type parameters. Both product and coproduct can be seen as bifunctor. The codomain of the bifunctor is more "coarse" than product of categories.

$$
f: \mathscr{ C \times D \to E} 
$$

## Profunctor

A **Profunctor** is like a bifunctor; however, it is _contravariant_ in the first category while _covariant_ in the second category:

$$
f: \mathscr{C^{op} \times C \to C}
$$


## Examples

_Constant Functor_: a functor maps all objects to a constant object $c$, denoted by $\Delta_c$

_Identity Functor_ maps a category to itself.

$Cat$ is the category of categories, where objects are categories and morphisms are functors.

$Hom: \mathscr C^{op} \times \mathscr C \to Set$, is a _profunctor_. 
$\forall C \in \mathscr C$, $Hom(C, \cdot)$ is a functor which is referred as _hom-functor_.
 
```haskell
class Functor f where
  -- this `f a` hints the compiler that 
  --   f is a type constructor
  fmap :: (a -> b) -> f a -> f b   

class Bifunctor b where
  bimap :: (a -> a') -> (b -> b') -> (f a b -> f a' b')
```


# Natural Transformation

![[Natural transformation.excalidraw]]

[Natural transformation is free](https://bartoszmilewski.com/2014/09/22/parametricity-money-for-nothing-and-theorems-for-free/) in [[Haskell]] due to [[parametric polymorphism]], which is different from [[ad-hoc polymorphism]].


## Composition

[natural transformation (nlab)](http://nlab-pages.s3.us-east-2.amazonaws.com/nlab/show/natural+transformation#composition)

### Compose with functors
A natural transformation can be thought as a function:
$$
\alpha: \forall c \in \mathscr C,\; c \to (F\;c \to G\;c) 
$$

1. To make $\alpha \circ T$  a natural transformation we need $T$ to map objects to objects, _functors restrict to $\mathbf{Obj}(C)$_;
2. To make $T \circ \alpha$ a natural transformation we need $T$ to map morphisms to morphisms, _functors restrict to $\mathbf{Hom}(C)$_. 

$$
\begin{align}
(\alpha \circ H)_x = \alpha_{Hx} \\
(H\circ \alpha)_x = H(\alpha_x)
\end{align}
$$

![[Natural transformation composition.excalidraw]]


### Horizontal Composition

[horizontal composition (nlab)](http://nlab-pages.s3.us-east-2.amazonaws.com/nlab/show/horizontal+composition)

$$
\begin{align}
\mathscr C \overset{F}{\to} \mathscr D \overset{G}{\to} \mathscr E \\
\mathscr C \overset{F'}{\to} \mathscr D \overset{G'}{\to} \mathscr E \\
\alpha : F \to F' \\
\beta: G \to G' \\
\end{align}
$$
and be need a composition $\beta \circ \alpha$:
$$
\beta \circ \alpha : G \circ F \to G'\circ F'

$$

![[Pasted image 20220217142941.png]]

the composition is defined as 
$$
(\beta \circ \alpha)_a 
  = \beta_{F'a} \circ G(\alpha_a)
  = G'(\alpha_a) \circ \beta_{Fa}
$$

![[Pasted image 20220217142922.png]]

### Vertical Composition

$$
\begin{align}
F: \mathscr C \to \mathscr D\\
G: \mathscr C \to \mathscr D\\
H: \mathscr C \to \mathscr D \\
\alpha : F \to G\\
\beta : G \to H 
\end{align}
$$
$$
(\beta \circ \alpha)_a = \beta_a \circ \alpha_a
$$


## Cone

![[Cone (category).excalidraw]]

TLDR;
A _Cone_ is a natural transformation that maps the _apex functor_ to the _diagram functor_:
1. the _apex functor_ maps the category to the apex of the cone;
2. the _diagram functor_ maps the category to the bottom of the cone;
3. the arrows of the natural transformation is arms of the cones
4. Naturality requires that all wall triangles commute. 

 
```ad-note
title: Cone Construction

In general, to build a **cone**:
1. we start with a category $\mathscr I$ that defines the pattern. It’s a small, often finite category.
2. We pick a _functor_ $$D: \mathscr I \to \mathscr C$$ and call it (or its image) a _diagram_. 
3. We pick some $c \in \mathscr C$ as the _apex_ of our _cone_. We use it to define the _constant functor_ $$\Delta_c: \mathscr I \to \mathscr C$$
4. A _natural transformation_ from $\Delta_c$ to $D$ is then our _cone_. 
```

![[Pasted image 20220101233831.png]]

For a finite $\mathscr I$ it’s just a bunch of morphisms connecting $c$ to the _diagram_: the image of $\mathscr I$ under $D$.


## Limit & Colimit

### Through Universal construction

Follow the construction of cones, with the source category $\mathscr I$ and the destination category $\mathscr C$ fixed, we can define the _category of cones_.

- we have the diagram or base of the cone which is a functor $D$.
- objects are cones i.e. a functor $\alpha_c: \Delta_c \to D$ where $c \in \mathscr C$ is the apex of the cone, and $\Delta_c$ is the constant functor.
- morphisms are arrows in $\mathscr C$ that connect cone apexes and commute with cone edges.

The _terminal object_ of this cone category is the limit of the diagram, i.e., functor $D$. 

```ad-note
The intuition is that the **limit** _embodies the properties of the whole diagram in a single object_. And being universal means that it has no extraneous junk.

Given any cone, there is a _unique_ morphism that is "compatible" with the diagram. We have a mapping of cones to special morphisms, and it’s a one-to-one mapping.
```

```ad-example
1. The _terminal object_ is a limit generated by an empty category
2. _Equalizer_ is a limit generated a two-element category with two parallel morphisms going between them.
3. A _pullback_ is the limit of _Cospan_, which is the _diagram_ of 
    
    `1 -> 2 <- 3`
```

### Construction through natural formalisation

#### Problem
The problem here is how to construct the unique factorisation arrow, i.e. for each cone apex $c$ pickup the arrow in $\mathbf{Hom}_{\mathscr C}(c, \mathbf{Lim}D)$ that satisfies the commute condition.

#### Theorem
The limit exists iff exists a natural isomorphism
$$
\begin{align}

\mathbf{Hom}_{\mathscr C}(-, \mathbf{Lim}D) &\simeq \mathbf{Nat}(\Delta_-, D) \\

\forall c\in \mathscr C, \mathbf{Hom}_{\mathscr C}(c, \mathbf{Lim}D) &\simeq \mathbf{Nat}(\Delta_c, D) 

\end{align}
$$
where 
1. $\mathbf{C}_{\mathscr C}(c, \lim D) \subset \mathbf{Hom}_{\mathscr C}(c, \lim D)$ is the _unique factorisation arrow_ from cones(with $c$ as its apex) to the limit.
2. $\mathbf{Nat}(\Delta_c, D)$ is the collection of cones with $c$ as its apex

In other words, _cones with the same apex is isomorphic to arrow from the apex to the limit_.

![[Limit (category)]]

To construct the $limit$, for each $c\in \mathcal D$ we have a set of cones with apex $c$, we need to find the _unique_ morphism from $c$ to the $limit$.

In other words, finding a morphism from $Hom_{\mathscr D}(c, \lim D)$, which can be formalised as a natural transformation:
$$
\begin{align}
  F, G &: \mathscr C \to \mathbf{Set} \\
  F(c) &= \mathbf{Nat}(\Delta_c, D) \\
  G(c) &= \mathbf{Hom}_{\mathscr C}(c, \mathbf{Lim}D) \\
  \alpha &: F \to G \\
  \alpha_c &: \mathbf{Nat}(c, D) \to \mathbf{Hom}_{\mathscr C}(c, \mathbf{Lim}D)
\end{align}
$$



#### Proof

##### The Natural Transformation 

We define two factors from $\mathscr C$ to $\mathbf{Set}$ (the category of set):

_The first one_ is another contravariant functor that maps an object to a set of cones (natural transformations). 
$$
F(c) = \mathbf{Nat}(\Delta_c, D )
$$

Let $f: c\to c'$,  we need $Ff: \mathbf{Nat}(\Delta_{c'}, D ) \to \mathbf{Nat}(\Delta_c, D )$.

$\forall \alpha' \in \mathbf{Nat}(\Delta_{c'}, D )$

$$
((Ff)\;\alpha')_x :=  \alpha'_x\circ f
$$
It is easy to verify that $(Ff)\; \alpha'$ is natural.

_The second one_ is a contravariant functor which maps an object to a hom-set:
$$
G(c) = \mathbf{Hom}_{\mathscr C}(c, \mathbf{Lim}D)
$$

Let $f: c\to c'$, then $Gf: \mathbf{Hom}_{\mathscr C}(c', \mathbf{Lim}D) \to \mathbf{Hom}_{\mathscr C}(c, \mathbf{Lim}D)$ is defined as follows:

$\forall m \in \mathbf{Hom}_{\mathscr C}(c', \mathbf{Lim}D)$
$$
(Gf)\;m := m \circ f
$$

$F, G$ are [[Presheaf]] (contravariant functors maps to $\mathbf{Set}$) 
1. $F$ maps $c$ to all cones with $c$ as apex
2. $G$ is the [[Presheaf#Representable Presheaf |representable presheaf]] of $\mathbf{Lim}D$ maps $c$ to the hom-set.


##### The proof

Let 
$$
\begin{align}
\alpha: F \to G \\
\beta: G \to F
\end{align}
$$
First, we prove if $\mathbf{Lim} D$ is the real limit of $D$, then $\alpha_c$  maps a cone to its factorisation arrow. $\beta_c$ maps each arrow $f: c\to \mathbf{Lim} D$ to the pull back of cone $\mathbf{Lim}D$.

Then, if $\alpha, \beta$ exists, it easy to verify that $\mathbf{Lim} D$ is the real limit of $D$ up to isomorphism.



# Function type (exponential object)

![[Function (category).excalidraw]]


# Kleisli Category

![[Kleisli category]]

The _Kleisli Category_ have same objects as the original category, with arrows defined as "embellished" arrows in the original category and a new composition definition, which usually denoted by the _fish operator_ `>=>`.

## Example

```haskell
type M a = (a, String)

compose :: (b -> M c) -> (a -> M b) -> a -> M c
compose g f a = (c, s1 ++ s2)
  where (b, s1) = f a
        (c, s2) = g b
```

```fsharp
type M<'a> = 'a * string   // tuple type in fsharp

let compose (g: 'b -> M<'c>) (f: 'a -> M<'b>) (a: 'a) : M<'c>
  = let (b, s1) = f a
    let (c, s2) = g b
    (c, s1 + s2)
```

```scala

```

```rust
fn compose<X, Y, Z>(g: impl Fn(Y) -> (Z, String), f: impl Fn(X) -> (Y, String)) -> impl Fn(X) -> (Z, String) {
    move |x| {
        let (y, s1) = f(x);
        let (z, s2) = g(y);
        (z, s1 + s2)
    }
}
```