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
- For all objects in the category, _initial object_ have one arrow to it. _Initial object_ is unique. This object is $\emptyset$  or `Void` in [[The Hask Category]];
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

![[product (category).excalidraw]]

```ad-note
It's like the _product_ $C$ is the _closest_ object to $P$ and $Q$ that can maps to both $P$, and $Q$. In other words, it neither too big nor too small.
```


## Coproduct (categorical sum)

![[coproduct (category).excalidraw]]

```ad-note
- Coproduct is product in the opposite category.
- Coproduct is $Set$ is disjoint union (taged union) of two sets.
```
## ADT (algebraic data type)

1. **Product** and **Coproduct** is _commutative_ and _associative_ (up to isomorphism).
2. **Product** with `Void` is a [[monoid]].
3. **Coproduct** with `Unit` is [[monoid]].
4. **Product** and **Coproduct** together form a __Rig__ (_ring with out negative, or semi-ring_)

- `Bool = Unit + Unit`
- `Maybe a = Unit + a`
- parameterised types are solution of equation, `l(a) = 1 + a * l(a)` result in `List a`. 

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