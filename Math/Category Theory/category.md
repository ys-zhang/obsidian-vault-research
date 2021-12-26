A _Category_ is a collection of _objects_ with _morphisms_ (arrows) between objects.

Let $C$ be a category,
- $Obj(C)$ denotes the collection of objects;
- $\forall X, Y \in Obj(C)$, $Hom_C(X, Y)$ denotes the _set_ of morphism from $X \to Y$.
    - (_identity arrow_) $\forall X \in Obj(C)$, $\exists \; id_X \in Hom_C(X, X)$, s.t. $\forall Y \in Obj(C)$, $\forall g\in Hom_C(X, Y), h \in Hom_C(Y, X$), we have $$g \circ id_X = g \; \textrm{and}\; id_X \circ h = h$$
    - (_composition_) $\forall X, Y, X \in Obj(C), f \in Hom_C(X, Y), g \in Hom_C(Y, X)$,  we have $$g\circ f \in Hom_C(X, Z)$$
    - (_associativity_) $$(f\circ g) \circ h = f \circ (g \circ h)$$ 
```ad-note
1. _Morphism_ always forms a set while _objects_ does not;
2. If _objects_ forms a _set_, then the category is said to be _small_;
3. Internal structure of objects are "forgotten", i.e. _objects_ are atoms of a _category_.
```

