#category-theory 

含幺半群。

In short, a [[group]] is a monoid where each element has an inversion element, and a monoid is a semi-group with an identity element.

- (_identity element_) $\exists e$, $\forall x \in M$, $$e \cdot x = x\cdot e = x;$$
- (_composition_) $\forall x, y \in M$, $x \cdot y \in M$;
- (_associativity_) $\forall x, y, z \in M$ , $(x \cdot y) \cdot z = x \cdot (y \cdot z)$.




Using [[Dependent Type Theory]], we can express a semi-group by 

$$
  \sum_{A:\text{Type}} \sum_{m: A\to A\to A} \prod_{x,y,z:A} \big [m(x, m(y, z)) =_A m(m(x, y), z) \big]
$$
which, speak in the theory of _predicate logic_, says that the collection of pair $(A, m)$, where $A$ is the set of semi-group elements, and the bi-function "multiply", we have the proof of _associativity_ for all triple of $A$.


See [[Monad#Monoid in Monoidal Category | Monad in Monoidal Category]]
