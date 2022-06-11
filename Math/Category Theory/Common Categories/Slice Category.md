#category-theory 

Given a Category $\mathscr C$ and an object $A$ in the category the slice category $\mathscr C/A$ is defined as 

1. the objects in $\mathscr C/A$ are arrows in $\mathscr C$ that points to $A$, i.e., **morphisms with object $A$ as their codomain**.
    $$   \mathbf{Obj}(\mathscr C/A) = \bigcup_{X\in \mathscr C}\mathbf{Hom}_{\mathscr C}(X, A) $$
2. the arrows in $\mathscr C/A$   are pull-backs, i.e., let $f: X\to A$, $\forall h: Y\to X$,  and $g= Y\to A$  are objects in $\mathscr C$, $$ \mathbf{Hom}_{\mathscr C/A}(f, g) = \{h \;|\; f = g\circ h \} \subset \mathbf{Hom}_{\mathscr C}(X, Y) $$

# Connection with dependent type theory

the objects in the slice category are very alike type families indexed by $A$ in [[Dependent Type Theory]]. 

For example suppose we have an arrow $f: X \to A$, its inverse maps each term $a:A$ to a subset $f^{-1}(a)\subset X$, thus we have a type family 
$$f^{-1}: A \to Set$$
and 
$$ f: \big ( \sum_{a:A} f^{-1}(a) \big ) \to A $$

