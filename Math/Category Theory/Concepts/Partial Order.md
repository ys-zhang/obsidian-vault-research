#set-theory #formal-method #order-theory 

A binary relation $\sqsubseteq$ on a set $S$ is called a _partial order_ if it is 
- _reflexive_: $\forall x\in S, \; x \sqsubseteq x$;
- _transitive_: $\forall x, y, z\in S,\; x\sqsubseteq y \land y \sqsubseteq z \implies x \sqsubseteq z$;
- _antisymmetric_: $\forall x,y\in S,\; x\sqsubseteq y \land y \sqsubseteq x \implies x = y$.

A _partial order_ $\sqsubseteq$ is a _total order_ if $\forall x,y\in S$,either $x\sqsubseteq y$ or $y\sqsubseteq x$. 

A pair of elements $x,y\in S$ are called _comparable_ if either $x\sqsubseteq y$ or $y\sqsubseteq x$, _incomparable_ otherwise.

# Discrete Partial Order

A _discrete partial order_ is a poset in which no two distinct elements of $S$ are $\sqsubseteq$-comparable.


