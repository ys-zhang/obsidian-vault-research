#set-theory  #formal-method #order-theory 

A set with a distinguished [[Partial Order]] defined on it, $(S, \sqsubseteq)$, is called a _partially ordered set_ or _poset_.

# Pointed Poset

Given any poset $(S, \sqsubseteq)$, we can add a new bottom element $\bot$ to get a new poset $(S_\bot, \sqsubseteq_\bot)$.

A partial order is called _pointed_ if it has a distinguished least element $\bot$.

# Bound and Supremum

Given poset $(X, \sqsubseteq)$ and $A \subset X$,

1. $x\in X$ is an _upper bound_ for $A$ if $\forall a\in A,\; a \sqsubseteq x$;
2. $x\in X$ is a _supremum_ of $A$ if $x$ is an upper bound of $A$ and for all $A$'s upper bound $y$, we have $x \sqsubseteq y$

The **supremum** of a subset $A$ is denoted by $\sqcup A$.

>[!WARNING]
>Upper bounds and suprema need not exist.

>[!NOTE]
>If $\emptyset$ has a supreme then it must be $\bot$
