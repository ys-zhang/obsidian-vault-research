A Lattice or Order is a [[Partial Order]] satisfies:
1. (**meet**) every pair of objects $(X, Y)$ has an unique infimum/meet/greatest-lower-bound $X\sqcap Y := X \times Y$, i.e. _product_.
2. (**join**) every pair of objects $(X, Y)$ has an unique supreme/join/least-upper-bound  $X\sqcup Y := X + Y$, i.e. _coproduct_.


# Bounded Lattice

A bounded lattice is a lattice that additionally has 
1. A **greatest element**, also called maximum, or top element, and denoted by $1$, or by $\top$;
2. And a **least element**, also called minimum, or bottom, denoted by $0$ or by $\bot$.

$$
\forall X\in \mathbf{Obj}(\mathscr C), 0\le X\le1
$$

Easy to see that $\top$ is the terminal object in the category and $\bot$ is the initial object.

# Lattice height

>[!def] ascending chain
> An _ascending chain_ is a list $\sigma$ (can be infinite) that 
> $$ \forall i\le j, \sigma_i \sqsubseteq \sigma_j $$
> A _finite ascending chain_ has _finite height_ $h$ if it contains $h+1$ _distinct_ elements

>[!def] lattice height
>A _lattice_ has finite height $h$ if there is an ascending chain with height $h$ and no ascending chains with height greater than $h$

# functions on lattice


>[!def] monotonic
> a function $f$ on a lattice $(L, \sqsubseteq)$ is _monotonic_ if 
> $$ x \sqsubseteq y \implies f(x) \sqsubseteq f(y) $$



>[!def] distributive
> a function $f$ on a lattice $(L, \sqsubseteq)$ is _distributive_ if 
> $$ f(x \sqcup y) = f(x) \sqcup f(y) $$

