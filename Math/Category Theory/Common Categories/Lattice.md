A Lattice or Order is a [[Math/Category Theory/Common Categories/Partial Order]] satisfies:
1. (**meet**) every pair of objects $(X, Y)$ has an unique infimum/meet/greatest-lower-bound $X\land Y := X \times Y$, i.e. _product_.
2. (**join**) every pair of objects $(X, Y)$ has an unique supreme/join/least-upper-bound  $X\lor Y := X + Y$, i.e. _coproduct_.


###### Bounded Lattice
A bounded lattice is a lattice that additionally has 
1. A **greatest element**, also called maximum, or top element, and denoted by $1$, or by $\top$;
2. And a **least element**, also called minimum, or bottom, denoted by $0$ or by $\bot$.

$$
\forall X\in \mathbf{Obj}(\mathscr C), 0\le X\le1
$$

Easy to see that $\top$ is the terminal object in the category and $\bot$ is the initial object.


