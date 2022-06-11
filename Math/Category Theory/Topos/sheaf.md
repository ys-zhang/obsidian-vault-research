#category-theory 
#topology 
#topos-theory 

![[sheaf.png]]


# Spatial topos

The category $\mathbf{Top}(I)$ is a [[Topos Theory|topos]] which contains **sheaves over topology space** $I$. 

- A **sheaf**  over $I$ is a [[Bundle.excalidraw|bundle]] $(A, f)$ with some additional topological structure.  
    $$ f : A \to I $$is _continues_ and _local homeomorphic_ (局部同胚). 
- __Arrows__ among sheaves are _continues maps_ makes the following diagram commute: $$ k: (A, f) \to (B, g) $$
          ![[comm-triangle.excalidraw]]


### Terminal Object

the _terminal object_ of $\mathbf{Top}(I)$ is $(I, id_I)$
$$id_I: I\to I$$
for any _sheaf_ $(A, f)$ the arrow $!: f \to 1$ is exactly the morphism $f$ since $f = id_I \circ f$ 


### Subobject classifier

TLDR;
The _subobject classifier_  $(\Omega, p)$ is a **sheaf of germs of open sets**.
where 
1. $\Omega$ is a topology space 
2. $p: \Omega \to I$ is a local homeomorphism.


#### Points

$\Omega$ as a point set is points of $I$ attached with local topology structure:
$$
\begin{align}
  \Omega &= \bigcup_{i\in I} \big \{  (i, [U]_i): [U]_i \text{ is the germ of } U\in \mathscr{O}(I) \text{ at } i \big \} 
\end{align}
$$


#### Germ

_germs_ at point $i$ defines how the topology space $I$ "looks like" locally at $i$.

First define an equivalence relation of open sets  $\sim_i$ at $i$.
For all $U, V \in \mathscr O(I)$,  $U \sim_i V$ if and only if
$$
  \exists W\in \mathscr O(I), \text{ such that }  
  i\in W \text{ and } W\cap U = W \cap V
$$
note that it is not necessary to have $W\cap U \ne \emptyset$, furthermore,
1. all open sets contains $i$ form the germ $[I]_i$ at $i$
2. all open sets does not contain $i$ form the germ $[\emptyset]_i$ at $i$
3. other germs has $i$ on the boundary


#### Topology

the open sets of $\Omega$ are derive from $\mathscr O(I)$ and of the form
$$
  [U, V] = \{(i, [U]_i): i\in V\}
$$


#### Characteristic morphism

- we need to construct the [[Pullback and Pushout#Pullback|pullback]]:
    $$
    \begin{CD}
    A @>f>> B \\
    @V!VV  @VV\chi_fV \\
    1 @>>true> \Omega
    \end{CD}
  $$
- the $true$ arrow $$ i \overset{true}{\longrightarrow} (i, [I]_i) $$
- the character $\chi_f$, $\forall x\in B$, choose a neighbourhood $S$ of $x$ on which the sheaf $q: B \to I$ is a homeomorphism on $S$, then $$ \chi_f(x) = \langle q(x), [q(A\cap S)]_{q(x)} \rangle $$
# Reference

[sheaf on a topological space](https://ncatlab.org/nlab/show/sheaf+on+a+topological+space)
