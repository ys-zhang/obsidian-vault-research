# 1 Grothendieck universe & Tarski-Grothendieck Set Theory

## 1.1 Russell-style paradoxes

> Problems of set theory arise by the unjustified recursion of the naive notion of a ‘collection of things.’ If ‘Col’ is one notion of collections (such as ‘Set’ or ‘Class’), then the notion ‘Col of all Cols’ is in general problematic.

it is problematic to refer to the proposition self in the definition of the proposition.

consider the set 
$$ A = \{ x : x \notin x \} $$
then $A\in A \implies A\notin A$.

## 1.2 Pure set

>[!def] pure set
> a pure set is a collection of pure sets.

we define _pure set_ recursively, 
1. the empty collection $\emptyset$ is a pure set.
2. $\star = \{\emptyset\}$ and $2 = \{\star, \emptyset\}$ is pure set, and thus $\mathbb N$ is pure set, also $\mathcal P(\mathbb N)$ is pure set.

## 1.3 Grothendieck universe

>[!def] universe
> A _Grothendieck universe_ is a _pure set_ $U$ such that 
>
> 1. $\forall u\in U, t\in u \implies t \in U$
> 2. $\forall u \in U, \mathcal P(u) \in U$
> 3. $\emptyset \in U$ 
> 4. $\forall I\in U$ and function $f: I\to U$ then $\bigcup_{i \in I}f(i) \in U$


>[!lemma]
> if $u\in U$ and $v \in U$ then $u \cup v \in U$ 

# 2 References
- [Math-overflow: universe in category theory](https://math.stackexchange.com/a/4853261)
- [pure set & structure set theory](https://ncatlab.org/nlab/show/pure+set)
- [Wiki- Self reference](https://en.wikipedia.org/wiki/Self-reference)
