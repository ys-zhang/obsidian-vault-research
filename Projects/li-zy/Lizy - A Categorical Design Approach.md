# Modelling a Langauge

## Requirements

- `AstNode` can have _properties_
- `AstNode` can have _children_
- `AstNode` can have _references_
- `AstNode` can query its _parent_ and the corresponding _link_
- `AstNode` can query its _children_ by _link_
- `AstNode` can query its _references_ by _link_
- `AstNode` can query its _referents_ and corresponding _links_


## Language as a Category

> We define a language is a set of `AstNode` types with relationship called link, which include child link and reference link

To simplify the problem, we only consider _child link_.

```tikz 
\usepackage{tikz-cd}
\begin{document} 
  \begin{tikzcd}[row sep=large] 
    & {r:R} \\ 
    & {} \\ 
    {n_1:N_1} && {m_1:M_1} 
    \arrow["{r_a: R_a}"', from=1-2, to=3-1] 
    \arrow["{r_b: R_b}", from=1-2, to=3-3]
  \end{tikzcd} 
\end{document} 
```
1. The tree is rooted as a node $r$ with type $R$.
2. $r: R$ has 2 children 
    1. Branch $r_a$ connects $r$ to  $n_1$ with type $N_1$ ;
    2. Branch $r_b$: connects $r$ to $m_1$ with type $M_1$.

>[!OBSERVATION] 
> - Tree node types can be viewed as **objects**;
> - Ancestor/Descendent relationship just like **morphism**
> 
> A _Language_ can be thought as a **category**.


## The Link Type

Continue on the above example. 
$$
\begin{align}
  r_a &= (r, n_1) \\
  r_b &= (r, m_1)
\end{align}
$$
The link type $R_a$ and $R_b$ is an morphism from $R$ to $N_1$ and $M_1$, respectively, i.e.
$$
\begin{align}
  R_a &: \text{Hom}_{\mathcal L}(R, N_1) \\
  R_b &: \text{Hom}_{\mathcal L}(R, M_1) \\
\end{align}
$$
Or in Haskell

```haskell
data R = R {}     -- node type R
data N1 = N1 {}   -- node type N1
data M1 = M1 {}   -- node type N2
ra :: R -> N1     -- link Ra, since this is a function, use lowercase
rb :: R -> M1     -- link Rb 


class Functor lang => Language lang where
  as_node :: n -> lang n
  

class Language lang => Link lang link where
  dst :: (Language lang a, Language lang b) => link -> lang a -> lang b

```


# Category of Languages


## Language Extension

Suppose we want to extend the language $\mathcal L$ to 

1. add more `AstNode` types
    - allow new types to be able to be associated by _existing links_
    - simply add more structure to the language.
2. extend existing `AstNode` types, e.g, add more _properties_ to a `AstNode` type.

Denote the extension language by $\mathcal L'$.

Intuitively, legal sentences (AST) in the extended language $\mathcal L$ should still be legitimate in the extension language $\mathcal L$ , i.e., link instances should be preserved.

