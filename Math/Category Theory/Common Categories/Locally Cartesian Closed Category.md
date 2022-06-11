#category-theory 

A category $\mathscr C$ is **locally Cartesian closed** if and only if 
1. $\mathscr C$ has all finite limits
2. For each object $A$, the [[Slice Category]] $\mathscr C/A$ is Cartesian closed.


Furthermore, we have each arrow in $\mathscr C$ introduces a functor between slice categories.


# Relation with dependent type theory

For each [[Dependent Type Theory|dependent type system]] we can have a one-to-one correspondence to a LCCC,

we construct the the LCCC from one DTS:
1. let the objects of the category be the constant types in the DTS;
2. let the arrows of the category be the type functions (indexed type families) in the DTS, for each type family $$ B: A \to \text{Type}$$, the projection $$ p_A : \sum_{x:A}B(x) \to A $$ is an arrow in $\mathscr C$, and an object in $\mathscr C/A$ 
