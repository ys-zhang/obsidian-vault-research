# 1 Induction & Coinduction

>[!def] monotone
> A function $F : \mathcal P(\mathcal U) \to \mathcal P(\mathcal U)$ is _monotone_ if 
> $$ X \subseteq Y \implies F(X) \subseteq F(Y) $$

>[!def] generating function
> a _generating function_ is a _monotone_ function 

>[!def] fixed point
>Let $X \in \mathcal P(\mathcal U)$, then
>1. $X$ is _$F-$closed_ if $F(X)\subseteq X$
>2. $X$ is _$F-$consistent_ if $X\subseteq F(X)$
>3. $X$ is a _fixed-point_ of $F$ if $F(X) = X$

A useful intuition for these definitions is to think of the elements of $\mathcal U$ as some sort of _statements_ or _assertions_, and of $F$ as representing a _“justification” relation_ that, given some set of statements (premises), tells us what new statements (conclusions) follow from them. 
- An _F-closed_ set, then, is one that cannot be made any bigger by adding elements justified by $F$—it already contains all the conclusions that are justified by its members. 
- An _F-consistent_ set, on the other hand, is one that is “self-justifying”: every assertion in it is justified by other assertions that are also in it. 
- A _fixed point_ of $F$ is a set that is both closed and consistent: it includes all the justifications required by its members, all the conclusions that follow from its members, and nothing else.


>[!proposition]
> Given a _monotone_ function $F : \mathcal P(\mathcal U) \to \mathcal P(\mathcal U)$,
> 1. The intersection of all _F-closed_ sets is the _least fixed point_ of $F$; 
> 2. The union of all _F-consistent_ sets is the _greatest fixed point_ of $F$. 

>[!def]
>1. denote the least fixed point of $F$ as $\mu F$
>2. denote the greatest fixed point of $F$ as $\nu F$

>[!remark]
> From an intuitive view, $\mu F$ can be find by repeating applying $F$ start from $\mathcal U$, since $F(\mathcal U) \subseteq \mathcal U$:
> $$\mu F = \cdots \circ F \circ F \circ F (\mathcal U)  $$

>[!corollary]
> 1. (_principle of induction_) if $X$ is _F-closed_, then $\mu F \subseteq X$,
> 2. (_principle of coinduction_) if $X$ is _F-consistent_, then $X \subseteq \nu F$,

# 2 Finite and Infinite types

we consider a calculus of only 3 type constructors $\top$,  $\to$, and $\times$, which corresponding to the following _gramma (production rules)_
$$
\begin{align}
  T ::= &\top \\
    | \; &T \times T \\
    | \; &T \to T
\end{align}
$$
the definition of gramma $T$ gives a _generating function_ $F$ where
$$
\begin{align}
  F(X) &= \{\top\} \\ 
       &\cup \{ x \times y \; : x \in X, y \in X \} \\
       &\cup \{ x \to y : x \in X, y \in X \}  
\end{align}
$$

>[!def] 
> 1. denote the least fixed point of $F$ to be $\mathcal T_f$
> 2. denote the greatest fixed point of $F$ to be $\mathcal T$.

## 2.1 Subtyping

>[!def] finite subtyping
> Two _finite_ types $S$ and $T$ are in the subtype relation, i.e. $S$ is a subtype of $T$, if 
> $$ (S, T) \in \mu S_f $$
> where $S_f: \mathcal P (\mathcal T_f \times \mathcal T_f) \to \mathcal P (\mathcal T_f \times \mathcal T_f)$ is defined as 
> $$ 
> \begin{align} 
>   S_f(R) &= \{(a, \top): a \in \mathcal T_f\} \\ &\cup \{ (a \times b,\;\; c\times d) \; : (a, c) \in R, (b,d) \in R \} \\ &\cup \{ (a \to b,\; c \to d) : (c, a) \in R, (b, d) \in R \}
> \end{align}
>  $$
> 
> Note that the definition of $S_f$ matches the rules of subtyping.


>[!def] infinite subtyping
> Two (finite or infinite) types $S$ and $T$ are in the subtype relation if 
> $$(S, T) \in \nu S$$
> where $S: \mathcal P (\mathcal T \times \mathcal T) \to \mathcal P (\mathcal T \times \mathcal T)$ is defined as 
> $$ 
> \begin{align} 
>   S(R) &= \{(a, \top): a \in \mathcal T\} \\ &\cup \{ (a \times b,\; \; c\times d) \; : (a, c) \in R, (b,d) \in R \} \\ &\cup \{ (a \to b,\; c \to d) : (c, a) \in R, (b, d) \in R \}
> \end{align}
>  $$

### 2.1.1 Subtyping is transitive

>[!def] transitive
>A relation $R \subseteq \mathcal{U\times U}$ is transitive if $R$ is _closed_ under the _monotone_ function,
>$$ TR(R) = \{ (x, y) : \exists z\in \mathcal U, (x, z)\in R \land (z, y) \in R \} $$ 


>[!lemma]
> Let $F\in \mathcal P(\mathcal{U\times U}) \to \mathcal P(\mathcal{U\times U})$ be a _monotone_ function. 
> if $\forall R \subseteq \mathcal{U\times U}$, we have 
> $$ TR(F(R)) \subseteq F(TR(R))$$
> then $\nu F$ is transitive.

