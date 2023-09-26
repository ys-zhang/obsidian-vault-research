#order-theory #set-theory #formal-method 

# Chain-Complete Partial Order


A _chain_ of a [[Poset]] $(X, \sqsubseteq)$ is a _subset_ of $X$ that is totally ordered in $\sqsubseteq$.

A [[Poset]] is a _chain-complete_ if **all** _nonempty chains_ have _supreme_.

A chain-complete partially ordered set is called a CPO.

# Continuous 

A function $f: X\to Y$ is _monotone_ if $f$ preserves order:
$$
\forall x,y \in X,\; x\sqsubseteq_X y \implies f(x) \sqsubseteq_Y f(y)
$$

A function $f: X\to Y$ is _continuous_ if for all _nonempty chain_ $C \subset X$, the supremum of $\text{Im}_f(C)\subset Y$ exists and 
$$
  \sqcup_Y f(C) = f(\sqcup_X C)
$$
>[!Theorem]
> all _continuous_ function are _monotone_ 

Let $[X \to Y]$ denote the space of continuous function from $X$ to $Y$.

_Continuous_ functions $[X\to Y]$ is _CPO_ if both $X$ and $Y$ are _CPO_, define the pointwise ordering 
$$
  f \sqsubseteq g   \iff \forall x\in C\; f(x) \sqsubseteq g(x) 
$$
furthermore, if $Y$ is _pointed_ with $\bot_Y$ then $[X \to Y]$ is _pointed_ with the constant function 
$$
  \bot_{[X\to Y]} = \lambda x.\bot_Y
$$
for all nonempty chain $C \subset [X\to Y]$, then 
$$
  \bigsqcup_{[X\to Y]} \{g: g\in C\} = \lambda x.\bigsqcup_Y \{ g(x): g\in C \}
$$

# Projection

A _continuous_ function $p\in [X \to X]$, is a _projection_ on a _CPO_ $X$ if $\forall x \in X,$
$$
\begin{align}
  p(x) &\sqsubseteq x \\
  p(p(x)) &= p(x)  
\end{align}
$$
or equivalently 
$$
\begin{align}
  p &\sqsubseteq \lambda x.x \\
  p \circ p &= p
\end{align}
$$

>[!Theorem]
>If both $p_1$ and $p_2$ are _projections_, then 
>$$ p_1 \sqsubseteq p_2 \implies p_1 \circ p_2 = p_1 $$

>[!Theorem]
>If $P$ is a _set of projections_ then $\sqcup P$ exists and is also a projection.

