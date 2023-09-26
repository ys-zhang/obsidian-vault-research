#data-structure #algorithm 

Representation can be seen as a functor from _abstract data type_ to _concrete data type_.

# Data representation

Whenever an _abstraction data type_ $A$ is represented by a _concrete data type_ $R$, two functions must be defined:
$$
\begin{align}
  abs: R \to A \\
  rep: A \to A 
\end{align}
$$

There are two senses in which a concrete object $r$ can represent an abstract object $a$:
1. $r = rep(a)$
2. $abs(r) = a$

>[!Theorem] Well Defined
> $$
>   \forall a\in A,\;\; abs(rep(a)) = a 
> $$

# Arrow representation

to make $rep$ and $abs$ a functor, i.e. representing functions the follow diagram must commute.
$$
\begin{CD}
  A @>f>> A        @. @. A @>f>> A\\
  @VrepVV  @VrepVV  @. @AAabsA @AAabsA\\
  C @>g>> C         @. @. C @>g>> C
\end{CD}
$$
i.e. 
$$
\begin{align}
  g \circ rep &= rep \circ f \\
  abs \circ g &= f\circ abs 
\end{align}
$$

