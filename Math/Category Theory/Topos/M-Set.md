#category-theory 
#topos-theory 

# Definition

Given a [[Monoid]] $M$, a $M$-set is a pair $(X, \lambda)$ where $X$ is a set and
$$ \lambda : M\times X \to X $$
satisfies
$$
\begin{align}
  \lambda_m\circ\lambda_n &= \lambda_{m * n} \\
  \lambda_e &= \mathtt{id}_X
\end{align}
$$

The $M$-$Set$ is the category of $M$-sets
- (objects): $(X,\lambda)$
- (arrows): $f : (X, \lambda) \to (Y, \mu)$ such that the diagram commutes 
      $$
      \begin{CD}
        X @>f>> Y \\
        @V\lambda_m VV @VV\mu_m V \\
        X @>>f> Y
      \end{CD}
  $$
    is called an **equivariant** or **action preserving** function.

# Topos Structure


## Terminal Object

the terminal object is the same as in $Set$ with $\lambda$ defined as 
$$
  \lambda(m,*) = *
$$


## Pullback

$$
\begin{CD}
  (A \mathop{\times}_C B, \lambda\times\mu)  @>i>> (A, \lambda) \\
  @VjVV         @VVfV \\
  (B, \mu) @>>g> (C, \gamma)
\end{CD}
$$
the [[Pullback and Pushout|pullback]] of $f$ and $g$ is the same as in $Set$ category


## Subobject classifier

The [[Subobject#Classifying subobject|subobject classifier]] of the $M$-$Set$ category is a pair $\Omega = (L_M, \omega)$ where
1. $L_M$ is the set of all $M$'s left [[Ideal]]
2. $$\omega_m(L) = \bigg \{n\in M \;:\; n*m \in L \bigg\} $$
3. the arrow $true$ map the only point in $1$ to the largest ideal: 
      $$ true(*) = M $$

Given a subobject $f: A \to B$,
$$
\begin{CD}
(A, \lambda) @>f>> (B,\mu) \\
@V!VV @VV\chi_fV \\
1 @>>\top> \Omega
\end{CD}
$$
Suppose $A \subset B$, $f$ is an embedding indicates $\mu=\lambda$ on $X$, 
$$
  \chi_f(b) = \{ m: \mu_m(b)\in X \}
$$


## Exponential

Notice $(M,*)$ is also a M-set.
Let $(E, \sigma)$ is an exponential
$$
  (E, \sigma) = (Y, \mu)^{(X,\lambda)}
$$
$E$ is the set of **equivariant arrows** such that
$$ f: (M,*)\times (X, \lambda) \to (Y,\mu) $$
and 
$$
  \sigma_m(f)(n, x) = f(m*n, x) 
$$

