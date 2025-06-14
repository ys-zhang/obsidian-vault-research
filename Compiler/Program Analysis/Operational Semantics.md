#compiler #formal-method #program-analysis

To reason about analysis correctness, we need a clear definition of _what a program means_. Usually, it contains 2 kind of meanings:
1. what's the value of this piece of program
2. what's the side-effect of this piece of program

>[!def] program state
> _program state_ is a function 
> $$ E: \text{Variable} \to \text{Value} $$
> basically the "current value" of variables



There are two broad classes of _operational semantics_: 
1. _big-step operational semantics_, which specifies the entire operation of a given expression or statement; 
2. and _small-step operational semantics_, which specifies the operation of the program one step at a time


# Big-step semantics

>[!def] big step
> There are 2 settings of big-step semantics
>1. Given _program state_ $E$, the _expression_ $a$ evaluates to _value_ $n$
>2. Given _program state_ $E$, the _statement_ $S$ evaluates to a _program state_ $E'$
> $$
> \begin{align}
>  \langle E, a\rangle &\Downarrow n \\
>  \langle E, S\rangle &\Downarrow E' \\
> \end{align}
> $$

# Small step semantics

> Whereas _big step semantics_ specifies program meaning as a function between a _configuration_ and a _new state_, _small step models_ it as a step _from one configuration to another_.


>[!def] configuration
> A _configuration_ is a pair of a _statement_ and a _program state_ denoted by $\langle E, S\rangle$. 
>
> To be more specific, $E$ represents the program state before the execution of $S$, and $S$ is the next statement to be executed.

  >[!def] small step
  > $$ 
  > \begin{align}
  >  \to \;\; &: \; (E \times Stmt) \to (E \times Stmt) \\
  >  \to_{e} \; &: \; (E \times Expr) \to Expr
  > \end{align}
  > $$

>[!note] small step is smaller than big step
>In small step, an expression do not needs to evaluate to a value in one step, how in big step, it must be reduced to a value.


# Misc

>[!thm]
> $$
> \forall a \in Expr ,\; \langle E, a \rangle \to^*_a n \iff \langle E, a \rangle \Downarrow n
> $$

>[!note]
> Note that this works because the semantics rules for expressions are strictly syntax-directed: _the meaning of an expression is determined entirely by the meaning of its subexpressions_, the structure of which guides the induction.


>[!def] deterministic
> $$ \langle E, a\rangle \Downarrow n \; \land \; \langle E, a\rangle \Downarrow n' \implies n = n'  $$
> $$ \langle E, S\rangle \Downarrow E' \; \land \; \langle E, S\rangle \Downarrow E'' \implies E' = E''  $$

>[!tip] inductive proofs
> sometimes a proposition $\forall a, P(a)$ cannot be proved by induction on $A$, we can rewrite the proposition to 
>
> $$\forall a, \forall q, q \land (q \implies P(a))$$
> then we can induct on both $a$ and $Q$, in other words, the inductive step becomes, "for all $a'<a$ and $q'<q$ such that $P(a')$ holds because of $q'$, we have $P(a)$ holds because of some $q''\le q$." 


# References 

1. Hutton, G. (2023). Programming language semantics: It’s easy as 1, 2, 3. _Journal of Functional Programming_, _33_, e9.
2. Nielson, F., Nielson, H. R., & Hankin, C. (1999). _Principles of Program Analysis_. Springer. [https://doi.org/10.1007/978-3-662-03811-6](https://doi.org/10.1007/978-3-662-03811-6)