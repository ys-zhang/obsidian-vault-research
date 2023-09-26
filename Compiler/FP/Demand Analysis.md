#Haskell #GHC #academic-paper 

paper: Theory and Practice of Demand Analysis in Haskell

# The Problem

>[!TLDR]
> Haskell uses Call By Need, which is inefficient as arguments are passed as heap-allocated thunks/suspension.
> 
> We can use call by value if the argument is 100% demanded.

>[!Definition] Strictness
> We say function `f` is strict in argument `x`, if demanding `f`'s output will 100% demanding `x` 
> 
> mathematically, suppose $f: X\to Y$, $f$ is strict in $X_p$ iff existing $x_1, \dots, x_{p-1}, x_{p+1}\dots x_n$ such that 
> $f(\cdot | x_{1:p-1}, x_{p+1:n})$ is not constant on $X_p \cup \{\bot\}$


## 2 Optimisations

1. _Call by value_: When the called function will definitely evaluate its argument, the caller can evaluate the argument early and pass the value itself instead of a thunk.
2. _Unboxing argument_: When the called function 100% only demands a subset of an argument's structure, the caller can only pass the subset instead of creating the whole.

The benefit of the 2nd optimisation maybe a little obscure.
1. it can avoid thunks;
2. it can improve the 1st optimisation.

Unless [`-XStrictData`](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/strict.html#strict-by-default-data-types) Language extension is on, by default GHC will create a thunk for each field in a compound data structure. 

After unboxing, there are chances that the 1st optimisation become applicable.

In the case `lenFst :: ([a], b) -> Int`, before _unboxing_, 1 thunk is allocated for each field, however, after unboxing, `b` no longer be passed to the function, and the thunk for `a` is eliminated by _call-by-value_ 


# Worker-Wrapper Transformation

The worker-wrapper transformation splits each function `f` into a _wrapper_, with the ordinary calling convention, and a _worker_, with a specialised calling convention. 

The _wrapper_ serves as an impedance-matcher to the _worker_; it simply calls the worker using the specialised calling convention.

>[!NOTE]
> The _wrapper_ often is very short and inlined at call site.

```haskell

f :: (Int, Int) -> Int
f p = <rhs> -- strict in fst, do not use snd

-- wrapper inlined at call site
f p = case p of (a, b) -> wf a
wf a = let p = (a, error "Urk")
       in  <rhs>
```

# Backward Analysis

2 sort of information is needed:
1. evaluation demands or _strictness_. The compiler uses strictness information to replace call-by-need with call-by-value.
2. usage-demand or _absence_. The compiler uses usage-demand information to decide which fragments of the argument to pass to the specialised version of the function.

> _strictness_ is about which part will be 100% demanded, while _absence_ is about which part will 100% **not** be demanded.


Backwards analysis answers the following question

> If an expression $e$ is consumed by a demand $d$, what demand is placed on $e$’s free variables?


## Demand 

A _demand_ expresses the _degree_ to which a value is evaluated: 

A tuple can be
- `(A)` not be evaluated at all
- `(S)` evaluated to head normal form
- `(S, A)` a tuple evaluate its first comp to HNF, snd absent 
- `(A, S)`
- `(S, S)`

A function can be 
- `(S)` HNF
- `(C(S))` applied and its result evaluated 


> Seen in this light, an expression $e$ is a _demand transformer_, that transforms a demand on $e$ into demands on $e$’s _free variables_.

For example the expression `(fst x+y)` transforms the demand `S` on the result into a demand `(S,A)` on `x`, and `S` on `y`.

Similarly a function
```haskell
f x y = <rhs>
```
is a demand transformer that transforms a demand on the result of a call `(C)` to $f$ to demands `(S)` on the arguments of that call.

# Projection Model

[[CPO (Complete Partial Order)#Projection|Projection]] model demands in the following way: a _demand_ that evaluates certain parts of a data structure (and not others) can be modelled by a _projection_ (recall projections are also _continuous_) that does not touch the evaluated parts of the data structure, but smashes the un-evaluated parts to $\bot$.


>[!Example] 
> the _projection model_ for _demand_ placed by function `fst` is a projection
> $$ p = \lambda(a, b).(a, \bot) $$
> and obviously `fst = fst . p`

>[!Idea] Projection-based Analysis
> Given a _projection_ $p$ and an element $d\in D$, the job is to infer the _least possible projection_ $q$ such that 
> $$ p(d) = p(q(d)) $$
> which means if element $d$ is used in the _context_ of $p$ (i.e., only the result of application of $p$ to $d$ is considered), then the projection $q$ describes how much information can be “removed” from $d$ so the result would remain the same.

>[!Theorem] Context Strengthening
> $p_1$ and $p_2$ are both projections and $p_1 \sqsubseteq p_2$ then 
> $$
>   p_2(d) = p_2(q(d)) \implies p_1(d) = p_1(q(d))
> $$

## Projection Operators

1. Defined the _product_ of projections $\times : Pr(D_\infty) \times Pr(D_\infty) \to Pr(D_\infty)$
    $$
      (p\times q)(d) = \begin{cases}
        (p(d_1), q(d_2))  & \text{if}\; d = (d_1, d_2) \in D_\infty \times D_\infty \\ \\
        \bot & \text{otherwise} 
      \end{cases}
    $$
2. High order projection
    $$
        (p \to q)(f) = \begin{cases}
            q \circ f \circ p   & \text{if}\; f \in [D_\infty, D_\infty] \\ \\
        \bot & \text{otherwise}
        \end{cases}
    $$

