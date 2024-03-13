#program-analysis #compiler 

A _dataflow analysis_ computes some dataflow information at each _program point_ in the _control flow graph_.

Let $\sigma$ denotes the dataflow information which maps variables to some value domain.
$$
 \sigma : Var \to L 
$$
The elements of $L$ are referred as _abstract values_. You can think $L$ to be some label or true value in some logic. 

>[!note]
>the dataflow information $\sigma$ is an abstracted version of program state $E$, see [[Operational Semantics]]

>[!def] flow function
> The core of any program analysis is how individual instructions in the program are analysed and affect the _analysis state_ $\sigma$ at each _program point_. 
>
> We define this using _flow functions_ that map the _dataflow information_ $\sigma_{pre}$ at the program point immediately before an instruction to the _dataflow information_ $\sigma_{post}$ after that instruction.
>
> A _flow function_ should represent the semantics of the instruction, but abstractly, in terms of the abstract values ($L$) tracked by the analysis.
> $$ f: Instr \to (Var \to L) \to (Var \to L) $$
> or in haskell
> ```haskell
> f :: Instr -> (Var -> L) -> Var -> L
> ```


# Branching 

In CFG when multiple edges "meets" at the same node, make the node have multiple parents, the dataflow information $\sigma_{pre}$ from different branches may not match with each other.

To solve this conflict, we require $L$ to be a [[Lattice]], or more accurate a _join semi-lattice_.

In other words, _join (least upper-bound)_ exists for each pair of elements in $L$.
$$
\forall x, y\in L.\; (x \sqcup y) := \inf\{e\in L: x\le e, y\le e\}
$$
use the join of $\sigma_{post}$ computed from each parent of the node as the result.

>[!def] top & bottom
> usually there is an element in $L$ represents that we are not certain about the abstract value of the variable which is dubbed _top_ and denoted by $\top$, the name _top_ is from [[Lattice]]
>
> we use $\bot$ to denote the dataflow information of a program point than has not been analysed. This is critical when there is a incoming branch that is originated from a node that we have not met yet, but its value is needed for analysing the current node. 

# Loop

use fix-point in dataflow for loops, basically, we run the dataflow analysis in the loop until post dataflow information reaches a fix-point, i.e., do not change any  reaches a fix-point, i.e., do not change any more.

```haskell
flow :: i -> Trie Var a -> Trie Var a
flow (Loop xs) = 
  fix $ \rec pre -> 
    let post = trans pre
    in  if post == pre 
          then post
           else rec post 
 where 
  trans pre = foldr pre flow (reverse xs)
  
```

we first use a map to represent the dataflow information $\sigma$ (we can make a [[Trie#Generalised Trie|generalised trie]] from a function, and convert it back to a function in Haskell).

# Theory

## Termination

>[!thm] dataflow analysis terminination
> If a dataflow lattice has _finite height_ and the flow function is _monotonic_, the _work list algorithm_ will terminate.

>[!def] monotonic
> a function $f$ on a lattice $(L, \sqsubseteq)$ is _monotonic_ if 
> $$ x \sqsubseteq y \implies f(x) \sqsubseteq f(y) $$

### Widening 

The termination theorem requires the lattice to have finite height.

One trick to deal with the problem of _infinite ascending chain_ is to _trade termination with [[#Precision|precision]]_, which consists of 3 step:
1. whether we are in a possible infinite ascending chain? or shall we run the infinite ascending chain detect algorithm
2. the infinite ascending chain detect algorithm
3. terminate the infinite ascending chain with some upper-bound in the lattice

The trick gets its name because upper abstract values usually represents a wider range of concrete values.

## Correctness

Intuitively, we would like the program analysis results to correctly describe every actual execution of the program.


>[!def] soundness
>The result $\langle \sigma_n | n \in P \rangle$ of a program analysis running on program $P$ is sound iff, for all traces $T$ of $P$ , for all $i$ such that $0\le i < length(T)$ we have $\alpha(c_i) \sqsubseteq \sigma_{n_i}$

>[!thm] correctness
>If a dataflow analysis’s flow function $f$ is _monotonic_ and _locally sound_, and for all traces $T$ we have $\alpha(c_0) \sqsubseteq \sigma_0$ where $\sigma_0$ is the initial analysis information, then any _fixed point_ $\{\sigma_n | n \in P\}$  of the analysis is sound.

>[!def] program trace
> A _trace_ is a formalisation of an execution of the program.
>
> A _trace_ $T$ of a program $P$ is a potentially infinite sequence ($\{c_0, c_1, \dots\}$) of _program configuration_ (see [[Operational Semantics]]) where
> - $c_0$ represents the entry point of the program
> - $P \vdash c_i \leadsto c_{i+1}$

>[!def] local soundness
> a flow function $f$ is _locally sound_ iff
> 
> Given $P \vdash c_i \leadsto c_{i+1}$ and $\alpha(c_i) \sqsubseteq \sigma_i$ then 
> $$\alpha(c_{i+1}) \sqsubseteq \sigma_{i+1} = f_i(\sigma_{i})$$

>[!def] fixed point
> Let 
> - $|P|$ denotes the number of program points of program $P$;
> - $f_i$ be the flow function at program point $i$;
> - $\sigma_i$ be the dataflow information before instruction $i$
>
> Then a dataflow analysis result $\sigma = \langle \sigma_1, \dots, \sigma_{|P|} \rangle$ is a fixed point if it is a fixed point of function $F$ 
> $$ F(\sigma)_i = \bigsqcup_{j\in \mathrm{pred}(i)} f_j(\sigma_j)$$

## Precision

let $\Gamma(j)$ denotes all _concrete configurations_ that can be observed at _instruction_ $j$.

$$
  \Gamma(j) = \big\{ c \;|\; c = \langle E, j\rangle, \;c\in \mathrm{Traces}(P) \big\}
$$

>[!def] optimal precision
>A dataflow analysis result $\langle \sigma_n | n \in P \rangle$ of program $P$ is _optimally precise_ iff 
>$$ \sigma_i = \bigsqcup_{c\in \Gamma(i)} \alpha(c) $$
> In other words, its the _join_ of all possible _abstract values_ that can be observed at the _program point_ $i$.
>
> It is not possible to get the result, since it requires to solve the _reachability problem_ of CFG paths.


>[!def] MOP approximation 
> MOP stands for _meet(join) over all paths_, the optimal joins over all _reachable paths_, thus is approximation is an over-approximation.
> 
> MOP is computable when the _flow functions_ are _distributive_

>[!thm] LFP approximation 
> $$ OPT \sqsubseteq MOP \sqsubseteq LFP $$
>Furthemore, if _flow functions_ are _distributive_ then the _least fixed point_ analysis result is exactly _MOP_ 

>[!def] distributive
> a function $f$ on a lattice $(L, \sqsubseteq)$ is _distributive_ if 
> $$ f(x \sqcup y) = f(x) \sqcup f(y) $$

>[!def] least fixed point
> Given a lattice $(L, \sqsubseteq)$, a _least fixed point_ of function $f: L \to L$ is the _meet_ of all fixed point of $f$  
> $$ \sqcap \{x: f(x) = x\}  $$

# Work list algorithm

#algorithm 

```
worklist = []
for Node n in cfg 
    input[n] = output[n] = _|_ 
    add n to worklist 
input[0] = initialDataflowInformation 

while worklist is not empty 
    take a Node n off the worklist 
    output[n] = flow(n, input[n]) 
    for Node j in succs(n) 
        newInput = input[j] `union` output[n] 
        if newInput != input[j] 
            input[j] = newInput 
            add j to worklist
```


```haskell
class Eq a => Lattice a where
  top :: a
  join :: a -> a -> a
  lessThan :: Partial => a -> a -> Bool
  lessThanRel :: [(a, a)]


-- | data flow analysis with 
-- instruction set i
-- variable's value type t
-- lattice a
class Lattice a => DataFlow i a t where
  -- | abstraction function
  abstract :: t -> a
  initAssumption :: a
  initAssumption = top
  flow :: i -> (Var t -> a) -> Var t -> a
  flow i = untrie . flowTrie i . trie
  flowTrie :: i -> Trie (Var t) a -> Trie (Var t) a
  flowTrie i = trie . flow i . untrie
```


# Interprocedural Analysis

>[!remark] inter- & intra-
>the prefix "inter-" means _between_ or _among_ groups while "intra-" means _within_ or _inside_

## function call instrucions

In _interprocedural analysis_ we treat function calls as a single instruction, and we need inference rules for these "function instructions".

There is some other problems we needs to consider:
- precondition of the function
- postcondition of the function
- side-effects of the function
- recursive function calls

>[!tip] idea
>we can treat a function as a single program and do dataflow analysis, suppose:
>- we have a initial dataflow info $\sigma_0 = L_a$ 
>- the analysis gives a info after the return instruction satisfies $\sigma_{return} \sqsubseteq L_r$
>- the flow function $f$ is _monotonic_
>
> Then we have $\forall \sigma_0 \sqsubseteq L_a$
> $$  \sigma_{return} = f(\sigma_0) \sqsubseteq f(L_a) = \sigma_{return} \sqsubseteq L_r  $$
> The problem becomes how to choose $L_a$?

Side-effect of mutating global variables can be handled by treating these global variable as both input and output of the function.

## Context Sensitive Analysis

> **Context-sensitive analysis** analyses a function either multiple times, or parametrically, so that _the analysis results returned to different call sites reflect the different analysis results passed in at those call sites_.


>[!remark]
>This is much like _function inlining_, we inline the function's local CFG to the whole program CFG. However, inlining do **not** support _recursive functions_
