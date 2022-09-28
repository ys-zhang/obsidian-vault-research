#model-checking
#logic

Computational Tree Logic (CTL) is based on notion of [[branching time]].

Infinite tree of states represents **behaviour** of system (incl. all possible traces of the system), like state tree in [[Reinforcement Learning]].

-   **state properties**: predicates on state sets
-   **path properties**: predicates on path sets

# Notation and Concepts
-   state formula: CAPITAL Greek letters $\Phi$
-   path formula: small Greek letters $\phi$

## State formula (a predicate on states)

$$ 
	\Phi ::= tt \; |\; a \;| \; \lnot \Phi \;|\; \Phi \lor \Phi \;|\; \Phi \land \Phi \;|\; \exists \phi \;|\; \forall \phi 
$$

-   $tt$ denotes `true`
-   $\exists$ means there exists a path $p$ that $\phi(p)$ holds
-   $\forall$ means for all path $p$ that $\phi(p)$ holds

## Path formula (a temporal predicate on paths)

$$
	\phi ::= X\Phi | \Phi \mathcal{U} \Psi
$$

-   $X$ denotes the next operator and $\mathcal{U}$ is the until operator.
-   $\Phi \mathcal{U} \Psi$ denotes that $\Phi$ holds for all states along the path until $\Psi$ holds.

## Property Specification

-   **eventually** (path formula): $\Diamond\Phi := tt \mathcal{U} \Phi$
-   **always** (state formula): $\Box\Phi := \not \Diamond¬Φ$ not [eventually (not $\Phi$)]



# Inference in CTL

a state $s$ satisfies a state predicate $\Phi$ is denoted by
	$$ s\models\Phi $$ , 
and 
	$$ Sat(\Phi) = \{ s \in S : s \models \Phi \} $$.

1.  Convert the predicate into a **ENF** (existential normal form) and parse the predicate into a parse tree.
2. The leaf of the parse tree are consist of **atomic predicates** and satisfying sets can be computed, then computation is performed bottom up.


# CTL* Logic

CTL* logic can express both [[linear-time temporal logic (LTL)]] and _branching time logic_, by release the CTL constrain of all temporal logic connectivities must be preceded by either $E$ or $A$. 

## State formula & Path formula
1. _State formulas_ have a truth value in a specific state;
    - _Atomic propositions_ are state formulas;
    - $f\lor g$ and $\neg f$ are state formulas if $f$ and $g$ are state formulas;
    - $Ef$ is a state formula if $f$ is a path formula.
2. _Path formulas_ have a truth value along a specific path.
    - $f$ is a path formula if it is a state formula;
    - $f\lor g$, $\neg f$, $Xf$ and $fUg$ are path formulas if $f$ and $g$ are path formulas.

## Define semantics using Kripke structure

Given a [[Kripke structure]] $M = (S, R, L)$.

- A _path_ in $M$ is an infinite sequence of states
    $$ \pi = s_0, s_1, s_2, \dots $$
- Denote the _suffix_ of $\pi$ starting from $s_i$ as $\pi^i$.
- $M, s \models f$ means that _state formula_ $f$ holds at state $s$.
    - $\forall p \in L(s)$ we have $s \models p$.
- $M, \pi \models f$ means that _path formula_ $f$ holds along the path $\pi$.
    - $\pi \models Xf \iff \pi^1 \models f$

## Relation with LTL and CTL

CTL has a restriction that $X,F,G,U,R$ must be preceded by $E, A$.
LTL do not have the branching logic connectivity $E$. All formulas have the form $Af$ where $f$ is a _path formula_ in which the only state sub-formulas are _atomic proposition_.



