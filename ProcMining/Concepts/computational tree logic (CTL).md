#model-checking

Computational Tree Logic (CTL) is based on notion of [[branching time]].

Infinite tree of states represents **behavior** of system (incl. all possible traces of the system), like state tree in [[Reinforcement Learning]].

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