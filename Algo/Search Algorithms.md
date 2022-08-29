#algorithm 
#algorithm-searching


# Modelling the problem

## State Model 

A state model $S(P)$ is a tuple $(S, s_0, S_G, A, f, c)$
1. state space $S$ :   **finite** and discrete.
2. **one** known _initial state_: $s_o \in S$
3. goal set: $S_G \subset S$
4. Action set: $A = \cup_{s\in S} A_s$
5. deterministic transition function: 
    $$f(s, a) :: \forall s \in S \to A_s \to S$$
6. positive deterministic **action cost** : $c :: A \to R_+$
  
> [!CONCEPTS]
> - **Search node $n$:** Contains a state reached by the search, plus information about how it was reached.
> - **Path cost $g(n)$:** The cost of the path reaching $n$.
> - Optimal cost $g^*$.
> - Search strategy: Method for deciding which node is expanded next.
> - **Open list/frontier**: Set of all nodes that currently are _candidates for expansion_.
> - **Closed list/expended**: Set of all states that were already expanded.


# Search Algorithms

## Search Types:

1.  Blind Search V.S. Heuristic/Informed Search
2.  Systematic Search V.S. Local Search


## Systematic Blind Search

![[algo-search-sys-blind.png]]






