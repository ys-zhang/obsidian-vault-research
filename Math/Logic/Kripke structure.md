#model-checking 
#logic 

A _Kripke_ structure $M = (S, R, L)$
where 
- $S$ is the set of states
- $R\subset S\times S$ is the transition relation, which must be **total**, i.e., all states have at least 1 successor.
- $L: S \to 2^{AP}$ is a function that labels each state with a set of _atomic propositions_. 


