#Prolog #algorithm 

> Resolution is based on the idea of _proof by contradiction_: To prove a logical consequence of a set of axioms, we assume the opposite of what we want to prove, and show that this contradicts the axioms which we take for granted.


Logically, when Prolog answers a query, it tries to find a _resolution refutation_ of the negated query and the set of clauses that constitute a program. When a refutation is found, it means that the query, with the appropriate bindings, is a logical _consequence_ of the program.

Resolution leaves a lot of freedom in _how_ the search for contradictions is to be performed. Accordingly, Prolog programs can be interpreted with different evaluation strategies. 
- The default execution strategy is called SLDNF resolution: SLD resolution with _negation as finite failure_. 
- Another popular execution strategy of Prolog programs is called SLG resolution, also known as [_tabling_](https://www.metalevel.at/prolog/memoization#tabling). 
- Other variants include iterative deepening and various heuristics.