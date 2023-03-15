#model-checking 
#logic 

A Buchi Automaton is a tuple $(Q, \Sigma, \delta, q_0, F)$ where
- $Q$ is a _finite_ set of states;
- $\Sigma$ is a finite alphabet;
- $\delta: Q\times\Sigma\times Q$ is a transition relation;
- $q_0\in Q$ is the initial state;
- $F\subset Q$ is a set of final states
---
An _infinite_ word $w$ is **accepted** by the automaton if there exists $q_f\in F$ such that the run over $w$ visits $q_f$ infinitely often.

The set of infinite words accepted by an automaton $A$ (i.e., the **language** defined by $A$) is denoted by $\mathscr L_w(A)$.

A _Generalised Buchi Automaton (GBA)_ in one for which $F$ is a set of acceptance sets such that _acceptance_ is defined as infinite visit of one of the acceptance set in $F$.

see also [[Finite State Automaton (FSA)]]

---

# Relation to Model Checking

Buchi Automaton is usually used in [[linear-time temporal logic (LTL)]] based model checking, where LTL formulas are compiled to a Buchi Automaton.

A computation satisfying LTL formula $\varphi$ is an infinite word over the alphabet $\Sigma=2^{Prop}$, which is accepted by the automaton $A_\varphi$ corresponding to $\varphi$.   


