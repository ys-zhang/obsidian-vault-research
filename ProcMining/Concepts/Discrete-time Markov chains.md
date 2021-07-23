# Discrete-time [[Markov Chain]]s

![[DTMC.excalidraw]]

## Definition

A (*labeled*) discrete-time Markov Chain (DTMC) is described as a tuple $(S, P, AP, L)$ where
- $S$ a countable set of **states**
- $P$ **transition probability matrix**, $P[s, s'] = Prob(s'|s)$
- $AP$ the set of atomic properties
- $L : S \to 2^{AP}$ a labeling function which assigns to each **state** $s ∈ S$ the set $L(s)$ of atomic propositions that are valid in the **state** $s$.

