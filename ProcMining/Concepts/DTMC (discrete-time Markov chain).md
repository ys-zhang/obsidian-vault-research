# Discrete-time [[Markov Chain]]s

![[DTMC.excalidraw|center]]

## Definition

A (*labeled*) discrete-time Markov Chain (DTMC) is described as a tuple $(S, P, AP, L)$ where
- $S$ a countable set of **states**
- $P$ **transition probability matrix**, $P[s, s'] = \Pr(s'|s)$
- $AP$ the set of _atomic properties_
- $L : S \to 2^{AP}$ a labelling function which assigns to each **state** $s ∈ S$ the set $L(s)$ of atomic propositions that are valid in the **state** $s$.

