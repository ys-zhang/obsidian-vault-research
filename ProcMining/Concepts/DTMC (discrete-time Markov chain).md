# Discrete-time [[Markov Chain]]s

![[DTMC.excalidraw|center]]

## Definition

A (*labeled*) discrete-time Markov Chain (DTMC) is described as a tuple $(S, \mathbf P, AP, L)$ where
- $S$ a countable set of **states**
- $\mathbf P$ is the **transition probability matrix**, $\mathbf P[s, s'] = \Pr(s'|s)$ and $\sum_{e} \mathbf P[s, e] = 1$
- $AP$ the set of _atomic properties_
- $L : S \to 2^{AP}$ a labelling function which assigns to each **state** $s ∈ S$ the set $L(s)$ of atomic propositions that are valid in the **state** $s$.

## Path and Cylinder Sets

A **path** is an execution of a DTMC. A path
$$
\omega = s_0s_1\dots
$$
where
  - $s_i\in S$
  - $\mathbf P[s_i, s_j] > 0$

Let $Path_{fin}^D(s)$ denote the set of all finite length paths _start from state $s$_.
For each $\omega_{fin}$ define the **cylinder set** $C(\omega_{fin})$ as all **infinite paths** with _prefix_ $\omega_{fin}$

$$
\Pr_s(C(\omega_{fin})) = \mathbf P(\omega_{fin}) = \prod_0^{|\omega_{fin}|-1} \mathbf P[s_i, s_{i+1}]
$$
