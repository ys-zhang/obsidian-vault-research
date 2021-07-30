---
title: Directed greybox fuzzing
authors: Marcel Böhme, Van-Thuan Pham, Manh-Dung Nguyen, Abhik Roychoudhury
year: 2017
date updated: '2021-07-15T16:59:02+10:00'

---

#program-test #fuzzing #academic-paper

# Problem

Existing [[greybox fuzzing]] fuzzers cannot be effectively directed.

> In this paper, we introduce** Directed Greybox Fuzzing (DGF)** which generates inputs with the objective of reaching a given set of target program locations efficiently. We develop and evaluate a [[Simulated annealing]]-based power schedule that gradually assigns more energy to seeds that are closer to the target locations while reducing energy for seeds that are further away.

Given a target line or block of code, the problem can be rephrased as **find out a seed s.t. its path passes the target code, i.e. a reachability problem.**

This is useful when only the changed code blocks need to be tested, including

- [[patch testing]]

# Idea

> On a high level, we cast reachability as an optimization problem and employ a specific meta-heuristic to minimize the distance of the generated seeds to the targets.

DGF casts the reachability of target locations as optimization problem while existing directed (whitebox) fuzzing approaches cast reachability as iterative constraint satisfaction problem.

Directed greybox fuzzing is effectively directed and efficiently complements symbolic execution-based directed fuzzing.

![[greybox-fuzzing-arch.excalidraw]]

In the **exploitation phase**, **AFLGo** generates substantially more new inputs from seeds that are closer to the target—essentially not wasting precious time fuzzing seeds that are too far away.

**AFLGo** slowly transitions from the** exploration phase** to the **exploitation phase**, according to the annealing function implemented as **power schedule**

# Measure Energy

> We define an inter-procedural measure of distance (i.e., seed to target locations) that is fully established at instrumentation-time and can be efficiently computed at runtime.

The distance computation requires finding the shortest path to the target nodes in the call graph and the intra-procedural control-flow graphs which are readily available in [[LLVM]].

## Notations

| Notation                | Concept                                | Description                               |
| ----------------------- | -------------------------------------- | ----------------------------------------- |
| $T_f$                   | target function set                    |                                           |
| $T_b$                   | target [[basic block]] set             |                                           |
|                         | function-level target distance         |                                           |
| $d_f(n_1, n_2)$         | function distance                      | functions distances in the call graph CG. |
| $R(n, T_f) \subset T_f$ | reachable target function set from $n$ |                                           |

$d_f(n_1, n_2)$ is the number of edges along the shortest path between functions $n$ and $n'$ in the call graph _CG_. 

## Function-level target distance

the function-level target distance $d_f(n,T_f)$ between a function $n$ and the target functions $T_f$ as the ***harmonic mean*** of the function distance between $n$ and any reachable target function $t_f ∈ T_f$ :

$$ 
d_f(n, T_f) = 
\begin{cases}
	\frac{1}{\sum_{t\in R(n, T_f)} \frac{1}{d_f(n, t）}} &\quad R(n, T_f) \neq \emptyset \\
	\infty & \quad R(n,T_f) = \emptyset
\end{cases}
$$

> The *harmonic mean* allows to distinguish between a node that is closer to one target and further from another and a node that is equidistant from both targets. 
> In contrast, the *arithmetic mean* would assign both nodes the same target distance.
> __*harmonic mean* prefers a more balanced node $n$.__

## Block-level target distance

The basic-block-level target distance determines the distance from a basic block to all other basic blocks that *call a function*, in addition to a multiple of the function-level target distance for the function that is called.

### inside one function $i$
More formally, we define ***BB distance*** $d_b(m_1,m_2)$ as the number of edges along the shortest path between basic block $m_1$ and $m_2$ in the **control flow graph** $G_i$ of function $i$.

Let $N (m)$ be the set of functions called by basic block $m$ such that $$\forall n \in N(m), \; R(n, T_f) \neq \emptyset$$.

Let $T$ be the set of basic blocks in $G_i$ such that 
$$\forall m \in T, \; N(m) \neq \emptyset $$

The **basic-block-level target distance**,
$$
d_b(m,T_b) = \begin{cases}
	0         &\quad m \in T_b \\
	c \cdot \min_{n\in N(m)} (d_f(n, T_f)) &\quad m \in T \\
	[\sum_{t\in T} (d_b(m, t) + d_b(t, T_b) )^{-1}]^{-1} &\quad \mathrm{otherwise}
\end{cases}
$$

> $T$ is the set of blocks calls reachable functions, functions can reach $T_f$.

##  Normalized seed distance

Let $\xi (s)$ be the ***execution trace*** of a seed $s$.

**Seed distance**
$$
d(s, T_b) = \frac{\sum_{m\in \xi(s)} d_b(m, T_b)}{|\xi(s)|}
$$

**Normalized seed distance**
$$
\tilde{d}(s, S, T_b) = \frac{d(s, T_b) - \min_{s'\in S} d(s', T_b)}{\max_{s'\in S} d(s', T_b) - \min_{s'\in S} d(s', T_b)} 
$$

# Simulated Annealing

> SA is a [[Markov Chain Monte Carlo method (MCMC) | MCMC ]] method for approximating the global optimum in a very large, often discrete search space within an acceptable time budget.