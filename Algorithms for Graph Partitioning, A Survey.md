
Fjällström, P. O. (1998). _Algorithms for graph partitioning: A survey_ (Vol. 3). Linköping: Linköping University Electronic Press.

# The Problem

Given a graph $G =(N,E)$ where $N$ is a set of *weighted nodes* and $E$ is a set of *weighted edges* and a positive integer $p$, find $p$ subsets $N_1,N_2, \dots, N_p$ of $N$ such that  
1. *$p$-way partition*: $N=\bigcup N_i$ and $N_i \cap N_j = \emptyset$
2. *Weight Balance*:    $W(i) \approx W/p$
3. *the cut size* i.e. the sum of weights of edges crossing between subsets is minimized.

- *node weight*: computation cost on node
- *edge weight*: transition/sync cost on edge
- *static v.s. dynamic*: whether the graph will change during the whole process.

###### Trend of research

>    Therefore all practical algorithms are heuristics that differ with respect to cost time and memory space required to run the algorithm and partition quality i.e. *cut size*

# Sequential Methods

## Local improvement methods

A local improvement algorithm takes as **input a partition of a graph** $G$ and tries to **decrease the cut size** by some local search method.

> Given an initial bisection the Kernighan Lin (KL) method tries to find a sequence of **node pair exchanges** that leads to an improvement of the **cut size**

[Kernighan–Lin algorithm - Wikipedia](https://en.wikipedia.org/wiki/Kernighan%E2%80%93Lin_algorithm)