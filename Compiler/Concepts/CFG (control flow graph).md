A graph with [[basic block]] as node and branch as edges.

# Concepts
1. Reachability: If a subgraph is not connected from the subgraph containing the entry block, that subgraph is **unreachable** during any execution.
2. A block $M$ _[dominates](https://en.wikipedia.org/wiki/Dominator_(graph_theory) "Dominator (graph theory)")_ a block $N$ if every path from the entry that reaches block $N$ has to pass through block $M$.

## Reducibility
A **reducible** CFG is one with **edges that can be partitioned into two disjoint sets: forward edges, and back edges**, such that:

1. Forward edges form a [directed acyclic graph](https://en.wikipedia.org/wiki/Directed_acyclic_graph "Directed acyclic graph") with all nodes reachable from the entry node.
2. For all [[back edge]]s $(A, B)$, node $B$ [dominates](https://en.wikipedia.org/wiki/Dominator_(graph_theory) "Dominator (graph theory)") node $A$.  (back edges are only loops)