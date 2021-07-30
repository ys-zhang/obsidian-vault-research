# Graph Algorithms


# Model

Graph $G=(V, E, w)$
- number of vertices $n = |V|$
- number of edge $m = |E|$
- edge weight $w: E \to \mathbb{R}$

## Adjacent Matrix

空间复杂度 $O(n^2)$

$$
	A_{ij} = \begin{cases}
		0   &\quad i=j \\
		w(i,j) &\quad (i,j)\in E  \\
		+\infty &\quad (i,j) \notin E \\
	\end{cases}
$$

## Linked List

```go
type Vertex int
type Edge struct {
	Start, End Vertex
	Weight float64
}
//
type Graph map[Vertex][]Edge
```

# Minimal Cost Route

## Single Source Shortest Path (SSSP)

Given a source vertex, compute the shortest path of the source to all other vertices.


# Graph Partition

###### Definition (Partition)

Given a natural number `n`, a `n-cut` hybrid partition $\mathrm{HP}(n) = (F_1, . . . , F_n)$ of a graph $G$, or simply a partition  
of $G$, divides $G$ into `n` small fragments$F_1,\dots,F_n$ such that  
1. $F_i = (V_i, E_i)$, 
2. $V = \bigcup V_i$,
3. $E = \bigcup E_i$.

Denote by $E^v$ (resp. $E_i^v$) the set of edges incident to vertex $v$ in $G$ (resp. $F_i$).

- ***v-cut***:  A vertex `v` is *v-cut* in `HP(n)` if the set of edges incident to `v` is not “complete” at any $F_i$, i.e., 
	$$E^v \neq E_i^v; \quad \forall i ∈ [1,n]$$.
	Each *copy* of such `v` in `HP(n)` is called a **v-cut node** of `v`.
- ***e-cut***  A vertex `v` is *e-cut* if 
	$$\exists i, \; s.t. \; E^v_i =E^v $$
	When there exist multiple copies of `v` in `HP(n)`, we refer to the copy in $F_i$ as an **e-cut node** and the others as **dummy nodes** of `v`.
	
- ***border***: Denote by $F_i.O = \{v ∈ V_i | v ∈ V_j \land i \neq j\}$ the set of border nodes of $F_i$.
- ***master node mapping***, for all nodes in $\mathcal{F}.O=\bigcup F_i.O$
- ***boundary edge***, $\partial(F_i) = \{(s,t)\in E| s \in F_i, j\notin F_i \}$


###### Basic Partition types

 - Partition `HP(n)` is **edge-cut** if 
	1. all vertices are *e-cut*; and  
	2. the *e-cut node* sets of the fragments are pairwise disjoint.  (master node set are disjoint)
- Partition `HP(n)` is **vertex-cut** if the edge sets are disjoint, i.e., $E_i \cap E_j = \emptyset$ for $i , j$, while *v-cut* nodes are replicated.

###### Partition quality

- ***Replication Ratio***    
$$
\begin{align}
f_v = \frac{ \sum|V_i|}{|V|} &\quad \mathrm{vertex\;replication\;ratio} \\
f_e = \frac{ \sum|E_i|}{|E|} &\quad \mathrm{edge\;replication\;ratio}
\end{align}
$$
- ***Balance factor***    A hybrid partition `HP(n)` is said $\lambda_v$ -balanced w.r.t. vertices if $|Vi | ≤ (1+λ_v) \frac{\sum^n_{j=1}|V_j |}{n}$ for all $i ∈ [1,n]$, i.e.,  the number of vertices of each fragment is not too deviated from the average. Similarly, `HP(n)` is $λ_e$ -balanced w.r.t. edges if $|E_i| \le (1 + λ_e) \frac{\sum^n_{j=1}|E_j |}{n}$ for all $i∈[1,n]$.

## Edge Cut Algorithms

## Vertex Cut Algorithms

[[Algorithms for Graph Partitioning, A Survey]]
## Hybrid Algorithms

