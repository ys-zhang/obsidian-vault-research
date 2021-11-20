# Discrete Differential Geometry

[CS 15-458/858: Discrete Differential Geometry](https://brickisland.net/DDGSpring2021/)

# Problems And Goals

Our main goal is to show how *fundamental geometric concepts* (like curvature) can be understood from **complementary computational** **and**
**mathematical** points of view.

## Topics

- curves and surfaces, curvature, connections and parallel transport
- exterior algebra, exterior calculus, Stokes’ theorem
- simplicial homology, de Rham cohomology, Helmholtz-Hodge decomposition,
- conformal mapping, finite element methods, and numerical linear algebra

One goal of these notes is to provide an introduction to working with real-world geometric data, expressed in the language of **discrete exterior calculus (DEC)**

# Combinatorial Surfaces

**Combinatorial surfaces**, or descriptions of shapes is a a purely discrete point of view that only tell you how surfaces are *connected up* and not *where they are in space.*

There several different ways to encode the *connectivity of combinatorial* surfaces: using an **abstract simplicial complex**, **adjacency matrices**, and a **half-edge mesh**, all of which tie in to the richer geometric objects and algorithms we want to work with later on.

## Abstract simplicial complex

Focus purely on connectivity: which pieces of the surface are connected to each other, and how.

The idea of a **simplicial complex** is to specify subsets of these **vertices** that are “right next to each-other,” called **k-simplices**

### Definition

Given a set of **vertices ($0$-simplicities)**:

$$V=\{1, 2, \cdots N \}$$

**K-simplex (单形)**

$$\sigma(P) = \{\sum_{i=0}^K t_i p_i : \Sigma t_i = 1 ,\; t_i \ge 0\} $$

**Face of a simplex:** non-empty subset of a **simplex.** It is ***proper*** if it is a *strict* subset*.*

**simplicial-complex (单纯复形)**

that a collection of simplices $\mathcal{K}$ is a simplicial complex if for every simplex $\sigma \in \mathcal{K}$, every face $\sigma_0 ⊆ \sigma$ is also contained in $\mathcal{K}$:

- a set of simplices $\mathcal{K}$;
- intersection of any 2 simplices is a simplex in $\mathcal{K}$;
- every face of every simplex in $\mathcal{K}$ also in $\mathcal{K}$.

A complex $\mathcal{K}$ is a **pure** $k$-simplicial complex if every simplex $\sigma_0 \in \mathcal{K}$ is contained in some simplex of degree $k$ (possibly itself).

### Operations on simplices

For any subset $S$ of a simplicial complex $\mathcal{K}$ (not necessarily a **subcomplex**):

- **Simplicial Star (neighborhood, open set):** The star `St(S)` is the *collection of all simplices* in $\mathcal{K}$ that contain any *simplex* in $S$.
	![[simplicial-complex-star.png]]
- **Closure (compact set):** The closure `Cl(S)` is the *smallest* (i.e., fewest elements) ***subcomplex*** of $\mathcal{K}$ that contains $S$.
	![[simplicial-complex-closure.png]]
- **Link (similar to boundary): `Lk(S)=Cl(St(S)) \ St(Cl(S))`**
	![[simplicial-complex-link.png]]
Another closely related object is the **boundary** `bd(K')` of a pure $k$-subcomplex $\mathcal{K'} \subset \mathcal{K}$. The **boundary** is the *closure of the set of all simplices $\sigma$ that are proper faces of exactly one simplex* of $\mathcal{K'}$.







![[simplicial-complex-boundary.png]]

### Orientation

An orientation the inverse number of the order of a simplex's $0$-faces:

$$(-1)^{\tau[v_0, \cdots, v_K]}$$

- **relative orientation**:  In general, if two $k$-simplices $\sigma_1, \sigma_2$ share exactly $k − 1$ vertices, then they have the same orientation if their restrictions to these *shared vertices are **oppositely** oriented.*

### simplicial surfaces

An **abstract simplicial surface** is a pure *simplicial $2$-complex* where *the **link** of every vertex is a single loop of edges*, or equivalently, where *the star of every vertex is a combinatorial disk made of triangles*.

A (combinatorial or abstract) ***simplicial n-manifold*** is a pure simplicial n-complex where the link of every vertex is a simplicial (n − 1)-sphere.

The fact that every vertex has a “disk-like” *neighborhood* captures the basic idea of a *topological surface*; we therefore say that such a complex is *manifold.*

We can extend our definition a bit to a simplicial surface with boundary by also allowing the link to be a simple path of edges, rather than a loop.

An **oriented simplicial surface** is an abstract simplicial surface where we can assign a ***consistent*** orientation to every triangle, i.e., where any two triangles that share a common edge are given the same orientation.

## Adjacent Matrices

**Problem**: how to encode complexes?

We need to encode the simplices and the connections of the simplices.

- use `uid` to represent simplices
- use matrices $A_i$ to encode connection of $i$-simplices.

$$A_{i, j} = 1 \;\; \mathrm{iff.}\;\; \sigma_j \subset \sigma_i\;,\; \sigma_i \in \Sigma_{i}(\mathcal{K}) \; , \; \sigma_j \in \Sigma_{i-1}(\mathcal{K}) \;\; $$

where $\Sigma_i(\mathcal{K})$ is the set of i-simplices of the complex.

![[adjacent-matrix.png]]


```go
type Simplex struct {
	ID, Dim int
}

// adjacent matrix
type mat [][]int

// Simplicial Complex
type Complex struct {
	splx map[int]*Simplex
	adjs []mat      // adjacent matrices
}

func (cplx *Complex) Dim() int {
	return len(cplx.cnns)
}

func (cplx *Complex) Splx(id int) *Simplex {
	return cplx.splx[id]
}

func (cplx *Complex) AdjMat(dim int) mat {
	return cplx.cnns[dim-1]
}
```

## Half-edge Mesh

The basic idea is to consider that for every unoriented edge `{i, j}` between vertices `i` and `j`, we have two oriented edges `(i, j) ≠ (j, i)` which in this context we refer to as **half-edges.**

> The data structure used here is the [[Double-connected Edge List]]

Let $H$ be the set of *half-edges*, we have the following function describes the connections:

- **twin**:    $\eta: H \to H$  s.t. $\eta$ maps an *oriented edge* to its inverse counterpart.
- **next**:    $\rho_{\sigma}: H(\sigma) \to H(\sigma)$ map to input's next *half-edge* under the orientation of $\sigma$.

Going the other direction, we can easily figure out the vertices, edges, and faces of a polygonal
mesh from nothing more than the two maps $\eta$ and $\rho$.

- the ***faces*** of the mesh are described by the ***orbits*** of the “next” map $\rho$.
- the ***orbits*** of $\rho$ describe the ***edges***

- the **faces** are orbits of $\rho$ ;
- the **edges** are orbits of $\eta$ ;
- the **vertices** are orbits of $\rho \circ \eta$ .

[exercise](https://www.notion.so/exercise-c6c9cf0b0d2d4f41ba18da89985b5d58)

# Differential Geometry

Its geometry can be described via a map $f : D_k \to M$ from a disk $D_k \subset \mathbb{R}^k$ in the Euclidean to a neighborhood of the manifold $M$.

![[manifold-chart.png]]


The differential of such a map, denoted by $df$ , tells us how to map a vector $X$ in the plane to the
corresponding vector $df(X)$ on the surface tangent space.

$$df_x : \mathbb{R}^k \to T(f(x))$$

# Misc.

## Links

git clone --recursive [https://github.com/GeometryCollective/ddg-exercises](https://github.com/GeometryCollective/ddg-exercises)

## 基本群

- 一个拓扑空间中， 从一点出发并回到该点的闭合曲线，称为该点的一个***回路*。**
- 如果一条***回路***能够连续地形变成另一条回路（起始和终点不动），就称这两条路***同伦等价***。
- 对于给定的一点， 所有的过该点的***回路***等价类全体形成一个集合。 这个集合具有加法性质， 即两条回路可以相加形成新的回路。这样此集合形成了一个群， 称为该点的**基本群**。