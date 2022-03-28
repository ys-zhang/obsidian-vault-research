# The framework

![[Pasted image 20210726204515.png]]

All the current techniques in manifold learning, as extensions of PCA and  
MDS, are often called as ***Spectral Kernel Embedding***.

>    The basic problem is: given a set of data points $X =[x_1; x_2; \cdots ;x_n]\in \mathbb{R}^{p\times n}$, how to find out $Y=[y_1; y_2; \cdots ;y_n]\in \mathbb{R}^{d\times n}$, where $d<p$,  such that some geometric structures (local or global) among data points are best preserved.


1. Construct a data graph $G = (V; E)$, where $V = \{x_i : 1\le i\le n \}$
	1. (topological, undirected graph) $\epsilon$-neighbourhood.
		$$ i \sim j \iff d(x_i, x_j) \le \epsilon $$
	2. (directed graph) k-nearest neighbour, 
		$$(i; j) \in E \iff j \in N_k(i)$$
2.  Construct a positive semi-definite matrix $K$ (kernel).
3.  Eigen-decomposition $K = UΛU^T$ , then $Yd = U_d\Lambda_d^{1 \over 2}$

- **[[Principle Component Analysis|PCA]]**:    $G$ is complete, $K = Σ^n$ is a ***covariance matrix***.
- **[[Multidimensional Scaling (MDS)|MDS]]**:    $G$ is complete,$K = -\frac{1}{2}HDH^T$ , where $D$ is the ***Gram matrix.***
- **[[ISOMAP]]**:  $G$ is incomplete.
	$$ D_{ij} = \begin{cases}
		d(x_i, x_j)  &\quad (i, j) \in E \\
		\hat{d}_g(x_i, x_j) &\quad (i, j) \notin E
	\end{cases}$$
where $\hat{d}_g$ is a graph shorted path. Kernel same as MDS.
- **[[LLE]]**:    G is incomplete. $K = (I - W)^T(I - W)$, where
	$$W_{ij}^{n\times n} =  w_{ij} \times \mathbb{I}_{j\in \mathcal{N}(i)}$$
	and $w_{ij}$ solves the following optimization problem
	$$\min_{\sum_j w_{ij} = 1} \| x_i-\sum_{j\in\mathcal N(i)} w_{ij}(x_j-x_i) \|^2$$
	that is express $x_i$ by a convex combination of its neighbours.
	The embedding is computed from
	$$\min_Y \sum_i \| Y_i - \sum_j W_{ij}Y_j \|^2 = tr((I-W)Y^TY(I-W)^T)
	$$
This is equivalent to find smallest eigenvectors of $K = (I - W)^T (I - W)$.

> Under dense-sample and regularity conditions on manifolds, [[ISOMAP]] is proved to show convergence to **preserve geodesic distances on manifolds**.


![[Pasted image 20210726220856.png]]
