Suppose an undirected graph $G=(V, E)$
let $d_i$ denote the degree of node $i$. And $A$ as the adjacent matrix. ($A = A^T$). 
let $D = diag(d_1, \cdots, d_n)$

# Unnormalized Laplacian

$$
	L = D - A
$$

we have 

## Properties

$L\succeq 0$ is semi-definite positive

$$
	v^TLv = \sum_{i\sim j} (v_i-v_j)^2 \ge 0
$$

性质见 [[Fiedler Theory]].
## Relation to the Laplacian operator $\Delta$

suppose the graph represents a grid of some manifold.

then the discrete $\Delta$ is related to $L$.

case dim-2 grid
$$
\begin{align}
	\partial_j f_i &\approx \frac{f_j-f_i}{\delta} \\
	\partial_{-j} f_i &\approx \frac{f_i-f_{-j}}{\delta} \\
	\partial^2_{j} f_i &\approx \frac{\partial_j f_i-\partial_{-j}f_i}{\delta} =\frac{f_j + f_{-j} - 2f_i}{\delta^2} \\
	
  \Delta f_i &\approx { \sum_{i\sim j}f_j - d_if_i \over \delta^2} = -\frac{1}{\delta^2}[Lf]_i
	
\end{align}
$$

let $\delta=1$ then
$$
	\Delta f = -Lf
$$




# Normalized Laplacian

$$
\mathcal L_{ij} = \begin{cases}
	1                       &\quad i=j \\
	\frac{1}{\sqrt{d_id_j}} &\quad i \sim j \\
	0                       &\quad \textrm{otherwise}
\end{cases}
$$

$$
\mathcal L = D^{-\frac{1}{2}}(D-A)D^{-\frac{1}{2}}
$$

- $v$ is a eigen vector of $\mathcal L$ then $D^{-\frac{1}{2}}v$ is a [[generalized eigen vector]] of $L$
	$$D^{-\frac{1}{2}}LD^{-\frac{1}{2}}v=\lambda v \iff Lu=\lambda Du, \; u=D^{-\frac{1}{2}}v$$
by [[Rayleigh Quotient]]
$$
	\frac{v^T\mathcal Lv}{v^Tv} = \frac{v^TD^{-\frac{1}{2}}LD^{-\frac{1}{2}}v}{v^Tv} = \frac{u^TLu}{u^TDu} =\frac{\sum_{i\sim j} (u_i-u_j)^2 }{\sum_id_iu_i^2}
$$


## Properties

see [[Cheeger Inequality]].