Let $A$ be the adjacent matrix. Consider random walk with transition probability

$$
	\Pr(i \to j) = P_{ij} = {A_{ij}\over \sum_jA_{ij}} = {1 \over d_i}
$$

the stationary $\pi$ satisfies
$$
	\pi^TP =\pi^T
$$
is the left eigen vector of $1$.
$$
	P = D^{-1}A= D^{-\frac{1}{2}}(I-\mathcal L)D^{-\frac{1}{2}}
$$
$P$ 相似于 $I - \mathcal L$ where $\mathcal L$ is the normalized [[Laplacian Matrix]]. 

##### $P$ 与 $\mathcal L$ 的特征值有如下关系

$$
\lambda_i(P) = 1-\lambda_i(\mathcal L)
$$

>    You can see $\bar u$ is the eigenvector of $\mathcal L$, and we can get left eigenvectors of $P$ from $\bar u$ by multiply it with $D^{1\over 2}$ on the left side, i.e. $v = \bar u D^{1\over 2}$. Similarly for the right eigenvectors  $v = D^{1\over 2}\bar u$.

![[Pasted image 20210728172754.png]]


# Relation to graph partition

$G = (V; E)$, $S\subset V$, $\bar S=V-S$.

$$
\begin{align}
	\mathrm{Vol}(S) &= \sum_{i\in S}d_i \\
	\mathrm{CUT}(S) &= \sum_{i\in S, j\notin S}A_{ij} \\
	\mathrm{NCUT}(S) &= \frac{\mathrm{CUT}(S)}{\mathrm{Vol}(S)\land \mathrm{Vol}(\bar S)}  
\end{align}
$$
NCUT: normalized cut.

##### Cheeger constant

$$
   h_G := \min_S \mathrm{NCUT}(S)
$$
or
$$
   h_G := \min_S  \{ h_S \lor h_{\bar S} \}
$$

##### Cheeger ratio (expander)

$$
   h_S := {\mathrm{CUT}(S) \over \mathrm{Vol}(S)}
$$



# Cheeger Inequality


For every undirected graph $G$,
$$ \frac{h_G^2}{2} \le	\lambda_1(L) \le 2h_G $$