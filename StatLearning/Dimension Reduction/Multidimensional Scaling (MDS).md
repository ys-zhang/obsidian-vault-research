# MDS
Multidimensional Scaling (MDS) roots in psychology which aims to **recover Euclidean coordinates given pairwise distance metrics or dissimilarities.**

> Equivalent to [[Principle Component Analysis (PCA) | PCA]] when pairwise distances are Euclidean (norm induced by inner product, i.e. we have angle).

##  Metric Multidimensional Scaling Problem

> Given pairwise distances between data points, can we find a system of *Euclidean coordinates* for those points whose *pairwise distances* meet given constraints?

### Metric MDS
###### General ideas of classic (metric) MDS is: 
1. transform **squared distance matrix** $D = (d^2_{ij})$ to an **inner product form; **
2. compute the **eigen-decomposition** for this inner product form.

Give the squared distance matrix $D = (d^2_{ij}) \in \mathbb{R}^{n\times n}$ and $p > 0$, find
coordinates $x_i\in \mathbb{R}^p \; \mathrm{for}\; 1\le i\le n$,
let 
$$X = (x_1, \dots, x_n) \in \mathbb{R}^{p \times n}$$
**each point as a column.**
then
$$
\begin{align}
	D_{ij} &=d^2_{i,j}=\|x_i - x_j\|^2 =x_i^Tx_i -2x_i^Tx_j +x_j^Tx_j \\
	&= K_{ii} -2K_{ij} + K_{jj} \\
	&= k \textbf{1}^T - 2K +  \textbf{1}k^T
\end{align}
$$
![[Pasted image 20210722220017.png]]
where
$$
\begin{align}
	K &= X^TX \in \mathbb{R}^{n\times n} \\
	K_{ij} &= \langle x_i, x_j \rangle = x_i^Tx_j \\
	k &= diag(K)
\end{align}
$$

**Coordinate mean and centered data**
$$
\begin{align}
	\hat{\mu}_n &= \frac{1}{n}X\textbf{1} \\
	\tilde{x_i} &= x_i - \hat{\mu}_n = x_i(1-\frac{1}{n}\textbf{1})\\
	\tilde{X} &= X - \frac{1}{n}X\cdot \textbf{1}\textbf{1}^T = XH_n
\end{align}
$$
where $H_n$ is the ***centering matrix*** (左乘), $H_n = H_n^T$
$$
H_n = I - \frac{1}{n}\textbf{1}\textbf{1}^T
$$

and
$$
\tilde{K} = \tilde{X}^T\tilde{X} = B = -\frac{1}{2}HDH^T
$$

###### Code

[2.2. Manifold learning — scikit-learn 0.24.2 documentation](https://scikit-learn.org/stable/modules/manifold.html#multidimensional-scaling)
![[Pasted image 20210723003648.png]]

### Theory

###### 半正定
$A, B \succeq 0 \implies A + B \succeq 0, A \circ B\succeq 0$, where $(A \circ B)_{ij} = A_{ij}+B_{ij}$ 
######   Conditionally Negative Definite
$A \in \mathbb{S}^{n \times n}$ is ***Conditionally Negative Definite*** (c.n.d.) iff. $\forall v\in \mathbb{R}^n, \textbf{1}^Tv=0$, we have $v^TAv \le 0$


#### Classical MDS


######  Lemma 2.1
$\forall \alpha, \textbf{1}^T \alpha = 1$
$$
-\frac{1}{2}H_{\alpha}CH_{\alpha}^T \succeq 0 \iff 
	C \textrm{ is c.n.d. }
$$
where $H_{\alpha}=I -1\cdot\alpha^T$.

###### Theorem 2.2    Classical MDS

$D \in \mathbb{S}^{n \times n}, C=D-\frac{1}{2}d\textbf{1}^T -\frac{1}{2}\textbf{1}d^T$ where $d=diag(D)$.  (or $C_{ij} = d_{ij}-\frac{d_{ii}+d_{jj}}{2}$) we have

$$
\begin{align}
   &B_α = -\frac{1}{2}H_αDH_α^T = -\frac{1}{2}H_αCH_α^T \\
   &C_{ij} = B_{ii}(α) + B_{jj}(α) - 2B_{ij}(α) \\
   &D \textrm{ is c.n.d.} \iff C \textrm{ is c.n.d.} \\
   (\star)\quad & C \textrm{ is c.n.d.} \implies    C \;\textrm{ is a square distance matrix}  
\end{align}
$$

只要 $B_\alpha$半正定则$C$是距离矩阵.
![[Classical MDS.png]]

######    Theorem 2.3 (Schoenberg Transform)

%%Given $D$ a *squared distance matrix*, $C_{ij} = Φ(D_{ij})$

   $$C \textrm{ is a squared distance matrix } \iff Φ \textrm{ is a Schoenberg Transform }$$%%
   
   ![[Pasted image 20210723152728.png]]
