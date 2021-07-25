Let 
$$
   X = [X_1,X_2, \cdots ,X_n] \in \mathbb{R}^{p×n}
$$

# The Problem

find a affine space 
$$
	\{ \mu + U\beta \; | \; \beta \in \mathbb{R}^k \}\subset \mathbb{R}^p \quad U\in \mathbb{R}^{p \times k}
$$

Aims to minimize the loss
$$
\begin{align}
	\min_{\mu,\beta,U} &\quad f(\mu,\beta,U) = \sum_i\|x_i -\mu-U\beta_i\|^2 \\
	s.t. &\quad U^TU=I_p
\end{align}
$$

$$
	\partial_\mu f=0 \implies \hat{\mu} = \frac{1}{n}\sum_1^n x_i
$$

$$
\partial_{\beta_i}f =  (x_i -\mu-U\beta_i)^TU=0 \implies \beta_i=U^T(x_i -\hat{\mu})
$$

let $P_k = UU^T$ be the projection matrix
$$
	\min \sum_1^n \| x_i -P_kx_i \|^2
$$
**minimize projection loss.**


![[Pasted image 20210724211736.png]]