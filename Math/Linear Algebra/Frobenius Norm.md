Matrix norm defined by inner product, $\forall X,Y \in \mathbb{R}^{p\times n}$ 
$$
	\langle X,Y \rangle = \sum_{ij}X_{ij}Y_{ij}
$$
the norm is defined by
$$
	\|X\|_F = \sqrt{\langle X,X \rangle} = \sqrt{\sum_{ij}X_{ij}^2} = \sqrt{tr(X^TX)} = \| X(:) \|_2
$$