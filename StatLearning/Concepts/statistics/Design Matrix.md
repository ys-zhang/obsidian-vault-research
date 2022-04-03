$$
X = 
\begin{bmatrix}
  1 & x^{(1)}_1 & x^{(1)}_2 & \cdots & x^{(1)}_p \\
  1 & x^{(2)}_1 & x^{(2)}_2 & \cdots & x^{(2)}_p \\
  \vdots & \vdots & \vdots & \ddots & \vdots \\
  1 & x^{(N)}_1 & x^{(N)}_2 & \cdots & x^{(N)}_p \\
\end{bmatrix}
\in \mathbb R^{N\times(1+p)}
$$


```ad-note
title: Rows and Columns of the Design Matrix
1. Each _row_ of the design matrix represents a _sample_;
2. Each _col_ of the design matrix represents a _regressor/design/explanatory variable_;
```


```ad-warning
title: Centered Design Matrix
We require that the design matrix is **centered**, i.e., except the first column all columns has a average of $0$.

$$
\forall j > 1,\; \sum_i x_j^{(i)} = 0
$$
```