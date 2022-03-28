# Unit Normal Scaling

$$
\begin{align}
z_{ij} &= \frac{x_{ij} - \bar x_{j}}{s_j}\\
y_i^* &= \frac{y_i -\bar y}{s_y}
\end{align}
$$
where $s$ is the unbiased standard variance.

# Unit Length Scaling
$$
\begin{align}
w_{ij} &= \frac{x_{ij} - \bar x_{j}}{\sqrt {D_{jj}}}\\
y_i^0 &= \frac{y_i -\bar y}{\sqrt{SS_T}}
\end{align}
$$

where
$$
D = (I-11^T)X^TX(I-11^T)
$$
is the centered [Gram Matrix](https://en.wikipedia.org/wiki/Gram_matrix).

And
$$
W^TW = \frac{Z^TZ}{n-1}
$$
is exactly the covariance matrix of $X$