#statistics 

# Spline Method


**Splines** are _piecewise polynomials_ of order $k$. 
The joint points of the pieces are usually called **knots**.

A **natural cubic spline** adds additional constraints, namely that the function is _linear beyond the boundary knots_. 

Generally we require the function values and _the first $k − 1$ derivatives_ to agree at the **knots**, so that the spline is a continuous function with $k − 1$ continuous derivatives. (if all $k$ derivatives are equal then the left part and the right part are exact same function).

Note that for a $M$ spline with $K$ knots, the **truncate power basis** :

$$
\begin{align}
h(x) &= (x - \xi_l)^{M}_+ & l= 1,\;\dots,\;K  \\
h(x) &= x^k & k= 1,\;\dots,\;M
\end{align}
$$
can serve as a **basis for splines**.


In real computation, often the **B-spline basis** is used.


![[Pasted image 20220328211207.png]]


![[Pasted image 20220328211404.png]]


# Smoothing Splines

The Loss function with **smooth penalty**

$$
RSS(f,\lambda) = \sum_{i=1}^N (y_i - f(x_i))^2 + \lambda \int \|f''(t)\|^2 dt
$$







