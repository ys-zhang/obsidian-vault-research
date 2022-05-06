#algorithm 


# Theorem

For a recurrent problem
$$
	T(n) = aT(\frac{n}{b}) + n^k
$$

then

$$
T(n) \in
\begin{cases}
	\Theta(n^k) &\quad a < b^k	\\
	\Theta(n^k\log n) &\quad a = b^k	\\
	\Theta(n^{\log_b a}) &\quad a > b^k	\\
\end{cases}
$$


# Cases

$$
\begin{align}
M(n) &= 2M(\frac{n}{2}) + O(n\log n) \\
N(n) &= 2N(\frac{n}{2}) + O(\frac{n}{\log n}) \\
P(n) &= \sqrt{n} P(\sqrt n) + n
\end{align}
$$

- $M(n) \in O(n\log ^2 n)$
- $N(n) \in O(n)$
- $P(n) \in O(n\log\log n)$



# Example

$$
K(n) = K(\frac{n}{2}) + 2K(\frac{n}{3}) + 3K(\frac{n}{4}) + n^2
$$

by recursion tree

$$
K(n) = n^2 \cdot \sum_{i=0}^{n} \sum_{\alpha + \beta + \gamma=i} \frac{2^\beta3^\gamma}{(2^\alpha3^\beta4^\gamma)^2}
$$
reduce to 

$$
\begin{align}
K(n)/n^2 &=  \sum_{i=0}^{\log n} \sum_{\alpha + \beta + \gamma=i}2^{\beta-2\alpha-4\gamma}\times3^{\gamma-2\beta} \\
&= \sum_{i=0}^{\log n}\sum_{\alpha+\beta+\gamma=i}2^{(1-2\log3)\beta -2\alpha + (\log3-4)\gamma} \\
&= \sum_{i=0}^{\log n}\sum_{\beta+\gamma\le i}2^{-2i +(3-2\log3)\beta  + (\log3-2)\gamma}
\end{align}
$$
easy to see $3<2\log3$ and $\log3<2$, then
$$
K(n)/n^2 \le \sum_{i=0}^{\log n} i^2 \cdot 2^{-2i} < \infty
$$
Thus

$$
K(n) \in \Theta(n^2)
$$




