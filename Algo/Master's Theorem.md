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