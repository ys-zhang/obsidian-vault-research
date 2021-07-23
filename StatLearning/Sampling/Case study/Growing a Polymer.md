# Growing a Polymer

## The self-avoid walk

2-D Lattice 上的不自交随机游走。

## Problem

The most naive way of simulating a SAW is to start a random walk at  
$(0, 0)$, and 
- at each step `i`, the walker, for that he is not allowed to fall back to where it came from at step `i - 1`, chooses with equal probability one of the $3$ allowed neighboring positions to go. 
- If that position has already been visited earlier, the walker has to go back to $(0,0)$ and start a new chain again.
- Otherwise, the walker keeps going until the presumed length $N$ is reached.

>The rate (number of successes over the number of starts) of obtaining a successful SAW of given length $N$ **decreases exponentially** as a function of $N$.

### Target distribution

$$
	\pi(x) = {1 \over Z_N}
$$
where $Z_N$ is the normalizing constant, which is just the total number of different SAWs with $N$ atoms.

## The Growth Method

>Hammersley and Morton (1954) and Rosenbluth and Rosenbluth (1955) introduced essentially identical methods, one termed "inversely restricted sampling" and the other "biased sampling."

> In this book we call their method the growth method since the approach can be intuitively seen as "growing" a polymer one monomer a time.

Let $n_k$ be the number of legal move at the end of $x_{1:k}$. generate next move by

$$
g_t(x_t|x_{1:t-1}) = \frac{1}{n_{t-1}}
$$

>$g_t$ is biased since it gives higher probability to curly and compact sequences.

thus it suggest we set the update weight $u_t = n_{t-1}$

auxiliary distributions $\pi_t(x_{1:t})$:    sequence of uniform distributions of SAWs with $t$ nodes (or $t-1$ steps); furthermore we have
$$
	\pi_t(x_t|x_{1:t-1}) = \frac{1}{n_{t-1}} 
$$
