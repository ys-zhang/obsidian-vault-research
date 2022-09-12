# The Model

Given a statistic process 
$$
X = (X_1, \cdots, X_{k-1}, X_k, X_{k+1}, \cdots)
$$

a trace

$$
x = (x_1, \cdots, x_{k-1}, x_k, x_{k+1}, \cdots)
$$

## Markov Property
$$
	\pi(x_k|x_{1:k-1}) = \pi(x_k|x_{k-1})
$$

![[mc-dp.excalidraw]]

Thus we have the factorization

$$
\pi(x_{1:k}) = \pi(x_1)\pi(x_2|x_1) \cdots\pi(x_k|x_{k-1})
$$

or equivalently

$$
\pi(x_{1:k}) \propto \exp\{ \sum_1^k h_i(x_{i-1}, x_{i}) \}
$$

### Time homogeneous
the probability 
$$
\Pr(X_t=s'|X_{t-1}=s)
$$
is independent of $t$

## State Properties

### Recurrent states
Let $R_s$ be r.v. that denotes start from state `s` the first time back to `s`.
The probability of returning back to state `s` for the first time after exactly `n` steps is defined as:
$$
	f_s(n)=\Pr(R_s=n) = \Pr(X_n=s,X_{n-1}\neq s,\dots,X_1\neq s | X_0=s)
$$

The probability to ***eventually return*** to state `s` is:
$$
	f_s = \Pr (R_s < \infty ) =\sum_{n=1}^\infty f_s(n)
$$

- **transient**: $\Pr (R_s < \infty ) <1$
- **recurrent**: $\Pr (R_s < \infty ) =1$
	- **positive recurrent** $E[R_s] < \infty$
	- **null recurrent** $E[R_s] = \infty$
- **Absorb**: $P(s, s)=1$ or equivalently $R_s=1$
-  **Irreducibility**:    A MC is called irreducible if from each state `s`, one can *reach any other state in a finite number of steps*.

In a finite state MC, **irreducibility** is equal to the requirement that all states are **recurrent**.

### Period

A state `s` is **periodic** if
$$
   \exists \; d > 1 : (\forall n : n \mod d \neq 0 )
$$

A state `s` is called ***ergodic*** if it is **aperiodic** and** positive recurrent**.

### Stationary Distribution

- irreducible, aperiodic and positive recurrent then
	- Exists *unique* station distribution.
	- Limit distribution converge to stationary distribution.
- irreducible and positive recurrent then Exists *unique* station distribution.

# Dynamic programming

[[Dynamic programming]] is highly related to the Markov property.

Consider the problem

$$
\begin{align}
\min_{x_{1:n}} \pi(x_{1:k})
\end{align}
$$

state transmission formula:
$$
\min_{x_{1:s-1}} \pi_s(x_{1:s-1}, x_s) = \min_{x_{s-1}} \{ \pi_s(x_s|x_{s-1}) \min_{x_{1:s-2}}[\pi_{s-1}(x_{1:s-2}, x_{s-1})] \}
$$

# Other variants

![[DTMC (discrete-time Markov chain)]]

![[CTMC (continues-time markov chain)]]


