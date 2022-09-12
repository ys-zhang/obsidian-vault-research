A Markov decision process (MDP) extends a [[Markov Chain]] with non-determinism.

In MDP, each state $s$ is associated with a set of transition distributions 
$$
\mathcal D(s) =\big \{\mu: \mu = \Pr(s, \cdot)\big \}
$$
On reaching a state $s$ in an MDP, non-deterministically a distribution $\mu\in \mathcal D(s)$ is selected by a _policy_. 
The next state is then determined according to $\mu$. That is, state $s'$ is selected with probability $\mu(s,s')$.

# Policy

- _Positional policies_ decide solely on the current state $s_i$ and not on the history, i.e., the prefix of the path until reaching $s_i$. 
- _Randomised positional policies_ select $\mu\in \mathcal D(s_i)$ with a certain probability. 
- _Deterministic policies_  select a fixed distribution from $\mathcal D(s_i)$. 
- _History-dependent policies_ base their decision on the prefix $s_0µ_0 \dots s_{i-1}µ_{i−1}s_{i}$. 
 
A policy imposed on an MDP yields an (possibly infinite-state) MC.

