# Problem

consider the DGF ([[Directed greybox fuzzing]]) problem.
After a seed is chosen and mutations are performed we get the generated inputs.

however the hitting rate of the generated input set is low

$$
	P(i) = \Pr(T\cap path_i | \theta) << 1
$$
where $T$ is the set of target basic blocks, and $\theta$ is the seed, i.e. $i \sim \pi(\theta)$.

	P tries to assess on the input not the seed.

if we can estimate the probability $P(i)$ we can increase the efficiency by rejecting inputs which has a low $P(i)$.
 

>    The basic idea of FuzzGuard is to predict whether a program can execute to the target buggy code with a newly generated input by learning from previous executions.


>    The basic idea is based on the observation: the pre-dominating nodes of the buggy code are earlier to be reached, which should gain balanced data earlier.  
>    Note that the **pre-dominating nodes** of the buggy code are the nodes that dominate the buggy code: *every execution path towards the buggy code will pass through the pre-dominating nodes*.

## Formalization of the approximation problem

**response**:

$$ y^i = (y^i_1, \cdots,y^i_m) $$

where $y^i_k$ is whether (or the probability that) path $i$ enters node $k$.

**input**:

$x=\langle x_1,\cdots, x_n \rangle$ where
$$
	x_k = \begin{cases}
		byte_i + 1 &\quad \\
		0  & \quad \textrm{no byte at the pos}
	\end{cases}
$$
