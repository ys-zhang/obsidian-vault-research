
因子分析

> As a basic [[density model]], Factor analysis (FA) is commonly used to define a appropriate density distribution of data to promote the well-known mixtures of Gaussians (Everitt 1984).

[因子分析 - 简书 (jianshu.com)](https://www.jianshu.com/p/2456837720fb)

# Problem
$X=(X_1, \cdots, X_p)^T$ is the manifest variable, i.e. observable.
$F=(F_1, \cdots, F_q)^T$ is the latent variables, i.e. factors.

The models states as 
$$
	X = AF +\mu + \epsilon 
$$
其中 $F$是公共因子，$A$是载荷因子，且
- $F$ 与 $\epsilon$ not correlated.
$$
	cov(F,\epsilon) = 0
$$
- $F_i$ 互不相关 且方差为$1$
$$
	\Sigma_F=I_q
$$
- $\epsilon_i$互相独立
$$
	\Sigma_\epsilon = diag(\sigma_1^2,\cdots, \sigma_p^2)
$$