# Generative Adversarial Networks

| Papers                      | Status | Description |
| --------------------------- | ------ | ----------- |
| Generative Adversarial Nets |        | First Paper |
|                             |        |             |



# Generative Adversarial Nets

> a **[[generative model]] $G$** that captures the data distribution
> and a **[[discriminative model]] $D$** that *estimates the probability that a sample came from the training data rather than $G$*

> This framework corresponds to *a minimax two-player game*. In the space of arbitrary functions $G$ and $D$, a unique solution exists, with $G$ recovering the training data distribution and $D$ equal to $1/2$ everywhere.

> The basic idea of GAN network deployment is for unsupervised ML techniques but also proved to be better solutions for semi-supervised and reinforcement learning.

# Comparison of other frameworks
![[Pasted image 20210809231512.png]]


# Adversarial Nets

$G$ and $D$ are both multi-layer perceptrons.

$G$ generates fake inputs
$$
	p_z(z) \overset{G(\cdot|\theta_g)}{\longrightarrow}  x = G(z;\theta_g)
$$

$D$ models the probability of assigning the correct label to both training examples and samples from $G$.

In other words, $D$ and $G$ play the following two-player minimax game with value function $V(G, D)$:

$$
	\min_G\max_D V(D, G) = \min_G\max_D E_{x\sim data}[\log D(x)] + E_{z\sim p_z}[\log(1 - D(G(z)))]
$$

> In practice, we must implement the game using an iterative, numerical approach. Optimizing D to completion in the inner loop of training is computationally prohibitive, and on finite datasets would result in overfitting. **Instead, we alternate between $k$ steps of optimizing $D$ and one step of optimizing $G$.**

![[adversarial-net-algo.png]]

