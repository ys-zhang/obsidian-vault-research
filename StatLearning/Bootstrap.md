#statistics #nonparametric-statistics 

- ESL Chapter 8


# Idea

In essence the bootstrap is a computer implementation of _nonparametric or parametric maximum likelihood_.

##### Properties of MLE and Likelihood
1. Suppose **non-informative** prior;
2. **Sufficient**, the likelihood depend on the samples only through the MLE, i.e. $\ell(\theta|\hat\theta)$ is independent of the samples given the value of $\hat\theta$.
3. **Symmetry**, the log-likelihood is symmetric in $\theta$ and $\hat\theta$. 
$$
\ell(\theta|\hat\theta) = \ell(\hat\theta|\theta)
$$

>[!NOTE] Link with Bayes Inference
>
>The bootstrap distribution of $T(\hat\theta^*)$ will converge to the _posterior distribution_ of $T(\theta)$ when a _non-informative prior_ is used  




