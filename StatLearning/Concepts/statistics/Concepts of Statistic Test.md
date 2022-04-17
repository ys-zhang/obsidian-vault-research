#statistics  #hypothesis-test

Some concepts of Hypothesis Tests

# Power Function

The **power function** of a hypothesis test with rejection region (critical region) $R$ is the function of parameter $\theta$  defined by
$$
\beta(\theta) = \Pr(X\in R\;|\;\theta)
$$
**The probability to reject the Null Hypothesis** 

>[!NOTE]
>A Ideal Test would have $\beta(\theta)=0$ on $\Theta_0$ and $\beta(\theta)=1$ on $\Theta_0^c$


# Level & Size of a Test

1. For $0<\alpha<1$, a test with power function $\beta(\theta)$ is a **size $\alpha$ test** if 
$$
 \sup_{\theta\in \Theta_0} \beta(\theta) = \alpha 
$$
2. For $0<\alpha<1$, a test with power function $\beta(\theta)$ is a **level $\alpha$ test** if 
$$
   \sup_{\theta\in \Theta_0} \beta(\theta) \le \alpha 
$$

>[!NOTE]
> **Size** and **Level** is the probability of Type I Error, i.e., about how good **Type I Error** is Controlled

>[!IDEA] Choose Level of Tests
> 
> We can use decision theory to choose the level of a test.
>
>Given a **Risk function** of Type I Error and Type II Error. we can choose the size $\alpha$ by minimizing the risk/loss function.



# Unbiased Test

A Test with power function $\beta(\theta)$ is **unbiased** if 
$$
\inf_{\theta\in\Theta_0^c} \beta(\theta) \ge \sup_{\theta\in\Theta_0} \beta(\theta)
$$

>[!NOTE]
> Null Hypothesis is more likely to be rejected if alternative is true.


# UMP (Uniformly Most Powerful Test)

Given a class of tests $\mathscr C$. A test in $\mathscr C$ is a **uniformly most powerful** test if 
for all $\theta \in \Theta_0^c$ and $\beta'$
$$
\beta(\theta) > \beta'(\theta)
$$
i.e., _uniformly smallest Type II Error_.

If $\mathscr C$ is all level $\alpha$ tests, then the UMP is a UMP level $\alpha$ test.

see:
- [[Neyman-Pearson Lemma]]


# MLR (Monotone Likelihood Ratio)

Let $\{g(t|\theta): \theta\in\Theta \}$ be a family of distribution of a univariate random variable $T$. 

If $\forall\; \theta_1 < \theta_2$:

$$
g(t|\theta_2) \over g(t|\theta_1)
$$
is **monotone** in $t$ on 
$$
\{ g(t|\theta_2) > 0 \} \; \bigcup \; \{ g(t|\theta_1) > 0 \}
$$
then the _family of distribution_ is said to have a **monotone likelihood ratio**.

>[!NOTE]
> monotone likelihood ratio assures test of if $\theta \le \theta_0$'s  UMP is in form of 
> $T>t_0$. see [[Karlin-Rubin Theorem]]



