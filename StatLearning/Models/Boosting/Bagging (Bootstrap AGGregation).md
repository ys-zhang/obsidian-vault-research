#statistics #ensemble-learning  #model-averaging

https://en.wikipedia.org/wiki/Bootstrap_aggregating


# Bagging (Bootstrap AGGregation)

>[!QUESTION] 
>How to use the [[Bootstrap]] to improve the _estimator_ or _prediction_?

>[!TLDR]
>Bootstrap values of an estimator are approximate posterior values of a corresponding parameter, from a kind of nonparametric Bayesian analysis.
>
>The Bagging estimator approximates the posterior mean. 


## Regression

Let
- $Z = \{(x_i,y_i)\}_{i=1}^N$ be the _samples_;
- $Z^{*b}$ is the $b$th _bootstrap samples_, which is sampled from $Z$ **with replacement**;
- $\hat f^{*b}(x)$ is the $b$th _bootstrap prediction_;


##### The Bagging Estimator

$$
\hat f_{\text{bag}}(x) = 
E_{\hat {\mathcal P}} f^*(x) 
$$
where 
- $Z^* = \{(x_i^*, y_i^*)\}_{i=1}^N$ 
- and $(x_i^*, y_i^*)\sim \hat {\mathcal P}$  

Usually a _Monte Carlo_ approximation is adopted

$$
\hat f_{\text{bag}}(x) = 
\frac{1}{B} \sum_{b=1}^B \hat f^{*b}(x)
$$
>[!WARNING]
>the bagging estimator with not differ from the vanilla version if the original estimator is linear in $X$

It can be applied to 
- [[Tree Models#Regression Tree]]


# Model Averaging and Stacking

Let
- $\mathcal M_i$ be a sequence of _candidate models_;
- $\zeta$ is the target we want to estimate;
- $Z$ is the observation

The posterior probability of $\zeta$:
$$
\Pr(\zeta|Z) = \sum_{m=1}^M \Pr(\zeta|M_m,Z) \Pr(M_m|Z)
$$
and the posterior mean:
$$
E[\zeta|Z] = \sum_{m=1}^M E[\zeta|M_m,Z]\Pr(M_m|Z)
$$
The posterior candidate model probability

$$
\begin{align}
\Pr(M_m|Z) &\propto \Pr(M_m) \Pr(Z|M_m) \\
&\propto \Pr(M_m) \int\Pr(Z|\theta_m,M_m)\Pr(\theta_m|M_m)\;d\theta_m
\end{align}
$$
- The BIC can be used to approximate $\Pr(Z|M_m)$.
- A more appropriate way is to specify the model parameter prior $Pr(\theta_m|M_m)$ 


