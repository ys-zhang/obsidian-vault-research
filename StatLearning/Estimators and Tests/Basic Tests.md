#statistics  #hypothesis-test 

# F-Test

[F-test - Wikipedia](https://en.wikipedia.org/wiki/F-test)


# t-Test

[Student's t-test - Wikipedia](https://en.wikipedia.org/wiki/Student%27s_t-test)



# Pearson's Chi-squared Test
https://en.wikipedia.org/wiki/Pearson's_chi-squared_test



# Wilks Test (Likelihood Ratio Test)

[Likelihood-ratio test - Wikipedia](https://en.wikipedia.org/wiki/Likelihood-ratio_test)

Hypothesis:
- $H_0: \theta \in \Theta_0$
- $H_0: \theta \in \Theta \setminus \Theta_0 = \theta_0^c$

Under $H_0$:
$$
\begin{align}
\lambda_{\textrm{LR}} &= 2\ln\frac{\sup_{\theta\in\Theta_0} L(\theta)}{\sup_{\theta\in\Theta} L(\theta)} \\
&= 2[\sup_{\theta\in\Theta_0} \ell(\theta) - \sup_{\theta\in\Theta} \ell(\theta)] \\
&\overset{d}{\longrightarrow} \chi^2
\end{align}
$$


> [!NOTE] Wilks Test
>
> As the sample size $N\to \infty$, the test statistic $\lambda_{\textrm{LR}}$ defined above will be asymptotically chi-squared distributed with **degrees of freedom** equal to the difference in dimensionality of $\Theta$ and $\Theta_0$.


