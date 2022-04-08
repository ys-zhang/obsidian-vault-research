Levene's test ( [Levene 1960](https://www.itl.nist.gov/div898/handbook/eda/section4/eda43.htm#Levene)) is used to test if _k_ samples have equal variances. Equal variances across samples is called homogeneity of variance.
Some statistical tests, for example the analysis of variance, assume that variances are equal across groups or samples. The Levene test can be used to verify that assumption.

Levene's test is an alternative to the [[Bartlett Test]]. 

**The Levene test is less sensitive than the Bartlett test to departures from normality.**

If you have strong evidence that your data do in fact come from a normal, or nearly normal, distribution, then Bartlett's test has better performance.

$$
\begin{align}
H_0 & : \sigma_1^2 = \sigma_2^2 = ... = \sigma_k^2\\
H_1 & : \exists i\ne j, \; σ_i^2 \ne σ_j^2    
\end{align}
$$




