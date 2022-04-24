#statistics  #hypothesis-test 


>[!WARNING]
>Bartlett’s test is sensitive to the normality assumption


## Bartlett's Test

_Purpose:  Test for Homogeneity of Variances_

Bartlett's test ([Snedecor and Cochran, 1983](https://www.itl.nist.gov/div898/handbook/eda/section4/eda43.htm#Snedecor)) is used to test if _k_ samples have equal variances. Equal variances across samples is called homogeneity of variances. Some statistical tests, for example the analysis of variance, assume that variances are equal across groups or samples. The Bartlett test can be used to verify that assumption.

Bartlett's test is **sensitive to departures from normality**. That is, if your samples come from non-normal distributions, then **Bartlett's test may simply be testing for non-normality**.

The [Levene test](https://www.itl.nist.gov/div898/handbook/eda/section3/eda35a.htm) is an alternative to the Bartlett test that is less sensitive to departures from normality.

# Definition

The Bartlett test is defined as:

$$
\begin{align}
H_0 & : \sigma_1^2 = \sigma_2^2 = ... = \sigma_k^2\\
H_1 & : \exists i\ne j, \; σ_i^2 \ne σ_j^2    
\end{align}
$$

Test Statistic:

$$
T=\frac{(N−k)\ln S^2_p−\sum^k_{i=1}(N_i−1)\ln S^2_i}
{1+\frac{1}{3(k−1)}(\sum^k_{i=1}\frac{1}{N_i−1}−\frac{1}{N−k})}
\sim \chi_{k-1}
$$

The Bartlett test statistic is designed to test for equality of variances across groups against the alternative that variances are unequal for at least two groups.

In the above, $S_i^2$ is the variance of the $i$th group, $N$ is the total sample size, $N_i$ is the sample size of the $i$th group, $k$ is the number of groups, and $S_p^2$ is the **pooled variance**. 

The **pooled variance** is a _weighted average of the group variances_ and is defined as:

$$
S^2_p=\frac{1}{N-k}\sum_{i=1}^k(N_i−1)S^2_i
$$

# Question

Bartlett's test can be used to answer the following question:

-   Is the assumption of equal variances valid?

# Importance

Bartlett's test is useful whenever the assumption of equal variances is made. In particular, this assumption is made for the frequently used one-way analysis of variance. In this case, Bartlett's or [[Levene Test]] should be applied to verify the assumption.

_Software_

```r
## Read data and save batch variable as a factor.
m <- matrix(scan("GEAR.DAT",skip=25),ncol=2,byrow=T)
diameter = m[,1]
batch = as.factor(m[,2])

## Run Bartlett's test.
bartlett.test(diameter~batch)

>         Bartlett test of homogeneity of variances

> data:  diameter by batch 
> Bartlett's K-squared = 20.7859, df = 9, p-value = 0.01364

## Find critical value.
> qchisq(.95,9)

> [1] 16.91898
```