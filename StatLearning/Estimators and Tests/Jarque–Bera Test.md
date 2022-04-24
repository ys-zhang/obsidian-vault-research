#statistics #hypothesis-test #timeseries #normal-distribution 


$H_0:$ the data is normal


Test whether sample data have the [[Skewness]] and [[Kurtosis]] matching a normal distribution.


$$
\text{JB} = \frac{n}{6} \big( S^2 + \frac{1}{4} (K-3) \big)
$$
where 
- $S$ is the sample skewness
- $K$ is the sample kurtosis


> The JB statistic is **asymptotically** $\chi^2_2$

>[!WARNING]
For **small samples** the $\chi^2$ approximation is **overly sensitive**, often rejecting the null hypothesis when it is true.

>[!CITE]
> Panagiotis Mantalos, 2011, "The three different measures of the sample skewness and kurtosis and the effects to the Jarque-Bera test for normality", International Journal of Computational Economics and Econometrics, Vol. 2, No. 1, [link](http://dx.doi.org/10.1504/IJCEE.2011.040576).


# R

`jarque.test` in the package `moments`


# See Also

- [Jarque–Bera test - Wikipedia](https://en.wikipedia.org/wiki/Jarque%E2%80%93Bera_test)
- [CRAN - Package moments (r-project.org)](https://cran.r-project.org/web/packages/moments/index.html)


