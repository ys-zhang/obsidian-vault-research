#statistics

>[!INFO]
> Kurtosis is a measure of the **tailedness** of the probability.
> 
> It is also a hint of difference with normal distribution ($\kappa=3$).


# Kurtosis


$$
\text{Kurt}[X] = E\big[\;(\frac{X-\mu}{\sigma})^4 \;\big]
$$

**Kurtosis** is a scaled version of the fourth **moment** of the distribution.

$$
\begin{align}
\kappa &= \frac{\mu_4}{\sigma^4} = \frac{\mu_4}{\mu_2^2} \\
 &\ge \big( \frac{\mu_3}{\sigma^3} \big)^2 +1
\end{align}
$$


# Interpretation


##### Pearson


Kurtosis is the average of the standardized data raised to the fourth power. 

- Standardized values that are less than 1 (i.e., data within one standard deviation of the mean, where the "peak" would be) contribute virtually nothing to kurtosis, since raising a number that is less than 1 to the fourth power makes it closer to zero. 
- The only data values (observed or observable) that contribute to kurtosis in any meaningful way are those outside the region of the peak; i.e., the outliers. 

**Therefore, kurtosis measures outliers only; it measures nothing about the "peak".**


##### Moors

$$
\kappa = \text{Var}[Z^2] + 1 
$$
where $Z=(X-\mu)/\sigma$ 

- The kurtosis can now be seen as a measure of the dispersion of $Z^2$ around its expectation.
- it can be seen to be a measure of the dispersion of $Z$ around $+1$ and $−1$.


**High values** of $\kappa$ arise in **two circumstances**:

- where the probability mass is **concentrated around the mean** and the data-generating process **produces occasional values far from the mean**,
- where the probability mass is **concentrated in the tails of the distribution**.

