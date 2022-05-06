#statistics #hypothesis-test #summary 


# Test for Homogeneity of Variances


## Hypothesis
$$
\begin{align}
H_0 & : \sigma_1^2 = \sigma_2^2 = \dots = \sigma_k^2\\
\\
H_1 & : \exists\; i\ne j, \; \sigma_i^2 \ne \sigma_j^2    
\end{align}
$$

## Tests

- [[Bartlett Test]]
- [[Levene Test]]



# Independence Tests (White Noise)


## Hypothesis

**$H_0$:** The data are _independently_ distributed (i.e. $\text{correlation} = 0$).
**$H_1$:** The data are _not independently_ distributed; they exhibit serial correlation.


## Tests
- [[Chi-squared Test#Chi-squared Test of Association]]
- [[Ljung-Box Test]]
- [[Box-Pierce Test]]


# Goodness-of-fit Normality Test

- [[Kurtosis]], [[Skewness]]
- [[Jarque–Bera Test]]
- [[Lilliefors Test]]
- QQ Plot 


# References

- [Time series tests (juliastats.org)](https://juliastats.org/HypothesisTests.jl/latest/time_series/)
- [Hypothesis Tests - MATLAB & Simulink - MathWorks Australia](https://au.mathworks.com/help/stats/hypothesis-tests-1.html?s_tid=CRUX_lftnav)
  - [Statistics and Machine Learning Toolbox Documentation - MathWorks Australia](https://au.mathworks.com/help/stats/index.html)
