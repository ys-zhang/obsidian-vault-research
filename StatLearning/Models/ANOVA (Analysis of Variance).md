#experiment-design #statistics  #ANOVA 


Essentially, any analysis-of-variance problem can be treated as a regression problem in which all of the regressors are [[Linear Models#Categorical Data|indicator variables]].


# Single Factor (One-way) ANOVA

> The is a reading note of the _(Chapter 3) Experiments with a Single Factor: The Analysis of Variance_ from Book _Design and Analysis of Experiments (8th edition)_


>[!QUESTION]
> How to **test** the **equality of output means** under **different levels** of **single factor** of treatment?

Suppose
- The factor has $a$ levels: $i \in \{1, 2, \dots, a\}$.
- Each level have $n$ samples $j \in \{1, 2, \dots, n\}$

## Models


###### The Means Model

$$
y_{ij} = \mu_i + \varepsilon_{ij} 
\begin{cases}
i = 1, 2, \cdots, a \\
j = 1, 2, \cdots, n
\end{cases}
$$
where $i$ specifies the level and $j$ specifies the $j$th sample in its level


###### The Effects Model

$$
y_{ij} = \mu + \tau_i + \varepsilon_{ij} 
\begin{cases}
i = 1, 2, \cdots, a \\
j = 1, 2, \cdots, n
\end{cases}
$$

######  Assumptions

- $\varepsilon_{ij}$ i.i.d. follows $\mathscr N(0, \sigma^2)$
- balanced sample size among groups 

> **Completely Randomized Design** is usually adopted for **independence** of the error


## Fixed Effects Model Analysis

> [!WARNING]
> The $a$ treatments could have been specifically _chosen by the experimenter_.
> 
> The conclusions **cannot be extended** to similar treatments that were not explicitly considered. 

**The ANOVA identity**:

$$
\sum_{ij}(y_{ij}- \bar y_{\cdot\cdot})^2 = 
n\sum_i(\bar y_{i\cdot} - \bar y_{\cdot\cdot}) +
\sum_{ij}(y_{ij} - \bar y_{i\cdot})^2 
$$
or $SS_T = SS_R + SS_E$

**Hypothesis**:

$$
\begin{align}
  H_0: & \mu_1 = \mu_2 = \cdots = \mu_a \\
  H_1: & \exists \;i\ne j,\;\;\; \mu_i\ne\mu_j
\end{align}
$$

The _test statistic_ is

$$
F_0 = \frac{MS_R}{MS_E} \sim F(a-1, N-a)
$$
where $N$ is the total sample size.

>[!WARNING]
>the F-test here is robust against normality but **sensitive to outliers**.


## Random Effects Model Analysis

>[!IDEA]
> the _levels of the factor_ actually used in the experiment were chosen **randomly**, inferences are made about **the entire population of factor levels**.

$$
y_{ij} = \mu + \tau_i + \varepsilon_{ij} 
\begin{cases}
i = 1, 2, \cdots, a \\
j = 1, 2, \cdots, n
\end{cases}
$$
where both $\tau_i\sim \mathscr N(0,\sigma_\tau^2)$ and $\varepsilon_{ij} \sim \mathscr N(0,\sigma^2)$ are random variables

We have 
$$
\begin{align}
  \text{Cov}(y_{ij}, y_{ij}) &= \sigma_\tau^2 +\sigma^2 \\
  \text{Cov}(y_{ij}, y_{i\ell}) &=  \sigma_\tau^2  & j\ne \ell \\
  \text{Cov}(y_{ij}, y_{k\ell}) &= 0 &  i\ne k  \\
\end{align}
$$ 

**Hypothesis**:

$$
\begin{align}
  H_0: & \;\; \sigma_\tau^2 =0 \\
  H_1: & \;\; \sigma_\tau^2> 0 
\end{align}
$$




# See Also

- [Analysis of Variance and Covariance - MATLAB & Simulink - MathWorks Australia](https://au.mathworks.com/help/stats/analysis-of-variance-and-covariance.html)
- [[Randomized Complete Block Design]]

