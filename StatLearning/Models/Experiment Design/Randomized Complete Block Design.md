#statistics #experiment-desgin  #ANOVA
#todo 

# Single Factor

suppose we have a single factor with $a$ levels and $b$ blocks.

- **Completeness** means that _all levels_ are run in _each block_ for the _same number_ of experiments.
- **Randomness** means the _order of the experiment_ are chosen randomly.

###### The effects model:

$$
y_{ij} = \mu +\tau_i + \beta_j + \varepsilon_{ij} \;\;\;\;\;
\begin{cases}
i &=& 1, 2, \;\dots,\; a\\
j &=& 1, 2, \;\dots,\; b
\end{cases}
$$
- $\tau_i$ is the effect of $i$th _treatment_, $\sum \tau_i =0$
- $\beta_j$ is the effect of $j$th _block_, $\sum \beta_j = 0$


>[!NOTE]
> It is always true that $y_{ij} = \mu + \tau_{ij} + \varepsilon_{ij}$, Randomization makes $\tau_{ij}$ to be separated in to 2 variables, but not guarantee to be additive.


**Assumption**:

- additive of effect of treatments and effect of blocks, there may be interaction terms



###### ANOVA Equation

$$
\begin{align}
SS_T &= \sum_{i=1}^a\sum_{j=1}^b (y_{ij}-\bar y_{\cdot \cdot})^2 \\ 
SS_{\text{Treatments}} &= b\sum_{i=1}^a (\bar y_{i\cdot}-\bar y_{\cdot \cdot})^2 \\
SS_{\text{Blocks}} &= a\sum_{j=1}^b (\bar y_{\cdot j}-\bar y_{\cdot \cdot})^2 \\
SS_E &= \sum_{i=1}^a\sum_{j=1}^b (y_{ij} - \bar y_{i\cdot} - \bar y_{\cdot j} + \bar y_{\cdot \cdot})^2 \\ \\

SS_T &= SS_{\text{Treatments}} + SS_{\text{Blocks}} + SS_E

\end{align}
$$


To test $\tau_i=0$
let the test statistic

$$
F_0 = \frac{MS_{\text{Treatments}}}{MS_E} \sim F\big(a-1, (a-1)(b-1)\big )
$$

>[!NOTE]
> The distribution of $F_0$ is $F$ is justified by the randomization within each block, which does not requite the errors to be i.i.d. normal.


