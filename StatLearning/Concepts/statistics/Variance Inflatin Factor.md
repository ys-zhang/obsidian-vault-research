Diagonal of $C=(X^TX)^{-1}$.

$$
\begin{align}
VIF_i &= C_{ii} = (X^TX)^{-1}_{ii} \\
&= \frac{1}{1-R_i^2}
\end{align}
$$
where $R^2_i$ is the  [[Coefficient of Variation| coefficient of multiple determination]] obtained from regressing $x_i$ on the other regressor variables.

The length of the normal theory confidence interval on the $j$th regression coefficient may be written as
$$
L_j = (2\sigma\sqrt{VIF_j})t_{n-p-1}(\alpha/2)
$$

VIFs can help identify which regressors are involved in the [[Multicollinearity]].