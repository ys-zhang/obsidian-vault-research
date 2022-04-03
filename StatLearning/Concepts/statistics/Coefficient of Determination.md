
- $SS_T = \sum_i (y^{(i)} - \bar y)^2$: sum of total variance
- $SS_R = \sum_i (\hat y^{(i)} - \bar y)^2$: sum of model variance
- $SS_{Res} = \sum_i (\hat y^{(i)} - y^{(i)})^2$ : sum of model residual / sum of training error
- $SS_T = SS_R + SS_{Res}$, see [[#Geometric Interpretation]].

$$
R^2 = \frac{SS_R}{SS_T}
$$

$$
R_{Adj}^2 = 1-\frac{SS_{Res}/(N-p-1)}{SS_T/(N-1)}
$$

```ad-warning


This describes the power/potential of the model.
Larger $R^2$ indicates better training error, however, _does not neccessary mean a better prediction error_.
```
