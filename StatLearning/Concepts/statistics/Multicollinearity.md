#statistics #linear-regression

>[!TLDR]
>linearity of columns, near-linear dependence among the regression variables.

 
> Generally, the covariance of least square estimator $\hat \beta_i$ and $\hat \beta_j$ will also be large if the regressors $x_i$ and $x_j$ are involved in a multicollinear relationship.


# Sources of Multicollinearity

1. The **data collection method** employed;
2. **Constraints** on the model or in the population; 
3. Model specification;
4. An over-defined model.

The **data collection method** can lead to multicollinearity problems when the analyst samples _only a subspace of the region_ of the regressors defined.

**Constraints** on the model or in the population being sampled can cause multicollinearity. For example, some physical rules governing the system may force some of the variable highly correlated.

Multicollinearity may also be induced by the **choice of model**. For example, adding polynomial terms to a regression model causes ill-conditioning in $X^TX$, where $X$ is the [[Design Matrix]].

# Mitigate the Problem
- use [[Principle Component Analysis]] on the regressors
- use [[Shrinkage Methods]].


# R code

```
model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_coll_diag(model)
```

```r
package(car); package(perturb)

deliver.model <- lm(time∼cases+dist, data=deliver) 
print(vif(deliver.model)) 
print(colldiag(deliver.model))
```

