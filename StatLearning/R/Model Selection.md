#model-selection  #summary 

# Best Subset Selection
> For theory see [[Best Subset Selection]]

- [Variable Selection Methods (r-project.org)](https://cran.r-project.org/web/packages/olsrr/vignettes/variable_selection.html)
- [[bestsubset-paper.pdf]]

##### Criteria and Idea

>[!Idea]
>**Best subset regression** finds for each $k \in \{0, 1, 2, . . . , p\}$ the **subset of size** $k$ that gives **smallest residual sum of squares** 


For $k = 1 \dots p$ , select $k$ predictors in all $p$ variables that minimize **RSS** 
$$
\min_{\|\beta\|_0\le k} \|Y-X\beta\|^2_2
$$

> the best subset selection problem is nonconvex and is known to be NP-hard.
> An efficient algorithm the _leaps and bounds procedure_ (Furnival and Wilson, 1974)—makes this feasible for $p$ as large as $30$ or $40$.

>[!WARNING]
>It minimizes the RSS i.e., the train error and may not optimal in prediction.
>Maybe it's better to consider cross-validation or AIC, BIC


##### Package

1. [`olsrr`: Tools for Building OLS Regression Models](https://cran.r-project.org/web/packages/olsrr/index.html)

```r
model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)

olsrr::ols_step_best_subset(model)
```
2.  [`LEAP`: Constructing Gene Co-Expression Networks for Single-Cell RNA-Sequencing Data Using Pseudotime Ordering](https://cran.r-project.org/web/packages/LEAP/index.html)

```r
x<-matrix(rnorm(100),ncol=4)

y<-rnorm(25)

leap::leaps(x,y)
```


# Forward(Backward) Stepwise(Stagewise) Selection

1. Forward-stepwise selection starts with the intercept, and then sequentially _adds into the model the predictor_ that most improves the fit. 
2. Backward-stepwise selection starts with the full model, and sequentially _deletes the predictor that has the least impact_ on the fit. The candidate for dropping is the variable with the smallest [[Linear Models#t-Test Individual Coefficient|Z-score]]. 
3. It starts like forward-stepwise regression, with an intercept equal to $\bar y$, and centered predictors with coefficients initially all $0$. At each step the algorithm identifies the variable _most correlated with the current residual_.

![[Pasted image 20220502190053.png]]

##### Package
`step` function in R defined in package `stats` selects a formula-based model by AIC using forward stepwise selection.



# Use Lasso in model selection
##### Packages
1. [lars: Least Angle Regression, Lasso and Forward Stagewise](https://cran.r-project.org/web/packages/lars/index.html)
2. `glmnet`
  - [An Introduction to `glmnet`](https://glmnet.stanford.edu/articles/glmnet.html): Homepage for `glmnet`
  - [`glmnet`: Lasso and Elastic-Net Regularized Generalized Linear Models](https://cran.r-project.org/web/packages/glmnet/index.html)
  - [`glmnetUtils`: Utilities for `glmnet`](https://cran.r-project.org/web/packages/glmnetUtils/index.html):  add formula support to `glmnet`



