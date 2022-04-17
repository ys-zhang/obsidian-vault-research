# Modelling Process
![[Pasted image 20220321215939.png]]


# Models

The linear model:

$$
  E[y|x] = f(x) = \beta_0 + \sum_{i=1}^p\beta_ix_i = x^T\beta
$$
Where:

- Input vector $x = (1, x_1, \dots, x_p)^T$ 
- Coefficients: $\beta = (\beta_0, \beta_1,\dots,\beta_p)^T$


## Terminology 


### Design Matrix 

![[Design Matrix]]


### Covariance Matrix

Let $x = (x_1,\dots,x_p)^T$ and $X=[x^{(1)}, \dots, x^{(N)}]^T$, which is exactly the **standardised** [[#Design Matrix]] drop the first column.

Then
$$
  \mathbf{Cov}(x_j, \; x_k) \sim C_{ij}= \sum_ix^{(i)}_jx^{(i)}_k
$$
Easy to see that 

$$
C = X^TX = 
\begin{bmatrix}
   \sum x^{(i)}_1x^{(i)}_1 & \sum x^{(i)}_1x^{(i)}_2 & \cdots & \sum x^{(i)}_1x^{(i)}_p \\
   \sum x^{(i)}_2x^{(i)}_1 & \sum x^{(i)}_2x^{(i)}_2 & \cdots & \sum x^{(i)}_2x^{(i)}_p \\
   \vdots & \vdots & \ddots & \vdots \\
   \sum x^{(i)}_px^{(i)}_1 & \sum x^{(i)}_px^{(i)}_2 & \cdots & \sum x^{(i)}_px^{(i)}_p \\
\end{bmatrix}
$$


### Hat Matrix

$$
\hat Y = H Y
$$
where
$$
H = X(X^TX)^{-1}X^T
$$

#### Projection

We have the property:

$$
X^T(I-H) = 0
$$
Let $X \in \mathbb R^{N\times p}$ be  some matrix whose column space is rank $p$.

We would like to project a vector $y\in \mathbb R^N$ to the column space of $X$.

It is easy to see that $H$ is the projection matrix.



### R Square (coefficient of determination)

![[Coefficient of Determination]]


### Influence & Leverage Points

[Measures of Influence in R](https://cran.r-project.org/web/packages/olsrr/vignettes/influence_measures.html)

![[Pasted image 20220327222626.png]]

**Leverage point** does not affect the estimates of the regression coefficients, but it certainly will _have a dramatic effect on the model summary statistics_ such as [[#R Square]] and the standard errors of the regression coefficients.

**Influence Points**, on the other hand, do have a dramatic effect on regression coefficient estimation.


#### Identify through position in design space

The elements $h_{ij}$ of the [[#Hat Matrix]] may be interpreted as the amount of leverage exerted by observation $y^{(i)}$  to fitted value $\hat y^{(j)}$.

The [[#Hat Matrix]] diagonal is a standardized measure of the distance of the $i$th observation from the centre (or centroid) of the $x$ space.

Traditionally assume that any observation for which the hat diagonal $h_{ii}$ _exceeds twice the average trace_ is remote enough from the rest of the data to be considered a leverage point.

$$
trace(H) = rank(H) = 1+p
$$
#### Cook's D
$$
D_i = \frac{(\hat \beta^{(i)} - \hat \beta)^T X^TX (\hat \beta^{(i)} - \hat \beta) }{(1+p)MS_{Res}} \sim F(1+p, n-p-1)
$$
**Cook's D** depend on both the _location of the point in the design space_ and the _response variable in measuring influence_.


#### `DFBeta` & `DFFit`
these are difference of leave one out estimate of $\beta$ and $\hat y$

$$
\begin{align}
DFBetaS_{ij} &= \frac{\hat \beta_j - \hat \beta_j^{(i)}}{\sqrt{S^2_{(i)}(X^TX)^{-1}_{jj}}} \\
DFFitS_i &= \frac{\hat y_i - \hat y_i^{(i)}}{\sqrt{S^2_{(i)}h_{ii}}}
\end{align}
$$

```r
model = lm(...)
dfbeta(model)
```


## Computation Issues

### Data Scaling

Many multiple regression computer programs use this scaling to **reduce problems arising from round-off errors** faced when computing $(X^TX)^{-1}$.

![[Data Scaling]]


### Multicollinearity 

![[Multicollinearity]]



## Ordinary Linear Regression Model

The model is _single dimension response_ model, which includes the **OLS (ordinary least square)** estimator and the **MLE (maximum likelihood estimator)**.


### Assumptions

1. The relationship between the response $y$ and the regressors is linear, at least approximately.
2. The error term ε has zero mean. 
3. The error term ε has constant variance $\sigma^2$ . 
4. The errors are uncorrelated.
5. The errors are normally distributed.



### Least square estimator

Thus the [[RSS (residual sum of squares)]] is

$$
\begin{align}
RSS &= (y - \hat{y})^T(y - \hat{y}) = \sum_i (y_i - \beta_0 -\sum_j x_{ij}\beta_j)^2 \\
 &= (y-X\beta)^T(y-X\beta)\\
 &= y^Ty - 2y^TX\beta + \beta^TX^TX\beta
\end{align}
$$


#### Estimate Coefficients


Take the derivative against $\beta$ :

$$
\begin{align}
\frac{\partial RSS}{\partial \beta} 
&= \frac{ \partial (\beta^TX^TX\beta - 2y^TX\beta) }{\partial \beta} \\
&= 2X^TX\beta - 2X^Ty
\end{align}
$$

Thus the estimator is
$$
\begin{align}
\hat \beta &= (X^TX)^{-1}X^Ty \\
\hat y &= Hy = X(X^TX)^{-1}X^Ty \\
e &= y - \hat y = (I-H)y
\end{align}
$$
where $H$ is the [[#Hat Matrix]].


$$
\begin{align}
\hat \beta &= (X^TX)^{-1}X^Ty  \\
&= (X^TX)^{-1}X^T(X\beta +\varepsilon)\\
&= \beta + (X^TX)^{-1}X^T\varepsilon \\
\end{align}
$$
Thus 

$$
\hat \beta \sim N(\beta, \;\sigma^2 (X^TX)^{-1})
$$


#### Estimate error variance

The **residual sum of squares**:

$$
\begin{align}
SS_{Res} &= e^Te \\
&= (y-X\hat\beta)^T(y-X\hat\beta) \\
&= y^Ty - 2y^TX\hat\beta + \hat\beta^T(X^TX\hat\beta) \\
&= y^Ty - 2y^TX\hat\beta + \hat\beta^TX^Ty \\
&= y^Ty - \hat\beta^TX^Ty\\
&= y^T(I-H)y \\
&\sim \sigma^2\chi^2_{n-p}
\end{align}
$$

and

$$
\hat\sigma^2 = MS_{Res} = \frac{SS_{Res}}{n-p}  
$$

> [!WARNING]
> This estimator of $\sigma^2$ requires the error $e$ are _normal_ and _i.i.d._ 



### Geometric Interpretation


![[Pasted image 20220322205606.png]]

$$
X^T(y-\hat y) = X^T(I-H)y = 0\cdot y = 0
$$
Since its projection we have the decomposition of variance:

$$
\begin{align}
\|y\|^2 &= \|\hat y\|^2 + \|y-\hat y\|^2 \\
\|y - \bar y\|^2 &= \|\hat y - \bar y \|^2 + \|y-\hat y\|^2 \\
SS_T &= SS_R + SS_{Res}
\end{align}
$$

where 
- $SS_T$: sum of total variance
- $SS_R$: sum of model variance
- $SS_{Res}$ : sum of model residual / sum of training error



### Testing

#### F-Test (Test for Model Significance)

$$
\begin{align}
H_0&: \beta_1 = \beta_2 = \cdots =\beta_p =0 \\
H_1&: \exists i\ge 1, \; \beta_i \ne 0 
\end{align}
$$
Under $H_0$:

$$
F_0 = \frac{MS_R}{MS_{Res}} = \frac{SS_R/p}{SS_{Res}/(n-p-1)}
\sim F(p, n-p-1)
$$
#### t-Test (Individual Coefficient)

$$
\begin{align}
H_0&: \beta_i=0 \\
H_1&: \beta_i\ne 0
\end{align}
$$



$$
\begin{align}
t_0 &= \frac{\hat \beta_i}{se(\hat \beta_i)}
= \frac{\hat \beta_i}{\sqrt{\hat \sigma^2 (X^TX)^{-1}_{ii}}} \\
&\sim t(N-p-1)
\end{align}
$$

$t_0$ is named as the **Z-score** or **standardized coefficient**. 
A _Z-score_ greater than $2$ in absolute value is approximately significant at the $5\%$ level.

> [!NOTE]
> - If $H_0$ is not rejected, it indicates that regressor $x_i$ can be removed from the model.
> - This is a test of the contribution of $x_i$ _given other regressors_ in the model .



#### Extra-sum-of-squares (Group of Coefficients)

Let 
$$
\beta = 
\begin{bmatrix}
\beta_1  \\
\beta_2
\end{bmatrix}
$$
Where 
$$
\begin{align}
\beta_1 &\in \mathbb R^{p+1-r} \\
\beta_2 &\in \mathbb R^{r} \\
\end{align}
$$

Hypothesis:

$$
\begin{align}
H_0&: \beta_2=0 \\
H_1&: \beta_2\ne 0
\end{align}
$$

Test Statistic:
$$
\begin{align}
F_0 &= \frac{MS_R(\beta_2|\beta_1)}{MS_{Res}} \\
&= \frac{SS_R(\beta_2|\beta_1)/r}{SS_{Res}/(n-p-1)}\\
&= \frac{(SS_R(\beta)-SS_R(\beta_1))/r}{SS_{Res}/(n-p-1)}\\
&\sim F(r, N-p-1)
\end{align}
$$

Since
$$
SS_R(\beta_2|\beta_1) = SS_R(\beta)-SS_R(\beta_1)
$$


### Model Adequacies (Assumption Verification)


#### Outlier 

1. use student-t test on training error, outliers usually have a large T value.
2. consider **press residuals** which is the prediction error with model trained with the predicted sample removed, i.e., the **leave one out estimator**s. High **press residual** indicate high influence points which may be outliers.

$$
\tilde e^{(i)} = y_i - \hat y^{(i)}
= \frac{e_i}{1-H_{ii}}
$$
where $H$ is the [[#Hat Matrix]].

#### Normal Distribution Test

1. Using [QQ-plot](https://data.library.virginia.edu/understanding-q-q-plots/) of studentized residuals.
2. Plot of _Residuals_ (preferably the  externally studentized residuals $t^{(i)}$) against the _Fitted Values_ $\hat y^{(i)}$,  useful for detecting several common types of model inadequacies.
3. Plot of Residuals against the Regressor

![[Pasted image 20220323234217.png]]

#### Lack of Fit test 

The formal statistical test for the lack of fit of a regression model assumes that the normality, independence, and constant - variance requirements are met and that **only the first order or straight - line character of the relationship is in doubt.**


> [!NOTE]  Idea
>
> sum square of residual = sum square of pure error + sum square of Lack of Fit
> 
> The _pure error_ can be estimated using _multiple observations_ at _same design point_



$$
\begin{align}
SS_{Res} &= SS_{PE} + SS_{LoF} \\
SS_{PE} &= \sum_{ij} (y_{ij} - \bar y_i)^2 \\
SS_{LoF} &= \sum_{ij} (\hat y_i - \bar y_i)^2 
\end{align}
$$


#### Transform data for adapting to OLS

![[Transformation#Basic Data Transformation]]


## Generalized Least Square Model

$$
y = X\beta + \varepsilon
$$
with $var(\varepsilon) = \sigma^2 V$ with _covariance matrix_ of the response samples $V$ **known**

> [!NOTE]  weighted least squares
>
> special case of $V$ is diagonal


## Linear Mixed Effects Model

The key points of this model is to _focus on subjects and not individual observation of each subject_. 

In OLS model, coefficients are fixed/constant for all samples. 
We release the restriction by make coefficient be variable among different group, and constant in each single group.

- [[A tutorial on linear mixed effect model-1.pdf]]
- [[A tutorial on linear mixed effect model-2.pdf]]
- [Introduction to linear mixed models (ourcodingclub.github.io)](https://ourcodingclub.github.io/tutorials/mixed-models/)
- [Introduction to Linear Mixed Models (ucla.edu)](https://stats.oarc.ucla.edu/other/mult-pkg/introduction-to-linear-mixed-models/)


The core of mixed models is that they incorporate _fixed and random effects_. (from frequentist's  view)
- A **fixed effect** $\beta$ is a parameter that does not vary.
- **Random effects** are parameters that are themselves random variables.

> [!NOTE]  what does random mean here?
> 
> Basically, the value of fixed effect are known precisely or picked up during experiment desing.
>
> Random effects are observed during the experiment and constant given the subject/group.
> 
> Example:
> 
> - fixed effects:
>    - age, sex (known)
>    - red blood cell count, white blood cell count (precisely observed)
> - random effects:
>   - factors related to groups/subjects
>   - doctor's income ...


> [!NOTE]  Should my variables be fixed or random effects?
> 
> In broad terms, **fixed effects** are variables that we expect will have an effect on the dependent/response variable: they’re what you call **explanatory** variables in a standard linear regression. In our case, we are interested in making conclusions about how dragon body length impacts the dragon’s test score. So body length is a fixed effect and test score is the dependent variable.
> 
> On the other hand, **random effects** are usually **grouping factors** for which we are trying to control. They are always categorical, as you can’t force R to treat a continuous variable as a random effect. A lot of the time we are not specifically interested in their impact on the response variable, but we know that they might be influencing the patterns we see.
> 
> Additionally, the data for our random effect is just **a sample of all the possibilities**: with unlimited time and funding we might have sampled every mountain where dragons live, every school in the country, every chocolate in the box), but we usually tend to generalise results to a whole population based on representative sampling. We don’t care about estimating how much better pupils in school A have done compared to pupils in school B, but we know that their respective teachers might be a reason why their scores would be different, and we’d like to know how much _variation_ is attributable to this when we predict scores for pupils in school Z.


> [!WARNING]  More about random effects
> 
> Note that the golden rule is that you generally want your random effect to have **at least five levels**. So, for instance, if we wanted to control for the effects of dragon’s sex on intelligence, we would fit sex (a two level factor: male or female) **as a fixed, not random, effect**.
> 
> This is, put simply, because estimating variance on few data points is very imprecise. Mathematically you _could_, but you wouldn’t have a lot of confidence in it. If you only have two or three levels, the model will struggle to partition the variance - it _will_ give you an output, but not necessarily one you can trust.
> 
> Finally, keep in mind that the name _random_ doesn’t have much to do with _mathematical randomness_. Yes, it’s confusing. Just think about them as the _grouping_ variables for now. Strictly speaking it’s all about making our models representative of our questions **and getting better estimates**. Hopefully, our next few examples will help you make sense of how and why they’re used.




$$
\overbrace{\mathbf{y}}^{N \times 1} \quad = \quad
\overbrace{\underbrace{\mathbf{X}}_{N \times (1+p)} \quad \underbrace{\boldsymbol{\beta}}_{(1+p) \times 1}}^{N \times 1} \quad + \quad
\overbrace{\underbrace{\mathbf{Z}}_{N \times (1+q)J} \quad \underbrace{\boldsymbol{u}}_{(1+q)J \times 1}}^{N \times 1} \quad + \quad
\overbrace{\boldsymbol{\varepsilon}}^{N \times 1}
$$
- $\beta$ is the fixed effect parameter
- $u$ is $1+q$ random effects with $J$ subjects/groups
- $Z$ is an indicator matrix




> [!NOTE] Implementation in R (LME4) and Python (`statsmodels`)
>
> Lindstrom, M. J., & Bates, D. M. (1988). Newton-Raphson and EM Algorithms for Linear Mixed-Effects Models for Repeated-Measures Data. _Journal of the American Statistical Association_, _83_(404), 1014–1022. https://doi.org/10.2307/2290128
> 
> [[Newton-Raphson and EM Algorithms for Linear Mixed-Effects Models for Repeated-Measures Data.pdf]]




## Categorical Data

Categorical data encoded with **one-hot vector**, i.e., each level of each categorical variable takes a dimension of the **one-hot vector**.

Generally, for $m$ categorical dimensions with $n_j$ levels for the $j$th dimension, the one-hot vector's length at least to be $(\sum_1^m n_j) -1$, i.e., $0$ for the 1st level, $e_i$ for the left levels.

The above full model is equivalent to fit a linear model in each level independently. 
$$
y = \beta_{0, i} + \beta_{1, i}x_1 + \cdots+ \beta_{p, i}x_p + \varepsilon
$$

However, more constraint models can be considered:

1. _Parallel lines_: i.e.  $\beta_{k, i} = \beta_k$ , slopes are constant over different levels.
2. _Concurrent lines_: equal intercepts but different slopes
3. Coincident lines: equal intercepts and equal slopes.


>[!WARNING] Allocated Code
>
> It seems more convenient to allocate a unique integer code for each level; however, the allocated codes **impose a particular metric** on the levels of the qualitative factor.
>
> For example, if the code is assigned as $x_g = 0, 1, 2, \dots, k, \dots$, it implicitly assumes the differences of each level are equal. 
>
> suppose after fit:
> 
> $$ y = \beta_0 \cdots + \beta_gx_g + \cdots $$
> 
> then intercept for level $k$ is 
> $$ \beta_{0,k} = \beta_0 + k\beta_g $$
> thus we have, the differences of adjacent group's intercept are all equal to $\beta_g$ 



