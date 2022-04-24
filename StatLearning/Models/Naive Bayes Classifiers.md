Assumption, give the groups the predictors are independent.

$$
f_k(x) = \Pr(x_1, \dots, x_p|G=k) = \prod_{i=1}^p f_{k,i}(x) 
$$


 - The individual class-conditional marginal densities $f_{ki}$ can each be estimated separately using one-dimensional kernel density estimates ([[Kernel Density Estimators]]).
- The reasons of Naive Bayes work: although the individual class density estimates may be biased, this bias might not hurt the posterior probabilities as much, especially near the decision regions.
