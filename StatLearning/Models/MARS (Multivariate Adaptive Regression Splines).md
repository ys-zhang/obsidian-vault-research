#statistics  #general-additive-model  

#todo 


>[!TLDR]
>MARS is a GAM, with basis functions are products of ReLU/hinge functions


# Basis

MARS is an adaptive procedure for regression, and is well suited for high-
dimensional problems (i.e., a large number of inputs).

The regression basis is the `ReLU` or hinge function:
$$
\mathcal C = \big\{\; (X_j-t)_+, \;\;(t-X_j)_+ \big\}_{t\in\{x^{(1)}_j, \dots, x^{(N)}_j \},\;\; j\in\{1, \dots ,p\} }
$$

# Model

The model is GAM

$$
f(x) = \beta_0 + \sum_1^M\beta_mh_m(x)
$$
where $h_m = c_1\times c_2 \times \dots \times c_k$ with $c_i\in \mathcal C$ and $k\ge 1$.


# Algorithm

##### Forward Pass

- (greedy) At each step it finds the pair of basis functions that gives the maximum reduction in _sum-of-squares residue_ error
- Each new basis function consists of a term already in the model (which could perhaps be the intercept term) multiplied by a new hinge function.
- Stop if
  -  SSR reduction is too small 
  - number of terms reach maximum $M$
  - a maximum allowable degree of interaction is reached

##### Backward Prune Pass

- Removes terms one by one, deleting the least effective term at each step until it finds the best sub-model.
- Model subsets are compared using the Generalized cross validation (GCV) criterion
$$
GCV = \frac{RSS}{ N \cdot (1 - \frac{\text{effective number of parameters}}{N})^2}
$$


# Relation with Tree Models

Consider categorical response and its relation with [[Tree Models]].



# R

`library(earth)`