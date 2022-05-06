#statistics 

# Idea

Least Angle Regression is similar to [[Model Selection#Forward Backward Stepwise Stagewise Selection|Forward Stepwise Regression]].

1. At the first step it identifies the variable most correlated with the response.
2.  Rather than fit this variable completely, LAR moves the coefficient of this variable continuously toward its least-squares value (causing its correlation with the evolving residual to decrease in absolute value). 
3. As soon as another variable “catches up” in terms of correlation with the residual, the process is paused. The second variable then joins the active set, and their coefficients are moved together in a way that keeps their correlations tied and decreasing.



# Algorithm

At iteration $k$ let
- $\mathcal A_k$ be the be the current active set of predictors 
- $\beta_{\mathcal A_k}$ be the current coefficients;
- $\hat y_k = X_{\mathcal A_k}\beta_{\mathcal A_k}$ be the fitted value  


1. Calculate the current residue
$$
r_k = \hat y_k - X_{\mathcal A_k}\beta_{\mathcal A_k} 
$$
2. Calculate the direction
$$
\begin{align}
\delta_k &= (X^T_{\mathcal A_k}X_{\mathcal A_k})^{-1}X^T_{\mathcal A_k}r_k 
\end{align}
$$
3. Define an $\alpha$ forward step, let  $u_k = X_{\mathcal A_k}\delta_k$ be the **LAR direction**
$$
\begin{align}
  \beta_{\mathcal A_k}(\alpha) &= \beta_{\mathcal A_k} + \alpha\cdot\delta_k \\ \\
  \hat y_k(\alpha) &= \hat y_k + \alpha \cdot u_k \\ \\
  r_k(\alpha) &= r_k + \alpha \cdot u_k
\end{align}
$$
4. Do an $\alpha$ forward step, 
    1. Test if there exists some predictor in $\mathcal A \backslash \mathcal A_k$ is more correlated to $r_k(\alpha)$ , add it to $\mathcal A_k$.
    2. if some predictor in $\mathcal A_k$ back to a zero coefficient, drop it out from $A_k$
5. Move to next iteration.


##### ESL Exercise 3.24 & 3.25

> _LAR directions_. Show that the LAR direction makes an equal angle with each of the predictors in $\mathcal A_k$.
> 
> Hint: Think when will a new predictor can be bring into the active set

> _LAR look-ahead (Efron et al., 2004, Sec. 2)_. Starting at the beginning of the $k$th step of the LAR algorithm, derive expressions to identify the next variable to enter the active set at step $k +1$, and the value of $\alpha$ at which this occurs.




