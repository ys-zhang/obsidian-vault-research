#statistics #distribution

$$
\begin{align}
f(x|\theta) &=  \frac{h(x)}{g(\theta)} e^{ \eta^T(\theta)T(x) } \\
&= \exp(\; \eta^T(\theta)T(x) - A(\theta) + B(x)) \;)
\end{align}
$$

if $\eta^T(\theta) = \theta$ then 
$$
f(x|\eta) = \exp(\; \eta^T T(x) - A(\eta) + B(x)) \;)
$$
is called in **Canonical Form**.

If $\eta^T(\theta) = \theta$ and $T(x) = x$ then
$$
f(x|\eta) = \exp(\; \eta^Tx - A(\eta) + B(x)) \;)
$$
is called in **Natural Form**

# Concepts

- **natural parameter**:  $\eta$
- $T(x)$ is a [[Sufficient Statistic]]
- $A(x)$ is called the **log partition function**, and the [[Cumulant|cumulant generative function]] of the distribution
    - $\mu = E[X] = A'(\theta)$
    - $\sigma^2 = Var[X] = A''(\theta)$




# Properties

- *Exponential family* is the only distribution family have a **finite sized** [[Sufficient Statistic]]
- It is **self conjugate**
- Support [[GLM (Generalized Linear Model)]]
- Support [[variational inference]]



# Examples

- is: $[f(x)]^{g(\theta)}$, $[g(\theta)]^{f(x)}$, $[f(x)]^{h(x)g(\theta)}$, $[g(\theta)]^{h(x)j(\theta)}$
- not: $[f(x)g(\theta)]^{h(x)j(\theta)}$



# Reference

- Murphy, Chapter 9, 21