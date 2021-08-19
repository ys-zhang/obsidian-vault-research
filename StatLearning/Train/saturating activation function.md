[machine learning - What does the term saturating nonlinearities mean? - Cross Validated (stackexchange.com)](https://stats.stackexchange.com/questions/174295/what-does-the-term-saturating-nonlinearities-mean)

## Intuition

A saturating activation function squeezes the input.

> Saturation occurs when the hidden units of a neural network (NN) **predominantly output values close to the asymptotic ends** of the activation function range.

---

## Definitions

- $f$ is non-saturating iff 
$$
|\lim_{z\to -\infty}f(z)|=+\infty \lor |\lim_{z\to+\infty}f(z)|=+\infty
$$
- $f$ is saturating iff $f$ is not non-saturating.

These definitions are not specific to convolutional neural networks.

## Why this is a PROBLEM

### Observations
1. **Non-linearity** of the hidden units allows a NN to approximate any non-linear mapping between inputs and outputs provided that enough neurons are used in the hidden layer.
2. **Upper and lower bounds** ensure that the signal does not grow uncontrollably as it propagates from one layer to the next.

> if the magnitude of the input signal lies **outside the active range** of a sigmoidal activation function, the output signal will be **close to an asymptotic value**

### Slow Training

let $f\in C^2(\mathbb R)$ be a saturating activation function with $\lim |f(x)| = C < \infty$ 
thus we have $x|f'(x)| \to 0$ as $x \to \infty$,
so asymptotically 
$$
	f'(x) < \frac{1}{x} \quad as \quad x >> 1
$$
as **the derivative is small, the convergence is slow**.

## Examples

The **Rectified Linear Unit (ReLU)** activation function, which is defined as $f(x)=\max(0,x)$ is **non-saturating** because $f(z) \to +\infty$, as $z \to +\infty$

![[ReLU.png]]

The **sigmoid** activation function, which is defined as $f(x) = \frac{1}{1+\exp(-x)}$ is **saturating**, because it squashes real numbers to range between $[0,1]$

![[sigmoid.png]]


The **tanh (hyperbolic tangent)** activation function is **saturating** as it squashes real numbers to range between $[-1,1]$:

![[tanh.png]]