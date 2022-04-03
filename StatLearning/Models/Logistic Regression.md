Consider 
$$
Y \in \{0, 1\}
$$
with 
$$
\begin{align}
\pi &= \Pr(Y=1|X=x) \\
\eta &= \ln (\textrm{odds}) = \ln \frac{\pi}{1-\pi} = x^T\beta 
\end{align}
$$
or equivalently,

$$
\pi = \frac{\exp(x^T\beta)}{1+\exp(x^T\beta)}
$$

```ad-note
title: Relation with LDA

Logistic regression has a connection with [[LDA (Linear Discriminate Analysis)]].
Suppose 

$$
\begin{align}
\Pr(X=x|Y=1) &= \exp(-x^T\beta_1) \\
\Pr(X=x|Y=0) &= \exp(-x^T\beta_0) \\
\end{align}
$$

suppose we have a prior on $\Pr(Y=1)$, which is a constant w.r.t. $x$.
Thus

$$
\begin{align}
\Pr(X=x, Y=1) &= \exp(-x^T\beta_1) \\
\Pr(X=x, Y=0) &= \exp(-x^T\beta_0) \\
\end{align}
$$
By Bayesion formula,
$$
\begin{align}
\pi &= \Pr(Y=1|X=x) \\
&= \frac{\exp(-x^T\beta_1)}{\exp(-x^T\beta_2) + \exp(-x^T\beta_1)} \\
&= \frac{\exp(x^T(\beta_2-\beta_1))}{1 + \exp(x^T(\beta_2-\beta_1))}
\end{align}
$$


```


# Multi-level 

$$
Y \in \{1, 2,\dots, K\}
$$
let $\pi_i = \Pr(Y=i|X=x)$
$$
\ln\frac{\pi_i}{\pi_k} = x^T\beta_i
$$