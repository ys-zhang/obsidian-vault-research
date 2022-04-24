#statistics #hypothesis-test  #normal-distribution 

###### Consider Hypothesis 
$$
\begin{align}
H_0 &: \theta = \theta_0 \\
H_1 &: \theta \ne \theta_0 
\end{align}
$$


**Generalized Wald Test** for [[M-estimator]]

$$
Z_{GW} = \sqrt{n}\frac{\hat\theta_M-\theta_0}{\sqrt{\widehat{\text{Var}_{\theta_0}}[\hat\theta_M]}}
$$
where $\widehat{\text{Var}_{\theta_0}}$ is a **consistent estimator**.


###### IDEA

Let $W_n=W(X_1, \dots, W_n)$ be a sequence of estimators with variance $\sigma_n^2$.
The idea is that, it sometimes might be natural to have 
$$
\frac{W_n - \theta}{\sigma_n} \overset{d} \longrightarrow \mathscr N(0, 1)
$$
e.g., 
1. $W_n$ is _MLE_
2. $W_n$ satisfies _central limit theorem_

If we have $S_n$ be an estimator of $\sigma_n$ such that
$$
\frac{S_n}{\sigma_n} \longrightarrow 1
$$
then by [[Slutsky's Theorem]]
$$
\frac{W_n - \theta}{S_n} \overset{d} \longrightarrow \mathscr N(0, 1)
$$

See also
- [[Score Test]];
- [[Graphical Representation of the “Holy Trinity”.pdf|Holy Trinity]].



###### MLE

MLE estimator is [[Asymptotically Efficient]], i.e.

$$
\sqrt{n}(\hat\theta_{mle}-\theta) \to \mathscr N(0, I_1^{-1}(\theta))
$$
Where $I(\theta)$ is the expected [[Fisher Information.pdf]], then the observed Fisher Information is a good estimate of asymptotic variance of $W_n$.
$$
S_n = \sqrt{I_n^{-1}(\theta)}_{\theta=W_n} = \sqrt{\hat I_n^{-1}(W_n)}
$$
thus
$$
\begin{align}
  & \; \frac{W_n - \theta}{S_n} \\
  =& \;\; \sqrt{n}(W_n -\theta) \sqrt{I_n(W_n)} \\
  \overset{d} \to & \;\;
  \mathscr N(0, 1)
\end{align}
$$



