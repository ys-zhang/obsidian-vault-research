#statistics 

# Converge in Probability
We say random variable $X_n$ converge to $X$ _in probability_ if

$$
\lim_{n\to\infty} \Pr(|X_n -X|<\varepsilon) = 1 \;\;\forall\varepsilon>0 
$$

>[!Theorem]
>If function $h$ is continuous and $X_n$ converge to $X$ in probability then 
>
>$h(X_n)$ converge to $h(X)$ in probability.

# Almost Surely Convergence

$$
 \Pr(\lim_{n\to\infty}|X_n -X|<\varepsilon) = 1 \;\;\forall\varepsilon>0 
$$

# Converge in Distribution

$$
\lim F_{X_n}(x) = F_X(x) \; \forall x
$$
converge in probability implies converge in distribution

>[!Theorem]
> $X_n \to c$  in distribution _if and only if_ converge to $c$ in probability


