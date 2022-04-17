#statistics 

>[!TLDR]
>A sequence of estimator is said to be **asymptotically efficient** if it's _asymptotic variance_ converge to [[Cramer-Rao Inequality|Cramer-Rao Lower Bound]]


$$
\sqrt{n}(W_n - \tau(\theta)) \to \mathscr N(0, {[\tau'(\theta)]^2\over E_\theta\big[ (\frac{\partial}{\partial\theta}\ln f(X|\theta))^2 \big]})
$$

>[!INFO]
> _MLE_ is asymptotically efficient. _(by Taylor Expansion of log likelihood_)
> 
> A similar result see [[M-estimator]].


###### Asymptotic Relative Efficiency

$$
\begin{align}
\sqrt{n}(W_n - \tau(\theta)) \to \mathscr N(0, \sigma^2_W) \\
\sqrt{n}(V_n - \tau(\theta)) \to \mathscr N(0, \sigma^2_V) \\

\end{align}
$$
Define _asymptotic relative efficiency_ of $V_n$ _w.r.t._ $W_n$.
$$
\text{ARE}(V_n, W_n) = \frac{\sigma_W^2}{\sigma_V^2}
$$
另见samplling中的 _等效样本量_
