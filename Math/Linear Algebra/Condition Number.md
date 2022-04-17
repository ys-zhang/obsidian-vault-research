# Absolute Condition Number

the absolute condition number of a problem $f$:

$$
\lim_{\varepsilon \to 0} \sup_{\|\Delta x\| \le \varepsilon} 
\frac{\|\Delta f(x)\|}{\|\Delta x\|}
$$

# Relative Condition Number

$$
\lim_{\varepsilon \to 0} \sup_{\|\Delta x\| \le \varepsilon} 
\frac{\|\Delta f(x)\| / \| f(x)\|}{\|\Delta x\| / \|x\|}
$$

# Matrix Condition Number

Consider the inversion problem
$$
Ax = b
$$
here 
$$
f(b) = A^{-1}b
$$
The (relative) **condition number** is defined more precisely to be the maximum ratio of the _relative error_ in $x$ to the relative error in $b$.
$$
\begin{align}
\kappa(A) =&\max_{\|e\|,\|b\|\ne 0}\frac{\|A^{-1}e\|/\|A^{-1}b\|}{\|e\|/\|b\|} \\
=& \max_{\|e\| \ne 0}\frac{\|A^{-1}e\|}{\|e\|} \cdot 
\max_{\|b\| \ne 0}\frac{\|b\|}{\|A^{-1}b\|} \\
= & \max_{\|e\| \ne 0}\frac{\|A^{-1}e\|}{\|e\|} \cdot 
\max_{\|b\| \ne 0}\frac{\|Ab'\|}{\|b'\|} \\
= & \|A^{-1}\| \cdot \|A\| \\
= & \frac{\lambda_{\max}}{\lambda_{\min}}
\end{align}
$$

Matrix condition number is used in [[Multicollinearity]] diagnose.

A problem with a low condition number is said to be **well-conditioned**, while a problem with a high condition number is said to be **ill-conditioned**.

> [!NOTE]  Rule of thumb
>
> If the condition number $\kappa(A) = 10^k$, then you may lose up to $k$ digits of accuracy on top of what would be lost to the numerical method due to loss of precision from arithmetic methods.

