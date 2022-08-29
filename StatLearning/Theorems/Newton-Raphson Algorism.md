#optimizer

![[Pasted image 20220413210422.png]]

$$
x_{n+1} = x_n -  (\partial_xf)^{-1} \cdot f(x_n)
$$


>[!WARNING]
>If you start it far from a root, the convergence can be hard to predict, and it may not even
converge at all (it can oscillate forever around a local minimum)


# Proof of Univariant Newton's Method

Expand $f(\alpha)$ at $x_n$, $\exists \xi$ between $\alpha$ and $x_n$ such that:
$$
f(\alpha) = f(x_n) + f'(x_n)\cdot(\alpha - x_n) + \frac{1}{2}f''(\xi_n)\cdot(\alpha - x_n)^2
$$
$\alpha$ is the solution of the function $f(x)  = 0$.

$$
(\alpha-x_n) + \frac{f(x_n)}{f'(x_n)}  =  - \frac{1}{2} \frac{f''(\xi_n)}{f'(x_n)} (\alpha - x_n)^2
$$
recall 
$$
x_{n+1} = x_n - \frac{f(x_n)}{f'(x_n)}
$$
then we have
$$
\alpha-x_{n+1} = (\alpha-x_n) + (x_n - x_{n+1}) =  - \frac{1}{2}f'^{-1}(x_n)f''(\xi_n)(\alpha - x_n)^2
$$
i.e.
$$
\varepsilon_{n+1} = \alpha - x_{n+1} = - \frac{1}{2}f'^{-1}(x_n)f''(\xi_n)\varepsilon_n^2
$$
Taking the norm:
$$
\|\varepsilon_{n+1}\| \le \frac{1}{2}  \frac{\|f''(\xi_n)\|}{\|f'(x_n)\|}  \cdot \|\varepsilon_n\|^2
$$
Convergence condition:
1.  $f'(x) \ne 0$; for all $x\in I$, where $I$ is the interval $[\alpha − r, \alpha + r]$ for some $r \ge |\alpha − x_0|$;
2.  $f''(x)$ is continuous, for all $x\in I$;
3.  $x_0$ is _sufficiently_ close to the root $\alpha$.


# Multi-variant Newton's Method

Suppose an equation 
$$
f(x) = 0
$$
where 
$$
f: R^n \to R^n
$$
the Jacobian of $f$ 
$$
J_f(x) = \bigg ( \frac{\partial f_i}{\partial x_j} \bigg)_{ij}
$$
An update step:

$$
x_{n+1} = x_n - J_f^{-1}(x_n) \cdot f(x_n) 
$$
Usually to prevent divergence usually a factor $\alpha_n$ is introduced
$$
  x_{n+1} = x_n - \alpha_n \cdot J_f^{-1}(x_n) \cdot f(x_n) 
$$


