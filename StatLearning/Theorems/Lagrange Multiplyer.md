Optimization on a surface

#  Equality constrains

$$
\begin{align}
\min &\quad f(x) \\
s.t. &\quad g_i(x) = 0
\end{align}
$$

Consider the surface $G = \{x: g_i(x)=0\}$
if $x_0$ is the optimal solution then:
1. $\nabla_x f(x_0) \perp T_G(x_0)$, i.e. 
$$\nabla_x f(x_0) = \sum \lambda_i \nabla_x g_i(x_0) $$
2. $g_i(x_0) = 0$ 

equivalently, Let
$$
L(x, \lambda) = f(x) - \langle \lambda, g(x) \rangle
$$
then
$$
\nabla_{x, \lambda}L(x)=0
$$
gives a necessary but not sufficient condition of the optimal solution $x_0$.

# Inequality constrains (Karush–Kuhn–Tucker conditions)

$$
\begin{align}
\min &\quad f(x) \\
s.t. &\quad g_i(x) \le 0 \\
&\quad h_i(x) = 0
\end{align}
$$

similarly we have

1. $$\nabla_x f(x_0) + \sum \mu_i \nabla_x g_i(x_0) +\sum\lambda_j\nabla_xh_j(x_0) = 0$$
2. $g_i(x_0) = 0$ 
3. $h_i(x_0) \le 0$ 

additionally
4. $\mu_i \ge 0$
5. $\sum\mu_i g_i(x_0)=0$

