Define the **differential model**:
$$
\begin{align}
(1 + \xi_1\nabla^1+\xi_2\nabla^2+\cdots +\xi_r\nabla^r)Y_t &= g\cdot(1+\eta_1\nabla^1 + \eta_2\nabla^2 +\cdots+\eta_s\nabla^s)X_{t-b} \\
\\
(1- \delta_1B - \delta_2B^2 - \cdots - \delta_rB^r ) Y_t &= (\omega_0 - \omega_1B - \omega_2B^2 - \cdots - \omega_sB^s)X_{t-b}

\end{align}
$$
where
$$
g = \frac{\omega_0 - \sum_1^s \omega_i}{1-\sum_1^r \delta_i}
$$
if $Y_t$ and $X_t$ are stationary then $E[Y_t] = g \cdot E[X_t]$.

Define the **impulse input model** $v(B)$
$$
Y_t = v(B)X_t
$$

Define the **step input model** 
$$
\begin{align}
V_k &= \sum_{i=0}^k v_j \\
 v(B) &= (1-B)V(B) 
\end{align}
$$

- $v_t$ is the response of impulse input at time $0$
- $V_t$ is the response of step input at time 0 (阶梯函数输入)

Define the **linear transform model**
$$
Y_t = (\delta_1Y_{t-1} + \delta_2Y_{t-2} + \cdots + \delta_rY_{t-r}) + (\omega_0 X_{t-b} - \omega_1X_{t-b-1} - \omega_2X_{t-b-2} - \cdots -\omega_sX_{t-b-s})
$$

Define the **noisy transform model**

$$
Y_t = \delta^{-1}(B)\omega(B)X_{t-b} + 
$$


