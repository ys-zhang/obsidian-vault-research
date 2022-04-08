$$
q(t|q_i, D_i, b) = \frac{q_i}{(D_ibt + 1)^{1/b}}
$$

$$
q(t|q_i, D_i, b, D_i', T) = \frac{q_i}{(D_ib \cdot t + 1)^{1/b}} \times (T - t \ge 0)
+ \frac{q_i'}{\exp(D_i't)} \times (t - T > 0)
$$

$$
L(q_i, D_i, b, D_i', T) = \sum \| q^{(i)} - q(t_i|q_i, D_i, b, D_i', T) \|^2
$$

$$
\partial L \over \partial (q_i, D_i, b, D_i', T)
$$
