# Basic Ideas

Problem solving pipeline:
1. start with greedy as baseline
2. go beyond greedy using:
    - Constraint Programming
    - Local Search
    - Mixed Integer Programming

# Problems

## Knapsack

Given $\{(x_i,y_i)\}$ and $K>0$

$$
\begin{align}
&\max \sum y_i \\
s.t. & \sum w_ix_i \le K
\end{align}
$$


