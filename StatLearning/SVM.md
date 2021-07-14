[sklearn doc](https://www.notion.so/yunshuiz/SVM-1929c77001d94c0593892d24cd1dc49e#90076d6aa1e64f2183df759925d8f840)

> material from **A tutorial on support vector regression**.

# Formulation
## Notations and Ideas

> In **_ε-SV regression_** (Vapnik 1995), our goal is to find a function $f(x)$ that has at most $\varepsilon$ deviation from the actually obtained targets $y_i$ for all the training data, and at the same time is **_as flat as possible._**

此处含义为 $\varepsilon$-分割超平面在 $\mathcal{X} \times \mathbb{R}$ 上越水平越好, 因为越水平则对$\mathcal{X}$ 上的误差越不敏感，即可以容忍更大的$x_i$的误差

Data Set 
$$\{(x_i, y_i)\} \subset \mathcal{X} \times \mathbb{R} $$

Let $$f(x) = \langle w, x \rangle + b$$

我们有超平面法向量为 $(w, 1)$，故问题等价于

$$
\begin{align}
	\min \;\; & \|w\| \\
	\mathrm{s.t.}\;\; & |\langle w, x_i \rangle + b - y_i| < \varepsilon 
\end{align}
$$


where **_flatness_** in the case of (1) means that one seeks a small $w$.

since (1) is not feasible in all cases, we can introduce **slack variables**.

$$
\begin{align}
	\min \;\; &\|w\| + C \sum(\xi_i^+ + \xi_i^- ) \\
	\mathrm{s.t.}\;\; & \langle w, x_i \rangle + b - y_i < \varepsilon + \xi_i^- \\ 
	& y_i - \langle w, x_i \rangle - b < \varepsilon + \xi_i^+ \\ 
	& \xi_i^+, \xi_i^- \ge 0
\end{align}
$$

It turns out that in most cases the optimization problem (2) can be solved more easily in its **dual formulation**.

Suppose $\mathcal{X} \subset \mathbb{R}^d$, and sample size is $n$. The original problem (2) has $O(n)$ constrains, while its dual has $O(d)$ constrains.

## The dual Problem

The basic idea is to use [[Lagrange Multiplyer]] to reform the original problem into this dual space. 