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
	\mathrm{s.t.}\;\; 
	& y_i - \langle w, x_i \rangle - b < \varepsilon + \xi_i^+ \\
	& \langle w, x_i \rangle + b - y_i < \varepsilon + \xi_i^- \\ 
	& \xi_i^+, \xi_i^- \ge 0
\end{align}
$$

It turns out that in most cases the optimization problem (2) can be solved more easily in its **dual formulation**.

Suppose $\mathcal{X} \subset \mathbb{R}^d$, and sample size is $n$. The original problem (2) has $O(n)$ constrains, while its dual has $O(d)$ constrains.

## The dual Problem

The basic idea is to use [[Lagrange Multiplyer]] to reform the original problem into this dual space. 

$$
\begin{align}
L(w,  ) =& \|w\| + C \sum(\xi_i^+ + \xi_i^- ) \\
	&-\sum_{i=1}^\ell(\eta^+_i\xi_i^+ + \eta^-_i\xi_i^-) \\
	&-\sum _{i=1}^\ell \alpha_i^+(\varepsilon + \xi_i^+ - y_i+ \langle w,x_i \rangle + b) \\
	&-\sum _{i=1}^\ell \alpha_i^-(\varepsilon + \xi_i^- + y_i- \langle w,x_i \rangle - b) \\
\end{align}
$$

Dual problem:

$$
\begin{align}
	\max &\quad -\frac{1}{2} \sum_{i,j}^\ell (\alpha^+_i - \alpha_i^-) (\alpha^+_j - \alpha_j^-) \langle x_i x_j \rangle
	-\varepsilon \sum_i^\ell (\alpha^+_i + \alpha_i^-) + \sum_i^\ell y_i (\alpha^+_i - \alpha_i^-)\\
   s.t. &\quad \sum (\alpha^+_i - \alpha_i^-) =0 \\
   &\quad \alpha_i^+, \alpha_j^- \in [0, C]
\end{align}
$$

Furthermore, we have:

$$
\begin{align}
   w &= \sum_i^\ell(\alpha_i^+ - \alpha_i^-)x_i \\
   f(x) &= \sum_i^\ell (\alpha^+_i - \alpha_i^-) \langle x_i,x\rangle + b
\end{align}
$$

# Kernel and nonlinear feature

consider Feature parameterize, map from parameter space to feature space
$$
\Phi : \chi \to \mathcal{F}
$$

> The **primary problem** has $dim(\mathcal{F})$ constrains, which is a problem when the feature space is high dimensional.
> While the **dual problem** has $O(\ell)$, i.e. , *number of sample* constrains.


# Risk/Loss

General form with weight penalty
$$
\begin{align}
	R[f] &= \int c(x, y, f(x)) dP_{x,y}  \\
	R_{emp}[f] &= \frac{1}{\ell} \sum_{i=1}^\ell c(x_i, y_i, f(x_i))   \\
	R_{reg}[f] &= R_{emp}[f] + \frac{\lambda}{2} \| w \|
\end{align}
$$

![[Common-loss-func-and-density-model.png]]

## $\varepsilon$-Risk

$$
	c(x, y, f(x)) = |y-f(x)|_\varepsilon = \max \{|y-f(x)|-\varepsilon, 0\}
$$

> The **primary problem** is equivalent to the $\varepsilon$-risk with $L_2$ penalty.

## Maximum Likelihood-Risk
$$
	c(x, y, f (x)) = - \log p(y - f (x))
$$

