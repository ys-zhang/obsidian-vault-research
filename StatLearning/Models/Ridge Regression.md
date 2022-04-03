$$
\hat \beta_{ridge} = (X^TX + \lambda I)^{-1}X^Ty 
$$

equivalent to 

$$
\min_\beta\; \|y - X\beta\|^2 + \lambda \|\beta\|^2 
$$
equivalent to a **normal prior** on $\beta$

equivalent to 
$$
\begin{align}
\min_\beta \; & (\beta-\hat\beta)^TX^TX(\beta-\hat \beta) \\
s.t. \; & \|\beta\| \le d
\end{align}
$$
notice $(\hat \beta_{ridge}-\hat\beta)^TX^TX(\hat \beta_{ridge}-\hat \beta) = \|\hat y_{ridge} - \hat y\|^2$.

![[Pasted image 20220330114056.png]]


