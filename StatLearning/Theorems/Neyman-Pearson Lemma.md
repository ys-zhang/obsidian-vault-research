#statistics  #hypothesis-test 

>[!TLDR]
> LRT (likelihood ratio test) is UMP in single point test.

Hypothesis

$$
\begin{align}
H_0: & \theta = \theta_0\\
H_1: & \theta = \theta_1
\end{align}
$$
Let a test with **critical/rejection region** 
$$
R =\{\;x: {L_{\theta_0}(x) \over L_{\theta_1}(x)} < k  \;\}
$$
for some $k>0$, let $\alpha=\Pr_{\theta_0}(X\in R)$ be the size/level of the test

Then

Any [[Concepts of Statistic Test#UMP Uniformly Most Powerful Test|UMP]] level $\alpha$ test is of the above form, and the above test is a UMP level $\alpha$ test
