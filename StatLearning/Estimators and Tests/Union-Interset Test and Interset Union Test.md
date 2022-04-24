#statistics #hypothesis-test  


The idea of **UIT** and **IUT** is to disassemble a test in to smaller subtests.

# UIT 


$$
\begin{align}
H_0 &: \;\; \theta \in \bigcap_{\gamma\in\Gamma}\Theta_\gamma \\
H_{0\gamma} &: \;\; \theta\in \Theta_\gamma
\end{align}
$$
the rejection area is the _union of all subtest's rejection region_.
$$
R = \bigcup_{\gamma\in\Gamma}R_\gamma
$$

# IUT
$$
\begin{align}
H_0 &: \;\; \theta \in \bigcup_{\gamma\in\Gamma}\Theta_\gamma \\
H_{0\gamma} &: \;\; \theta\in \Theta_\gamma
\end{align}
$$
the rejection area is the _intersection of all subtest's rejection region_.
$$
R = \bigcap_{\gamma\in\Gamma}R_\gamma
$$

>[!EXAMPLE]
>two-side student $t$ test


# Calculate size/level of UIT/IUT


## LRT of UIT


>[!TLDR] 
>UIT is more conservative than LRT but less powerful.
> 
>UIT through LRT has a level higher than overall LRT,  but is uniformly less powerful than overall LRT


when all subtests are Likelihood Ratio Test

- let $\lambda_\gamma(x)$ be the likelihood ratio of subtest $\gamma$ 
- let $\lambda(x)$ be the likelihood ratio of the original test

 Then the UIT  rejection region of the problem is 
 $$
 R_{\text{UIT}} = \{\inf_\gamma \lambda_\gamma(x) < c \}
$$
and the LRT of the original test is 
$$
  R_{\text{LRT}} = \{\lambda(x) < c\}
$$
then easy to see that

$$
R_{\text{LRT}} \subset  R_{\text{UIT}}
$$
and
$$
 \beta_{\text{UIT}}(\theta) \le \beta_{\text{LRT}}(\theta)
$$
since $\lambda_\gamma(x) \ge \lambda(x)$. 


## IUT

Let $\alpha_\gamma$ is the level of subtest $H_{0\gamma}$ with rejection region $R_\gamma$ 
The IUT rejection region $R = \bigcap_\gamma R_\gamma$  has a level 
$$
\alpha = \sup_{\gamma\in\Gamma}\alpha_\gamma
$$

This result can be quite conservative.



