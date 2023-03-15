#logic 

PCTL is an extension of [[computational tree logic (CTL)|CTL]].

The syntax of PCTL is as follows:
$$
\begin{align}
  \Phi &::= \mathrm{true}\; |\; a \;|\; \neg \Phi \; |\; \Phi \;\wedge\; \Phi\; |\; \Pr_{∼p}[\phi] \\
  \phi &::= \mathrm X \; \Phi\; |\; \Phi \;\mathrm U^{\le k} \Phi 
\end{align}
$$
where $∼ \in \{<, \le, \ge, > \}$

>[!NOTE]
> We distinguish between **state formulae** $\Phi$ and **path formulae** $\phi$, which are _evaluated over states and paths, respectively_.
> 
> To specify a property of a [[DTMC (discrete-time Markov chain)|DTMC]], we always use a state formulae: path formulae only occur as the parameter of the $\Pr_{∼p}[\cdot]$ operator.
> 
> Intuitively, a state $s$ of $D$ satisfies $\Pr_{∼p}[\phi]$ if the probability of taking a path from $s$ satisfying $\phi$ is in the interval specified by $∼p$.

- $\mathbf X \Phi (\omega)$ holds iff. $\Phi$ hold's in **next state** (2nd state) of the path $\omega$.
- $\Phi \mathbf U^{\le k} \Psi (\omega)$ is true if $\Psi$ is satisfied within $k$ time-steps and $\Phi$ is true _up until that point_, i.e., 
    $$
    \begin{align*}
      \exists i\le k,& \; \text{s.t.}  \\
       & \omega_i \models \Psi \\
       & \forall j< i,\; \omega_j \models \Phi 
    \end{align*}
    
  $$
