#algorithm-reduction  #Jeff-algo-book

# Leaned Needle Problem 

> From [Jeff's Algorithm](http://algorithms.wtf/) Exercise 1.3

![[Pasted image 20220430144025.png]]



##### Assumption
- $3$ pegs: peg, $a$, $b$, and $c$;
- peg $b$ is leaning to one side and _any number of disks on the leaning
peg can be moved together to another needle in a single move_.


##### target
move all plates from $a$ to $c$ using fewest steps

##### Observations

1. It has to reach an internal state that 
    - one peg is empty
    - one peg holds the largest plate
    - one peg holds all other plates
2. Further investigate shows that
    - $a$ is empty 

##### Algorithm

trivial

##### Analysis
Let 
- $T_1(n)$ be the minimum steps to move $n$ plates _from ordinary needle to ordinary needle_
- $T_2(n)$ be the minimum steps to move $n$ plates _from ordinary needle to leaned needle_

We have 

$$
\begin{align}
T_1(n) &= T_2(n-1) + 2 \\
T_2(n) &= T_1(n-1) + 1 + T_2(n-1) \\
&= T_2(n-1) + T_2(n-2) + 3
\end{align}
$$
where 
- $T_2(1) = 1$, $T_2(2) = 3$, equivalent $T_2(0) = -1$ 
- $\alpha$ and $\beta$ be the roots of $x^2 = x + 1$, and $c_n = T_2(n) - \alpha T_2(n-1)$
  - $\alpha = \frac{1-\sqrt{5}}{2}$ and $\beta = \frac{1+\sqrt{5}}{2}$, with $\alpha\beta=-1$ and $\alpha + \beta = 1$
  - $c_n = \beta c_{n-1} + 3$,  $c_1 = T_2(1)-\alpha T_2(0) = 1 + \alpha$ 
  - solve equation of $c_n - \gamma = \beta(c_{n-1}-\gamma)$ gets $\gamma = -3\beta$
  - $c_n = \beta^{n-1}(c_1 -\gamma) + \gamma=\beta^{n-1}(2+2\beta) - 3\beta = 2\beta^n + 2\beta^{n-1} - 3\beta$   
- we have
$$
  T_2(n) = \alpha T_2(n-1) + 2\beta^n + 2\beta^{n-1} - 3\beta
$$

to solve the above induction suppose the form $T_2(n) -\delta = \alpha (T_2(n-1) - \delta)$, it result in 
$$
  T_2(n) = \alpha T_2(n-1) + \delta-\alpha\delta
$$
and the equation 
$$
\delta-\alpha\delta = -3\beta
$$
is solved by $\delta = -3$, thus

$$
  T_2(n) + 3 = \alpha (T_2(n-1) + 3) + \beta^{n-1}(2+2\beta) 
$$
reform to 

$$
\begin{align}
  \frac{T_2(n) + 3}{\beta^n} &=  -\alpha^2\frac{T_2(n-1) + 3}{\beta^{n-1}} + (2-2\alpha) \\ \\
  d_n &= -\alpha^2 d_{n-1} + 2\beta 
\end{align}
$$
with 
  1. $d_n=\frac{T_2(n) + 3}{\beta^n}$
  2. $d_1 = \frac{T_2(1) + 3}{\beta} = -4\alpha$

Further reform it to 

$$
d_n - \eta = -\alpha^2(d_{n-1}-\eta)
$$
gives $\eta = \frac{2\beta}{1+\alpha^2}$

thus 
$$
\begin{align}
d_n &= -\alpha^{2n-2}(d_1 - \eta) + \eta \\
& = \alpha^{2n-2}(4\alpha + \eta) + \eta
\end{align}
$$
and
$$
\begin{align}
T_2(n) + 3 &= \alpha^{n-2}(4\alpha + \eta) + \eta\beta^n \\
&= \alpha^{n-2}(4\alpha + \frac{2-2\alpha}{1+\alpha^2})  + \frac{2\beta^{n+3}}{1+\beta^2}
\end{align}
$$
with $1+\alpha=\alpha^2$ we have
$$
\begin{align}
4\alpha + \frac{2-2\alpha}{1+\alpha^2} &= 2\frac{2\alpha^3 + 1+\alpha}{1+\alpha^2} \\
&= 2\frac{2\alpha^3 + \alpha^2}{1+\alpha^2} \\
&= 2\alpha^2\frac{2\alpha + 1}{1+\alpha^2} \\
&= 2\alpha^2\frac{(\alpha + 1) + \alpha}{1+\alpha^2} \\
&= 2\alpha^2\frac{\alpha^2 + \alpha}{1+\alpha^2} \\
&= 2\alpha^2\frac{\alpha(1 + \alpha)}{1+\alpha^2} \\
&= 2\alpha^2\frac{\alpha^3}{1+\alpha^2} \\
\end{align}
$$
thus we have
$$
T_2(n) = 2(\frac{\alpha^{n+3}}{1+\alpha^2} + \frac{\beta^{n+3}}{1+\beta^2}) - 3
$$
and

$$
T_1(n) = 2(\frac{\alpha^{n+3}}{1+\alpha^2} + \frac{\beta^{n+3}}{1+\beta^2}) - 1
$$
where $\alpha = \frac{1-\sqrt{5}}{2}$ and $\beta = \frac{1+\sqrt{5}}{2}$.


# All goes through 0 problem


##### Assumptions

- need to move all plates from peg $0$ to peg $2$.
- We have 3 pegs, peg $0$, peg $1$ and peg $2$.
- All movements must go trough peg $0$, i.e., no movement allow between peg $1$ and peg $2$.

 ##### Analysis

let 
- $T(n)$ be the fewest number of  steps from peg $0$ to peg $1$ or peg $2$;
- $S(n)$ be the fewest number of  steps from peg $1$ or peg $2$ to peg $0$;
- $R(n)$ be the fewest number of  steps between  peg $1$ and peg $2$.


We have
$$
\begin{align}
T(n) &= T(n-1) + 1 + R(n-1) \\
R(n) &= R(n-1) + 1 + R(n-1) + 1 + R(n-1) = 3R(n-1) + 2
\end{align}
$$
with $R(1)$ = 2, reform induction of $R(n)$ to
$$
R(n) + 1 = 3(R(n-1) + 1)
$$
gives 
$$
R(n) = 3^n - 1
$$
and
$$
T(n) = T(n-1) + 3^{n-1}
$$
with $T(1)=1$, and equivalent $T(0)=0$ gives

$$
T(n) = T(0) + \sum_0^{n-1} 3^k = \frac{3^{n}-1}{2}
$$



# Counter-clockwise problem 
![[Pasted image 20220430171320.png]]

##### Assumption

only allow
- $0 \to 2$
- $2\to 1$
- $1\to0$ 

##### Analysis

Let
- $T_n$ be the fewest number steps to adjacent peg, e.g., $2\to0$
- $R_n$ be the fewest number steps to the peg need a jump, e.g., $2\to1$

then

$$
\begin{align}
T_n &= R_{n-1} + 1 + R_{n-1} \\ 
    &=  2R_{n-1} + 1 \\ \\
R_n &= R_{n-1} + 1 + T_{n-1} + 1 + R_{n-1} \\
    &= 2R_{n-1} + T_{n-1} + 2 \\
    &= 2R_{n-1} + 2R_{n-2} + 3
\end{align}
$$
solve the generator function $x^2-2x-2=0$  gives 
1. $\alpha = 1 + \sqrt{3}$ and $\beta = 1 - \sqrt{3}$
2. $\alpha+\beta=2$ and $\alpha\beta=-2$
 and
$$
R_n - \beta R_{n-1} = \alpha(R_{n-1} - \beta R_{n-2}) +3
$$
with $R_1 = 2$ and $R_2 = 2*2 + 1 +2=7$, equivalent $R_0 = 0$.

Let $c_n = R_n - \beta R_{n-1}$ with $c_1=2$ then
$$
\begin{align}
c_n &= \alpha c_{n-1} + 3 \\ \\
c_n - \frac{3}{1-\alpha} &= \alpha (c_{n-1} - \frac{3}{1-\alpha})
\end{align}
$$
gives

$$
\begin{align}
R_n - \beta R_{n-1} &= c_n = \alpha^{n-1}(2-\frac{3}{1-\alpha}) + \frac{3}{1-\alpha} \\
&= \alpha^{n-1}(2-\frac{3}{1-\alpha}) + \frac{3}{1-\alpha} \\
&= -\alpha^{n-1}\frac{1+2\alpha}{1-\alpha} + \frac{3}{1-\alpha} \\
&= -\alpha^{n-1}\frac{(2+2\alpha) - 1}{1-\alpha} + \frac{3}{1-\alpha} \\
&= \alpha^{n-1}(1+\alpha) + \frac{3}{1-\alpha} \\
&= \alpha^{n-1}\frac{\alpha^2}{2} + \frac{3}{1-\alpha} \\
&= \frac{\alpha^{n+1}}{2} + \frac{3}{1-\alpha} \\

\end{align}
$$

reform the above to $R_n -\gamma = \beta(R_{n-1} -\gamma)$ and  solve the equation for 
$$
\gamma - \beta\gamma = \frac{3}{1-\alpha}
$$
gives $\gamma=-1$ thus
$$
R_n + 1 = \beta(R_{n-1} + 1) + \frac{\alpha^{n+1}}{2}
$$
which reduce to 
$$
\frac{R_n + 1}{\alpha^n} - \delta = \frac{\beta}{\alpha}(\frac{R_{n-1} + 1}{\alpha^{n-1}} - \delta)
$$
where $\delta$ satisfies

$$
\delta - \frac{\beta\delta}{\alpha} = \frac{\alpha}{2}
$$
which gives $\delta=\frac{\alpha^2}{2(\alpha-\beta)}$.

Thus we have

$$
\frac{R_n + 1}{\alpha^n} - \frac{\alpha^2}{2(\alpha-\beta)} = \frac{\beta^n}{\alpha^n}(1-\frac{\alpha^2}{2(\alpha-\beta)})
$$
gives

$$
R_n = \frac{\alpha^{n+2}}{2(\alpha-\beta)} + \frac{\beta^{n+2}}{2(\beta-\alpha)} - 1
$$
and

$$
T_n = \frac{\alpha^{n+1}}{\alpha-\beta} + \frac{\beta^{n+1}}{\beta-\alpha} - 1
$$


# No move from 0 to 2

>[!Question]
> Finally, suppose your only restriction is that you may never move a disk directly from peg $0$ to peg $2$. 
> 
> Describe an algorithm to solve this version of the puzzle in as few moves as possible. 
> 
> How many moves does your algorithm make?
> 
> (Hint: Matrices! This variant is considerably harder to analyse than the other two.)


# Block from 2 to 0


![[Honoi-block-0-to-2.png]]
