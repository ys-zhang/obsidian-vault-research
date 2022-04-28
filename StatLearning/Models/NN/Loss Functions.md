#machine-learning 

# Exponential Loss


$$
L(y, f(x)) = \exp(-y\cdot f(x))
$$
where $y\in \{-1, 1\}$ 

## Properties

##### Population Minimiser
$$
f^*(x) = \arg\min_{f} E_{Y|x}[e^{-Yf(x)}] = \frac{1}{2}\log 
\frac{\Pr(Y=1|x)}{\Pr(Y=-1|x)}
$$
and equivalently
$$
\Pr(Y=1|x) = \frac{1}{1+e^{-2f^*(x)}}
$$
It shares the same population minimiser with **binary cross-entropy**, however, it penalty more on negative value (miss-classification) and less on positive value, thus are more robust than the _binary cross-entropy_.

![[Pasted image 20220426000801.png]]