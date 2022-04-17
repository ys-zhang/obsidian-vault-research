#statistics 

The Delta Method solves the problem of **calculating asymptotic distribution**. 

>[!CITE]-   Reference
>
> Casella, Berger, Chapter 5.5.4 The Delta Method


# First Order Delta Method

###### Univariant

Suppose we have a sequence of univariant random variable $Y_n$ have the asymptotic distribution $\mathscr N(\mu, \sigma^2)$ i.e., 
$$
\sqrt{n}\big( Y_n - \mu \big) \overset{d} \longrightarrow \mathscr N(0, \sigma^2)
$$
for any _differentiable_ function $g$ satisfies
$$
g'(\mu)\ne 0
$$
Then $g(Y_n)$ have asymptotic distribution $\mathscr N\big(g(\mu), \sigma^2 g'(\mu)^2\big)$, i.e.,

$$
\sqrt{n}\big[g(Y_n) - g(\mu) \big] \overset{d} \longrightarrow \mathscr N(0, \sigma^2 g'(\mu)^2)
$$

>[!NOTE]- PROOF
> 
> Consider Taylor Expansion
> $$ g(Y_n) = g(\mu) + g'(\mu)(Y_n - \mu) + O(|Y_n - \mu|^2) $$
> thus
> $$ \sqrt{n}\big[g(Y_n) - g(\mu)\big] = \sqrt{n}g'(\mu)(Y_n - \mu) + O(Y_n - \mu)  \sqrt{n}\big( Y_n - \mu \big) $$
>  since $Y_n -\mu \xrightarrow{p} 0$  and  $\sqrt{n}\big( Y_n - \mu \big) \overset{d} \longrightarrow \mathscr N(0, \sigma^2)$, by [[Slutsky's Theorem]] we have the remainder 
>  $$ O(Y_n - \mu)  \sqrt{n}\big( Y_n - \mu \big) \overset{d} \longrightarrow 0$$
>  Apply the [[Slutsky's Theorem]] once more we have the result
>  $$ \sqrt{n}\big[g(Y_n) - g(\mu) \big] \overset{d} \longrightarrow \mathscr N(0, \sigma^2 g'(\mu)^2) $$



###### Multivariant

Suppose 
$$
\sqrt{n}\big( \mathbf{Y}_n - \mathbf{\theta} \big) \overset{d} \longrightarrow \mathscr N(0, \Sigma)
$$
$g: \mathbb R^p \to \mathbb R$ differentiable at $\theta \in \mathbb R^p$ and $\nabla g(\theta) \ne 0$  then

$$
\sqrt{n}\big[g(\mathbf{Y}_n) - g(\mathbf{\theta})\big] 
\overset{d} \longrightarrow \mathscr N\big(\;0,\; (\nabla g(\theta))^T\Sigma(\nabla g(\theta)\;\big)
$$


# Second Order Delta Method

Suppose we have a sequence of univariant random variable $Y_n$ have the asymptotic distribution $\mathscr N(\mu, \sigma^2)$ i.e., 
$$
\sqrt{n}\big( Y_n - \mu \big) \overset{d} \longrightarrow \mathscr N(0, \sigma^2)
$$
for any _differentiable_ function $g$ satisfies

$$
g'(\mu) = 0 \text{ and } g''(\mu)\ne 0
$$
Then

$$
\sqrt{n}\big[g(\mathbf{Y}_n) - g(\mathbf{\theta})\big] 
\overset{d} \longrightarrow \frac{\sigma^2g''(\theta)}{2} \chi^2_1
$$



