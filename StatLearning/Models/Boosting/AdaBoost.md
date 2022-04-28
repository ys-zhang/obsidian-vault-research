#boosting  #general-additive-model  


# AdaBoost.M1

>[!ALGORITHM]  Algorithm:  Discrete AdaBoost
>1. Initialise the observation weights $w_i = 1/N$, $i = 1,2,\dots,N$;
>2. For $m \gets 1$  to $M$:
>    1. Fit a classifier $G_m(x)$ to the training data using weights $w_i$
>    2. Compute fitting error
>         $$ \text{err}_m = \frac{\sum_{i=1}^N w_i \mathbb I(y_i\ne G_m(x_i))}{\sum_{i=1}^N w_i} $$
>    3. Compute model weights
>         $$\alpha_m = \log \frac{1-\text{err}_m}{\text{err}_m}$$ 
>    4. Update sample weights
>         $$ w_i \gets w_i \exp[\alpha_m \mathbb I(y_i\ne G_m(x_i))] $$
> 3. Output
>     $$ G(x) = \text{sign} \big[ \sum_{m=1}^M \alpha_m G_m(x) \big ] $$

# Forward Stagewise Additive Modelling

>[!TLDR]
> AdaBoost is equivalent to a Forward Stagewise Additive Modelling approach using
> [[Loss Functions#Exponential Loss|Exponential Loss]].


The forward stagewise algorithm is used for modelling a general additive model:

$$
f(x) = \sum_{m=1}^M \beta_m b(x|\gamma_m)
$$
At each iteration it fixes previously estimated $\beta_i$ with $i<m$,  estimates $\beta_m$ and $\gamma_m$ by 
$$
(\beta_m, \gamma_m) = \arg \min_{(\beta,\gamma)} \sum_{i=1}^N L\big(y_i, f_{m-1}(x_i) + \beta b(x_i|\gamma) \big)
$$
