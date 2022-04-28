#general-additive-model  #boosting #algorithm 

suppose an additive model
$$
f(x) = \sum_{m=1}^M \beta_m b(x|\gamma_m)
$$
The **Forward stagewise Additive Modelling** fixed previous regression terms when trying to add new terms.

> [!ALGORITHM]   Algorithm: Forward Stagewise Boosting
> 1. Initialise $f_0(x) = 0$;
> 2. For $m \gets 1$ to $M$:
>        1.  Compute
>            $$ (\beta_m, \gamma_m) = \arg \min_{(\beta,\gamma)} \sum_{i=1}^N L\big(y_i, f_{m-1}(x_i) + \beta b(x_i|\gamma) \big) $$
>        2. Set $f_m(x) = f_{m-1}(x) + \beta_m b(x|\gamma_m)$ 
        

