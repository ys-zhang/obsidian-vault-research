# Logistic Growth Model

$$
y = \frac{\theta_1}{1+\theta_2\exp(-\theta_3t)} + \varepsilon
$$

# Gompertz Growth Model

$$
y = \theta_1 \exp(-\theta_2 e^{-\theta_3t}) + \varepsilon
$$

# Weibull Growth Model

$$
y = \theta_1 - \theta_2\exp(-\theta_3t^{\theta_4}) + \varepsilon
$$

# R Code

 
```r
puro.model <- nls(y∼t1*x/(t2+x), start=list(t1=205,t2=.08), data =puro)  
summary(puro.model)
yhat <- fitted(puro.model) 
e    <- residuals(puro.model)
```

