[Stationary process - Wikipedia](https://en.wikipedia.org/wiki/Stationary_process)

Time series with _trends_, or with _seasonality_, are **not stationary**.



# strong-sense stationary

A stochastic process $X_t$ is a strictly stationary process if the CDF of $X$ satisfies
$$
F_X(x_{t_1 + \tau}, x_{t_2 + \tau}, \cdots, x_{t_n + \tau}) = 
F_X(x_{t_1}, x_{t_2 }, \cdots, x_{t_n}) \;\; \forall n, (t_1,\cdots, t_n), \tau
$$



# weak-sense stationarity

only require that 1st [moment](https://en.wikipedia.org/wiki/Moment_(mathematics) "Moment (mathematics)") (i.e. the mean) and [autocovariance](https://en.wikipedia.org/wiki/Autocovariance "Autocovariance") do not vary with respect to time and that the 2nd moment is finite for all times.



