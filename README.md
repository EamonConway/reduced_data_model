# Reduced Data model

It is quite easy to run the reduced data model.
First you must construct a data structure that consists of
 ```
  stan_data <- list(
    N_data, # Number of data points to use.
    D, # Vector of measured aggregated data points.
    D_time, # Vector of time points that data is reported.
    D_window_size, # Vector of aggregation window sizes,
    N_generation, # Length of generation interval
    g, # Generation Interval
    forecast_length, # How far past last data point to forecast (not really used)
    I0, # Prior for initial condition
    R0, # Prior estiamte for R0
    alpha, # Prior parameters
    beta, #Prior parameter
    lambda # Prior parameter
  )
```

The model can then be called by invoking stan.
```
  fit <- rstan::stan(
    file = "reduced_data.stan",
    data = stan_data,
    warmup = 8000,
    iter = 20000,
    chains = 4,
    cores = 4,
    control = list(max_treedepth = 15,
                   adapt_delta = 0.95)
  )
```

The parameters can be tuned so that you get the appropriate convergence of the hamiltonian monte carlo method.
