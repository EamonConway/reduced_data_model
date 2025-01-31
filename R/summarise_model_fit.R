summarise_model_fit <- function(model_fit, quantiles){
  results <- rstan::extract(model_fit)
  return(list(
    R_T = apply(results$R_T,2,quantile, probs = quantiles),
    R_forecast = apply(results$forecast_RT,2,quantile, probs = quantiles),
    I_T = apply(results$I_T,2,quantile, probs = quantiles),
    I_forecast = apply(results$forecast_IT,2,quantile, probs = quantiles)
  ))
}
