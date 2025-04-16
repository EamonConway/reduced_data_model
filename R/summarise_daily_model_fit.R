summarise_daily_model_fit <- function(model_fit, quantiles){
  results <- rstan::extract(model_fit)
  return(list(
    R_T = apply(results$R_T,2,quantile, probs = quantiles)
  ))
}
