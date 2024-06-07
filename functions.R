library(dplyr)
library(ggplot2)
library(readr)
library(StanHeaders)
library(readr)
library(tibble)
library(ggplot2)
library(tidyr)
library(rstan)
library(dplyr)
library(lubridate)
library(magrittr)
library(bayesplot)
library(lubridate)
generate_simulated_incidence <- function(R_T,generation_interval,initial_incidence){
  n_days <- length(R_t)
  I_t <- rep(initial_incidence, n_days + 1)
  n_generation_interval <- length(generation_interval)
  for(t in 2:(n_days+1)){
    current_foi = 0.0;
    for(s in 1:n_generation_interval){
      if((t-s)>0){
        current_foi = current_foi + generation_interval[s]*I_t[t-s];
      }
    }
    I_t[t] <- rpois(1,R_t[t-1]*current_foi)
  }
  return(I_t[-1])
}

summarise_model_fit <- function(model_fit, quantiles){
  results <- rstan::extract(model_fit)
  return(list(
  R_T = apply(results$R_T,2,quantile, probs = quantiles),
  R_forecast = apply(results$forecast_RT,2,quantile, probs = quantiles),
  I_T = apply(results$I_T,2,quantile, probs = quantiles),
  I_forecast = apply(results$forecast_IT,2,quantile, probs = quantiles)
  ))
}

summarise_daily_model_fit <- function(model_fit, quantiles){
  results <- rstan::extract(model_fit)
  return(list(
    R_T = apply(results$R_T,2,quantile, probs = quantiles)
  ))
}

format_plot_data <- function(quantile_data,start_date){
  tibble(
    lower = quantile_data[1,],
    mid_lower = quantile_data[2,],
    mid_upper = quantile_data[3,],
    upper = quantile_data[4,]
  ) %>%
    mutate(diagnosis_date = row_number() + start_date-1)
}