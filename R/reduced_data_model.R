reduced_data_model <- function(data,
                               g,
                               Area,
                               window_size,
                               filter_size,
                               alpha,
                               beta) {
  lambda = -log(Area / 100)
  if (is.numeric(window_size)) {
    windows <- seq(0, nrow(data), by = window_size)
  } else {
    windows <- c(0, cumsum(sample(1:10, 40, replace = TRUE)))
  }
  windows <- windows[which(windows < nrow(data))]

  # Real Data
  real_data <- data %>%
    mutate(date_group = cut(diagnosis_date, breaks = min(diagnosis_date) + windows) %>%
             as_date())

  aggregated_real_data <- real_data %>%
    group_by(date_group) %>%
    summarise(WeeklyIncidence = sum(cases), window = n()) %>%
    filter(!is.na(date_group)) %>%
    mutate(
      day = as.integer(date_group) + window,
      week = as_date(day),
      day = day - min(as.integer(date_group))
    )

  rows_to_keep<- sample(1:nrow(aggregated_real_data),floor(nrow(aggregated_real_data)/filter_size))
  aggregated_real_data <- aggregated_real_data %>%
    # filter(row_number() %in% seq(1, n(), by = filter_size))
    filter(row_number() %in% rows_to_keep)

  daily_data <- real_data %>%
    filter(diagnosis_date < max(aggregated_real_data$week))

  stan_data <- list(
    N_data = nrow(aggregated_real_data),
    D = aggregated_real_data$WeeklyIncidence,
    D_time = aggregated_real_data$day,
    D_window_size = aggregated_real_data$window,
    N_generation = length(g),
    g = g,
    forecast_length = 7,
    I0 = 100,
    R0 = 1.0,
    alpha = alpha,
    beta = beta,
    lambda = lambda
  )

  fit <- rstan::stan(
    file = "reduced_data.stan",
    # file = "binomial_aggregate.stan",
    data = stan_data,
    warmup = 8000,
    iter = 20000,
    chains = 4,
    cores = 4,
    control = list(max_treedepth = 15,
                   adapt_delta = 0.95)
  )
  return(list(model_fit = fit, model_data = stan_data))
}
