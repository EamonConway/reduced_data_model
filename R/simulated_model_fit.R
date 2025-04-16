simulated_model_fit <- function(simulated_data,
                                 g,
                                 Area,
                                 window_size,
                                 filter_size,
                                 alpha,
                                 beta){
  lambda = -log(Area / 100)
if(is.numeric(window_size)){
  windows <- seq(0,100,by=window_size)
} else {
  windows<- c(0,cumsum(sample(1:10,40,replace = TRUE)))
}
windows <- windows[which(windows < 100)]

# Data
data <- simulated_data %>%
  select(diagnosis_date,Total_case_count) %>%
  group_by(diagnosis_date) %>%
  summarise(cases = sum(Total_case_count)) %>%
  mutate(date_group = cut(diagnosis_date,
                          breaks = 0 + windows,include.lowest = TRUE))

  aggregated_data_full <- data %>%
    group_by(date_group) %>%
    summarise(AggIncidence = sum(cases),window = n()) %>%
    filter(!is.na(date_group)) %>%
    mutate(day2 = as.integer(date_group) + window,
           day = cumsum(window),
           week = (day))
  rows_to_keep <- sample(1:nrow(data),floor(nrow(data)/filter_size))

  aggregated_data <- aggregated_data_full %>%
    filter(row_number() %in% rows_to_keep)

  daily_data <- simulated_data %>%
    filter(diagnosis_date < max(aggregated_data$week))

  IncidenceInitialCondition <- rep(0,length(g))
  IncidenceInitialCondition[1] <- 100
  stan_data <- list(
    N_data = nrow(aggregated_data),
    D = aggregated_data$AggIncidence,
    D_time = aggregated_data$day,
    D_window_size = aggregated_data$window,
    N_generation = length(g),
    g = g,
    forecast_length = 7,
    I_initial = IncidenceInitialCondition,
    R_initial = 1.0,
    alpha = alpha,
    beta = beta,
    lambda = lambda
  )

  fit <- rstan::stan(file = "reduced_data_known_init.stan",
                     data = stan_data,
                     iter = 10000,
                     chains = 4,
                     cores = 4,
                     control = list(max_treedepth = 12))
  return(list(model_fit = fit, model_data = stan_data))
}
