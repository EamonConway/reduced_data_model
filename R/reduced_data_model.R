reduced_data_model <- function(data, g, Area, window_size, filter_size,alpha,beta) {
  lambda = -log(Area / 100)
  if (is.numeric(window_size)) {
    windows <- seq(0, 100, by = window_size)
  } else {
    windows <- c(0, cumsum(sample(1:10, 40, replace = TRUE)))
  }
  windows <- windows[which(windows < 100)]

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
    ) %>%
    filter(row_number() %in% seq(1, n(), by = filter_size))

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
    file = "binomial_aggregate.stan",
    data = stan_data,
    warmup = 2000,
    iter = 8000,
    chains = 4,
    cores = 4,
    control = list(max_treedepth = 12)
  )
 return(list(model_fit = fit,model_data = stan_data))
}

figure_plotting <- function(fit_data, daily_data){
  summary_fit <- summarise_model_fit(fit_data, c(0.025, 0.25, 0.75, 0.975))

  Iforecast_plot_data = summary_fit$I_forecast %>%
    format_plot_data(., max(aggregated_real_data$week))

  Rforecast_plot_data = summary_fit$R_forecast %>%
    format_plot_data(., max(aggregated_real_data$week))

  R_plot_data <- summary_fit$R_T %>%
    format_plot_data(., min(real_data$diagnosis_date))

  I_plot_data <- summary_fit$I_T %>%
    format_plot_data(., min(real_data$diagnosis_date))


  figure1 <- ggplot(daily_data, aes(x = diagnosis_date)) +
    geom_ribbon(
      data = I_plot_data,
      aes(ymin = mid_lower, ymax = mid_upper),
      alpha = 0.8,
      fill = "#7570b3"
    ) +
    geom_ribbon(
      data = I_plot_data,
      aes(ymin = lower, ymax = upper),
      alpha = 0.5,
      fill = "#7570b3"
    ) +
    geom_vline(xintercept = aggregated_real_data$week - 1, linetype =
                 "dotted") +
    geom_point(aes(y = cases), size = 0.5)  +
    scale_y_continuous("Daily Incidence") +
    scale_x_date("Day") +
    theme_classic() +
    theme(text = element_text(size = 16))

  ggsave(
    paste0(
      "final_pictures/RealData/IncidenceWindow",
      window_size,
      "Filter",
      filter_size,
      "Area",
      Area,
      ".png"
    ),
    plot = figure1,
    ,
    width = 89,
    height = 60,
    units = "mm",
    dpi = 300,
    create.dir = TRUE
  )

  figure1 <- figure1 + geom_ribbon(
    data = Iforecast_plot_data,
    aes(ymin = mid_lower, ymax = mid_upper),
    alpha = 0.8,
    fill = "#1b9e77"
  ) +
    geom_ribbon(
      data = Iforecast_plot_data,
      aes(ymin = lower, ymax = upper),
      alpha = 0.5,
      fill = "#1b9e77"
    )
  ggsave(
    paste0(
      "final_pictures/RealData/IncidenceWindow",
      window_size,
      "Filter",
      filter_size,
      "Area",
      Area,
      "Forecast.png"
    ),
    plot = figure1,
    ,
    width = 89,
    height = 60,
    units = "mm",
    dpi = 300,
    create.dir = TRUE
  )

  figure2 <- ggplot(R_plot_data, aes(x = diagnosis_date)) +
    geom_ribbon(aes(ymin = mid_lower, ymax = mid_upper),
                alpha = 0.8,
                fill = "#7570b3") +
    geom_ribbon(
      data = R_plot_data,
      aes(ymin = lower, ymax = upper),
      alpha = 0.5,
      fill = "#7570b3"
    ) +
    geom_vline(xintercept = aggregated_real_data$week - 1, linetype =
                 "dotted") +
    scale_y_continuous("R Effective") +
    scale_x_date("Day") +
    theme_classic() +
    theme(text = element_text(size = 16))

  phi_plot <- mcmc_areas(
    fit,
    "phi",
    transformations = function(x)
      1 / x
  ) + scale_x_continuous("Overdispersion")
  ggsave(
    paste0(
      "final_pictures/RealData/PhiWindow",
      window_size,
      "Filter",
      filter_size,
      "Area",
      Area,
      ".png"
    ),
    plot = phi_plot,
    width = 89,
    height = 60,
    units = "mm",
    dpi = 300,
    create.dir = TRUE
  )
  ggsave(
    paste0(
      "final_pictures/RealData/ReproductionWindow",
      window_size,
      "Filter",
      filter_size,
      "Area",
      Area,
      ".png"
    ),
    plot = figure2,
    width = 89,
    height = 60,
    units = "mm",
    dpi = 300,
    create.dir = TRUE
  )

  figure2_forecast <- figure2 + geom_ribbon(
    data = Rforecast_plot_data,
    aes(ymin = mid_lower, ymax = mid_upper),
    alpha = 0.8,
    fill = "#1b9e77"
  ) +
    geom_ribbon(
      data = Rforecast_plot_data,
      aes(ymin = lower, ymax = upper),
      alpha = 0.5,
      fill = "#1b9e77"
    )
  ggsave(
    paste0(
      "final_pictures/RealData/ReproductionWindow",
      window_size,
      "Filter",
      filter_size ,
      "Area",
      Area,
      "Forecast.png"
    ),
    plot = figure2_forecast,
    width = 89,
    height = 60,
    units = "mm",
    dpi = 300,
    create.dir = TRUE
  )
}
