daily_model <- function(daily_data,g,alpha,beta,Area){
  lambda = -log(Area / 100)
  daily_stan_data <- list(
    N_days = nrow(daily_data),
    D = daily_data$cases,
    N_generation = length(g),
    g = g,
    R0 = 1.0,
    I0 = 100,
    alpha = alpha,
    beta = beta,
    lambda = lambda
  )

  stan(
    file = "daily.stan",
    data = daily_stan_data,
    warmup = 2000,
    iter = 8000,
    chains = 4,
    cores = 4
  )
}
#
#   daily_summary <- summarise_daily_model_fit(daily_fit, c(0.025, 0.25, 0.75, 0.975))
#
#
#   R_plot_data_daily <- daily_summary$R_T %>%
#     format_plot_data(., min(daily_data$diagnosis_date))
#
#
#
#   figure2_compare <- figure2 +
#     geom_ribbon(
#       data = R_plot_data_daily,
#       aes(ymin = mid_lower, ymax = mid_upper),
#       alpha = 0.8,
#       fill = "#d95f02"
#     ) +
#     geom_ribbon(
#       data = R_plot_data_daily,
#       aes(ymin = lower, ymax = upper),
#       alpha = 0.5,
#       fill = "#d95f02"
#     )
#
#   figure2_compare_forecast <- figure2_compare + geom_ribbon(
#     data = Rforecast_plot_data,
#     aes(ymin = mid_lower, ymax = mid_upper),
#     alpha = 0.8,
#     fill = "#1b9e77"
#   ) +
#     geom_ribbon(
#       data = Rforecast_plot_data,
#       aes(ymin = lower, ymax = upper),
#       alpha = 0.5,
#       fill = "#1b9e77"
#     )
#   phi_plot <- mcmc_areas(
#     daily_fit,
#     "phi",
#     transformations = function(x)
#       1 / x
#   ) + scale_x_continuous("Overdispersion")
#   ggsave(
#     paste0(
#       "final_pictures/RealData/DailyPhiWindow",
#       window_size,
#       "Filter",
#       filter_size,
#       "Area",
#       Area,
#       ".png"
#     ),
#     plot = phi_plot,
#     width = 89,
#     height = 60,
#     units = "mm",
#     dpi = 300,
#     create.dir = TRUE
#   )
#   ggsave(
#     paste0(
#       "final_pictures/RealData/ReproductionWindow",
#       window_size,
#       "Filter",
#       filter_size,
#       "Area",
#       Area,
#       "Compare.png"
#     ),
#     plot = figure2_compare,
#     width = 89,
#     height = 60,
#     units = "mm",
#     dpi = 300,
#     create.dir = TRUE
#   )
#   ggsave(
#     paste0(
#       "final_pictures/RealData/ReproductionWindow",
#       window_size,
#       "Filter",
#       filter_size,
#       "Area",
#       Area,
#       "CompareForecast.png"
#     ),
#     plot = figure2_compare_forecast,
#     width = 89,
#     height = 60,
#     units = "mm",
#     dpi = 300,
#     create.dir = TRUE
#   )
# }
