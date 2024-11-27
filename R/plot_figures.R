plot_figures <- function(summary_fit, daily_data) {
  R_plot_data <- summary_fit$R_T %>%
    format_plot_data(., min(daily_data$diagnosis_date))

  I_plot_data <- summary_fit$I_T %>%
    format_plot_data(., min(daily_data$diagnosis_date))

  daily_data <- daily_data %>% filter(diagnosis_date %in% I_plot_data$diagnosis_date)
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
    #   geom_vline(xintercept = aggregated_real_data$week - 1, linetype =
    #                "dotted") +
    geom_point(aes(y = cases), size = 0.5)  +
    scale_y_continuous("Daily Incidence") +
    scale_x_date("Day") +
    theme_classic() +
    theme(text = element_text(size = 16))

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
    # geom_vline(xintercept = aggregated_real_data$week - 1, linetype =
    #              "dotted") +
    scale_y_continuous("R Effective") +
    scale_x_date("Day") +
    theme_classic() +
    theme(text = element_text(size = 16))
  return(list(I_t = figure1, R_t = figure2))
  }
  #
  # phi_plot <- mcmc_areas(
  #   fit,
  #   "phi",
  #   transformations = function(x)
  #     1 / x
  # ) + scale_x_continuous("Overdispersion")
  # ggsave(
  #   paste0(
  #     "final_pictures/RealData/PhiWindow",
  #     window_size,
  #     "Filter",
  #     filter_size,
  #     "Area",
  #     Area,
  #     ".png"
  #   ),
  #   plot = phi_plot,
  #   width = 89,
  #   height = 60,
  #   units = "mm",
  #   dpi = 300,
  #   create.dir = TRUE
  # )
  # ggsave(
  #   paste0(
  #     "final_pictures/RealData/ReproductionWindow",
  #     window_size,
  #     "Filter",
  #     filter_size,
  #     "Area",
  #     Area,
  #     ".png"
  #   ),
  #   plot = figure2,
  #   width = 89,
  #   height = 60,
  #   units = "mm",
  #   dpi = 300,
  #   create.dir = TRUE
  # )
  #
  # figure2_forecast <- figure2 + geom_ribbon(
  #   data = Rforecast_plot_data,
  #   aes(ymin = mid_lower, ymax = mid_upper),
  #   alpha = 0.8,
  #   fill = "#1b9e77"
  # ) +
  #   geom_ribbon(
  #     data = Rforecast_plot_data,
  #     aes(ymin = lower, ymax = upper),
  #     alpha = 0.5,
  #     fill = "#1b9e77"
  #   )
  # ggsave(
  #   paste0(
  #     "final_pictures/RealData/ReproductionWindow",
  #     window_size,
  #     "Filter",
  #     filter_size ,
  #     "Area",
  #     Area,
  #     "Forecast.png"
  #   ),
  #   plot = figure2_forecast,
  #   width = 89,
  #   height = 60,
  #   units = "mm",
  #   dpi = 300,
  #   create.dir = TRUE
  # )
# }
