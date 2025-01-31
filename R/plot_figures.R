format_plot_data <- function(quantile_data,start_date){
  tibble::tibble(
    lower = quantile_data[1,],
    mid_lower = quantile_data[2,],
    mid_upper = quantile_data[3,],
    upper = quantile_data[4,]
  ) %>%
    mutate(diagnosis_date = row_number() + start_date-1)
}


plot_incidence <- function(summary_fit, daily_data) {
  I_plot_data <- summary_fit$I_T %>%
    format_plot_data(., min(daily_data$diagnosis_date))

  daily_data <- daily_data %>% filter(diagnosis_date %in% I_plot_data$diagnosis_date)
  ggplot(daily_data, aes(x = diagnosis_date)) +
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
    # geom_vline(xintercept = aggregated_data$week-1,linetype="dotted") +
    geom_point(aes(y = cases), size = 0.5)  +
    scale_y_continuous("Daily Incidence") +
    scale_x_date("Day") +
    theme_classic() +
    theme(text = element_text(size = 16))+
    theme(
      text = element_text(size = 16),
      legend.title = element_blank(),
      legend.text = element_text(size = 10, ),
      legend.position = "inside",
      legend.justification = c(0.90, 0.95)
    )
}

plot_effective_reproduction_number <- function(summary_fit, summary_daily, daily_data) {
  R_plot_data <- summary_fit$R_T %>%
    format_plot_data(., min(daily_data$diagnosis_date)) %>%
    mutate(Model = "Reduced data")

  R_plot_daily_data <- summary_daily$R_T %>%
    format_plot_data(., min(daily_data$diagnosis_date)) %>%
    mutate(Model = "Daily data") %>%
    filter(diagnosis_date %in% R_plot_data$diagnosis_date)

  R_plot_data = bind_rows(R_plot_data, R_plot_daily_data)
  #
  ggplot(R_plot_data, aes(x = diagnosis_date, fill = Model)) +
    geom_ribbon(aes(ymin = mid_lower, ymax = mid_upper), alpha = 0.8) +
    geom_ribbon(data = R_plot_data,
                aes(ymin = lower, ymax = upper),
                alpha = 0.5) +
    scale_y_continuous("R Effective",limits = c(0.0,max(R_plot_data$mid_upper)+0.5)) +
    scale_x_date("Day",limits = c(min(R_plot_data$diagnosis_date),max(R_plot_data$diagnosis_date)+5)) +
    theme_classic() +
    # theme(text = element_text(size = 16))+
    scale_fill_manual(values = c("#d95f02", "#7570b3")) +
    theme(
      text = element_text(size = 16),
      legend.title = element_blank(),
      legend.text = element_text(size = 8),
      legend.position = "inside",
      legend.justification = c(0.95, 0.95),
      legend.key.size = unit(3,"mm")
      )
}
