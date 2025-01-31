epiestim_estimate<-function(data){
  EpiEstim::estimate_R_agg(
    data$D,
    method = "parametric_si",
    iter = 50L,
    config = EpiEstim::make_config(list(mean_si = 3.6, std_si = 3.1))
  )
}

get_epiestim_rt <-function(result){
  tibble(
    date = 0.5*(result$R[,"t_start"] + result$R[,"t_end"]),
    start_date = result$R[, "t_start"] ,
    end_date = (result$R[, "t_end"]) ,
    lower = result$R[, "Quantile.0.025(R)"],
    mid_lower = result$R[, "Quantile.0.25(R)"],
    mid_upper = result$R[, "Quantile.0.75(R)"],
    upper = result$R[, "Quantile.0.975(R)"],
    Model = "EpiEstim"
  )
}

get_epiestim_incidence <-function(result){
  tibble(
    date = result$dates - 1,
    incidence = result$I,
    Model = "EpiEstim"
  )
}

epiestim_plot_incidence <-function(incidence, summary, data){
  incidence <- incidence %>% mutate(diagnosis_date = min(data$diagnosis_date) + date)
  plot_incidence(summary,data) + geom_line(data = incidence,aes(x = diagnosis_date, y = incidence),color = "#d95f02")
}

epiestim_plot_rt <-function(rt, summary, data){
  rt <- rt %>% mutate(diagnosis_date = date -1 + min(data$diagnosis_date),) %>%
    select(-date)

  R_plot_data <- summary$R_T %>%
    format_plot_data(., min(data$diagnosis_date)) %>%
    mutate(Model = "Reduced data")

  R_plot_data = bind_rows(R_plot_data, rt)
  R_plot_data$Model <- factor(R_plot_data$Model, levels = c("Reduced data", "EpiEstim"))

  ggplot(R_plot_data, aes(x = diagnosis_date, fill = Model)) +
    geom_ribbon(aes(ymin = mid_lower, ymax = mid_upper), alpha = 0.8) +
    geom_ribbon(data = R_plot_data,
                aes(ymin = lower, ymax = upper),
                alpha = 0.5) +
    scale_y_continuous("R Effective") +
    scale_x_date("Day") +
    theme_classic() +
    theme(text = element_text(size = 16))+
    scale_fill_manual(values = c("#7570b3","#d95f02")) +
    theme(
      text = element_text(size = 16),
      legend.title = element_blank(),
      legend.text = element_text(size = 10, ),
      legend.position = "inside",
      legend.justification = c(0.90, 0.95)
    )
}
