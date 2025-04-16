format_plot_data <- function(quantile_data,start_date){
  tibble::tibble(
    lower = quantile_data[1,],
    mid_lower = quantile_data[2,],
    mid_upper = quantile_data[3,],
    upper = quantile_data[4,],
  ) %>%
    mutate(diagnosis_date = row_number() + start_date-1)
}


plot_incidence <- function(summary_fit, daily_data, plotting_regions = NULL) {
  # model =model_data
  I_plot_data <- summary_fit$I_T %>%
    format_plot_data(., min(daily_data$diagnosis_date))



  daily_data <- daily_data %>% filter(diagnosis_date %in% I_plot_data$diagnosis_date)
  g<- ggplot(daily_data, aes(x = diagnosis_date))

  if(!is.null(plotting_regions)){
    agg_data <- plotting_regions$reported |>
      mutate(finish_date = finish+ min(daily_data$diagnosis_date)-1)
    missing_data <- plotting_regions$missing |>
      mutate(finish_date = finish + min(daily_data$diagnosis_date)-1,
             start_date = start + min(daily_data$diagnosis_date)-1)

    if(nrow(missing_data)!=0){
      for(i in 1:nrow(missing_data)){
        g <- g + annotate("rect",xmin = missing_data$start_date[i],xmax=missing_data$finish_date[i],ymin = -Inf,ymax = Inf,alpha = 0.2)
      }
    }

    g <- g +
      geom_vline(xintercept = agg_data$finish_date,linetype="dotted",color = "grey")
  }
  g<-g+
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
    geom_point(aes(y = cases), size = 0.5)  +
    scale_y_continuous("Daily Incidence") +
    theme_classic() +
    theme(text = element_text(size = 16))+
    theme(
      text = element_text(size = 16),
      legend.title = element_blank(),
      legend.text = element_text(size = 10, ),
      legend.position = "inside",
      legend.justification = c(0.90, 0.95)
    )

  if(lubridate::is.Date(min(daily_data$diagnosis_date))){
    g<- g+ scale_x_date("Day")
  }else{
    g<- g+ scale_x_continuous("Day")
  }

  return(g)
}

plot_effective_reproduction_number <- function(summary_fit, summary_daily, daily_data, plotting_regions = NULL) {
  R_plot_data <- summary_fit$R_T %>%
    format_plot_data(., min(daily_data$diagnosis_date)) %>%
    mutate(Model = "Reduced data")

  R_plot_daily_data <- summary_daily$R_T %>%
    format_plot_data(., min(daily_data$diagnosis_date)) %>%
    mutate(Model = "Daily data") %>%
    filter(diagnosis_date %in% R_plot_data$diagnosis_date)

  R_plot_data = bind_rows(R_plot_data, R_plot_daily_data)
  g<-ggplot(R_plot_data, aes(x = diagnosis_date, fill = Model))

  if(!is.null(plotting_regions)){
    agg_data <- plotting_regions$reported |>
      mutate(finish_date = finish+ min(daily_data$diagnosis_date)-1)
    missing_data <- plotting_regions$missing |>
      mutate(finish_date = finish + min(daily_data$diagnosis_date)-1,
             start_date = start + min(daily_data$diagnosis_date)-1)

    if(nrow(missing_data)!=0){
      for(i in 1:nrow(missing_data)){
        g <- g + annotate("rect",xmin = missing_data$start_date[i],xmax=missing_data$finish_date[i],ymin = -Inf,ymax = Inf,alpha = 0.2)
      }
    }

    g <- g +
      geom_vline(xintercept = agg_data$finish_date,linetype="dotted",color = "grey")
  }

  g<- g+
    geom_ribbon(aes(ymin = mid_lower, ymax = mid_upper), alpha = 0.8) +
    geom_ribbon(data = R_plot_data,
                aes(ymin = lower, ymax = upper),
                alpha = 0.5) +
    scale_y_continuous("R Effective",limits = c(0.0,max(R_plot_data$upper)+0.1)) +
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
  if(lubridate::is.Date(min(daily_data$diagnosis_date))){
    g<- g+  scale_x_date("Day",limits = c(min(R_plot_data$diagnosis_date),max(R_plot_data$diagnosis_date)+5))
  }else{
    g<- g+  scale_x_continuous("Day",limits = c(min(R_plot_data$diagnosis_date),max(R_plot_data$diagnosis_date)+5))
  }
  return(g)
}


plot_simulated_effective_reproduction_number <- function(summary_fit, daily_data,plotting_regions = NULL) {
  R_plot_data <- summary_fit$R_T %>%
    format_plot_data(., min(daily_data$diagnosis_date))

  g<-ggplot(R_plot_data, aes(x = diagnosis_date))

  if(!is.null(plotting_regions)){
    agg_data <- plotting_regions$reported |>
      mutate(finish_date = finish+ min(daily_data$diagnosis_date)-1)
    missing_data <- plotting_regions$missing |>
      mutate(finish_date = finish + min(daily_data$diagnosis_date)-1,
             start_date = start + min(daily_data$diagnosis_date)-1)

    if(nrow(missing_data)!=0){
      for(i in 1:nrow(missing_data)){
        g <- g + annotate("rect",xmin = missing_data$start_date[i],xmax=missing_data$finish_date[i],ymin = -Inf,ymax = Inf,alpha = 0.2)
      }
    }

    g <- g +
      geom_vline(xintercept = agg_data$finish_date,linetype="dotted",color = "grey")
  }

  g<- g+
    geom_ribbon(aes(ymin = mid_lower, ymax = mid_upper), alpha = 0.8,fill = "#7570b3") +
    geom_ribbon(data = R_plot_data,
                aes(ymin = lower, ymax = upper),
                alpha = 0.5,fill = "#7570b3") +
    scale_y_continuous("R Effective",limits = c(0.0,max(R_plot_data$upper)+0.1)) +
    geom_point(data = daily_data,aes(y = R_t))+
    theme_classic() +
    theme(
      text = element_text(size = 16),
      legend.title = element_blank(),
      legend.text = element_text(size = 8),
      legend.position = "inside",
      legend.justification = c(0.95, 0.95),
      legend.key.size = unit(3,"mm")
    )
  if(lubridate::is.Date(min(daily_data$diagnosis_date))){
    g<- g+  scale_x_date("Day",limits = c(min(R_plot_data$diagnosis_date),max(R_plot_data$diagnosis_date)+5))
  }else{
    g<- g+  scale_x_continuous("Day",limits = c(min(R_plot_data$diagnosis_date),max(R_plot_data$diagnosis_date)+5))
  }
  return(g)
}

plot_bayes_estimate <- function(summary_fit, daily_data) {
  I_plot_data <- tibble::tibble(
    I_T_estimate = summary_fit$I_T_estimate) %>%
    mutate(diagnosis_date = row_number() +  min(daily_data$diagnosis_date) -1)
  daily_data <- daily_data %>% filter(diagnosis_date %in% I_plot_data$diagnosis_date)
#
  corr_plot <- inner_join(daily_data,I_plot_data)
#  ggplot(corr_plot,aes(x = I_T_estimate,y = cases)) +
#    geom_point() +
#    geom_abline(slope = 1,linetype = "dashed")+
#      scale_x_continuous("Bayes Estimator") +
#      scale_y_continuous("Daily Incidence") +
#      theme_classic() +
#      theme(text = element_text(size = 16))+
#      theme(
#        text = element_text(size = 16),
#        legend.title = element_blank(),
#        legend.text = element_text(size = 10, ),
#        legend.position = "inside",
#        legend.justification = c(0.90, 0.95)
#      )

  ggplot(corr_plot, aes(x = diagnosis_date)) +
    geom_line(
      data = I_plot_data,
      aes(y = I_T_estimate),
      alpha = 0.8,
      fill = "#7570b3"
    ) +
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

plot_zoomed_incidence <- function(g1){
  g2 <- g1 + coord_cartesian(xlim = c(34.5,50.5),ylim = c(0,75)) + scale_x_continuous("") + scale_y_continuous("") + theme(text = element_text(size=8),
                                                                                                                           # panel.background = element_blank(),
                                                                                                                           plot.background = element_blank())
  ggdraw() +
    draw_plot(g1 + # Inset (position & size)
                geom_rect(xmin = 34.5, xmax = 50.5, ymin = 0, ymax = 75,
                          fill = NA, color = "black", linetype = "dashed"), 0, 0, 1, 1) +  # Main plot (full size)
    draw_plot(g2, 0.45, 0.4, 0.4, 0.4) # Inset (position & size)
}
