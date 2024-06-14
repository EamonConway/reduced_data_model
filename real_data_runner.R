source("functions.R")
g_mean <- 3.6
g_var <- 3.1^2
g_shape <- g_mean^2/g_var
g_scale <- g_var/g_mean
s <- 0:16
g <- pgamma(s[-1],shape = g_shape, scale = g_scale) - pgamma(s[-length(s)],shape = g_shape, scale = g_scale)
g <- g/sum(g)

for(Area in c(30,40,50,60,70)){
for(window_size in list(1,2,4,7, "Random")){
  alpha = 3
  beta = 0.5
  lambda = -log(Area/100)
  if(is.numeric(window_size)){
    windows <- seq(0,100,by=window_size)
  } else {
    windows<- c(0,cumsum(sample(1:10,40,replace = TRUE)))
  }
  windows <- windows[which(windows < 100)]
  for(filter_size in c(1,2)){


    # Real Data
    real_data <- read_csv("victoria_data.csv") %>%
      select(diagnosis_date,Total_case_count) %>%
      group_by(diagnosis_date) %>%
      summarise(cases = sum(Total_case_count)) %>%
      mutate(diagnosis_date = ymd(diagnosis_date)) %>%
      filter(diagnosis_date < dmy("01-09-2021") + 7*15,
             diagnosis_date >= dmy("01-09-2021")) %>%
      mutate(date_group = cut(diagnosis_date,
                              breaks = min(diagnosis_date) + windows) %>%
               as_date())

    aggregated_data_full <- real_data %>%
      group_by(date_group) %>%
      summarise(WeeklyIncidence = sum(cases),window = n()) %>%
      filter(!is.na(date_group)) %>%
      mutate(day = as.integer(date_group) + window,
             week = as_date(day),
             day = day - min(as.integer(date_group)))

    rows_to_keep <- sample(1:nrow(real_data),floor(nrow(real_data)/filter_size))

    aggregated_data <- aggregated_data_full %>%
      filter(row_number() %in% rows_to_keep)

    missing_data <- aggregated_data_full %>%
      filter(!(row_number() %in% rows_to_keep))

    xmax <- missing_data$week-1
    xmin <- xmax - missing_data$window

    daily_data <- real_data %>%
      filter(diagnosis_date < max(aggregated_data$week))



    daily_data <- real_data %>%
      filter(diagnosis_date < max(aggregated_data$week))

    stan_data <- list(
      N_data = nrow(aggregated_data),
      D = aggregated_data$WeeklyIncidence,
      D_time = aggregated_data$day,
      D_window_size = aggregated_data$window,
      N_generation = length(g),
      g = g,
      forecast_length = 7,
      I0 = 100,
      R0 = 1.0,
      alpha = alpha,
      beta = beta,
      lambda = lambda
    )

    fit <- rstan::stan(file = "reduced_data.stan",
                       data = stan_data,
                       warmup = 2000,
                       iter = 8000,
                       chains = 4,
                       cores = 4,
                       control = list(max_treedepth = 12))

    summary_fit <- summarise_model_fit(fit,
                                       c(0.025,0.25,0.75,0.975))

    Iforecast_plot_data = summary_fit$I_forecast %>%
      format_plot_data(.,max(aggregated_data$week))

    Rforecast_plot_data = summary_fit$R_forecast %>%
      format_plot_data(.,max(aggregated_data$week))

    R_plot_data <- summary_fit$R_T %>%
      format_plot_data(.,min(real_data$diagnosis_date))

    I_plot_data <- summary_fit$I_T %>%
      format_plot_data(.,min(real_data$diagnosis_date))


    figure1 <- ggplot(daily_data, aes(x = diagnosis_date)) +
      geom_ribbon(data = I_plot_data, aes(ymin = mid_lower,ymax = mid_upper),alpha = 0.9,fill = "#7570b3") +
      geom_ribbon(data = I_plot_data, aes(ymin = lower,ymax = upper),alpha = 0.5,fill = "#7570b3") +
      geom_vline(xintercept = aggregated_data$week-1,linetype="dotted") +
      geom_point(aes(y = cases),size = 0.6)  +
      scale_y_continuous("Daily Incidence")+
      scale_x_date("Day") +
      theme_classic() +
      theme(text = element_text(size=16))
    if(length(xmax)!=0){
      for(i in 1:length(xmax)){
        figure1 <- figure1 + annotate("rect",xmin = xmin[i],xmax=xmax[i],ymin = -Inf,ymax = Inf,alpha = 0.2)
      }
    }
    ggsave(paste0("final_pictures/RealData/IncidenceWindow", window_size, "Filter",filter_size,"Area", Area, ".svg"),plot = figure1,, width = 89,height = 60,units="mm",dpi=300,create.dir = TRUE)

    figure2 <- ggplot(R_plot_data,aes(x = diagnosis_date)) +
      geom_ribbon( aes(ymin = mid_lower,ymax = mid_upper),alpha = 0.8,fill = "#7570b3") +
      geom_ribbon(data = R_plot_data, aes(ymin = lower,ymax = upper),alpha = 0.5,fill = "#7570b3") +
      geom_vline(xintercept = aggregated_data$week-1,linetype="dotted") +
      scale_y_continuous("R Effective")+
      coord_cartesian(ylim = c(0,2.5))+
      scale_x_date("Day") +
      theme_classic() +
      theme(text = element_text(size=16))
    if(length(xmax)!=0){
      for(i in 1:length(xmax)){
        figure2 <- figure2 + annotate("rect",xmin = xmin[i],xmax=xmax[i],ymin = -Inf,ymax = Inf,alpha = 0.2)
      }
    }

    phi_plot <- mcmc_areas(fit,"phi",transformations = function(x) 1/x) + scale_x_continuous("Overdispersion")
    ggsave(paste0("final_pictures/RealData/PhiWindow", window_size, "Filter", filter_size,"Area", Area, ".svg"),plot = phi_plot, width = 89,height = 60,units="mm",dpi=300,create.dir = TRUE)
    ggsave(paste0("final_pictures/RealData/ReproductionWindow", window_size, "Filter", filter_size,"Area", Area, ".svg"),plot = figure2, width = 89,height = 60,units="mm",dpi=300,create.dir = TRUE)

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


    daily_fit <- stan(file = "daily.stan",
                      data = daily_stan_data,
                      warmup = 2000,
                      iter = 8000,
                      chains = 4,
                      cores = 4)
    daily_summary <- summarise_daily_model_fit(daily_fit,
                                               c(0.025,0.25,0.75,0.975))


    R_plot_data_combined <- daily_summary$R_T %>%
      format_plot_data(.,min(daily_data$diagnosis_date)) %>% bind_rows(.,R_plot_data,.id = "Model")

     figure2_compare <- ggplot(R_plot_data_combined,aes(x = diagnosis_date)) +
      geom_ribbon( aes(ymin = mid_lower,ymax = mid_upper, fill = Model),alpha = 0.9) +
      geom_ribbon(aes(ymin = lower,ymax = upper, fill = Model),alpha = 0.6) +
      geom_vline(xintercept = aggregated_data$week-1,linetype="dotted") +
      scale_y_continuous("R Effective")+
       coord_cartesian(ylim = c(0,2.5))+
      scale_fill_manual(values = c("2"="#7570b3","1"="#d95f02"),
                        labels = c("1"="Daily data","2"="Reduced data")) +
      scale_x_date("Day") +
      theme_classic() +
      theme(text = element_text(size=16),
            legend.title = element_blank(),
        legend.text = element_text(size = 10,),
        legend.position = "inside",
        legend.justification = c(0.90,0.95),
      )

     if(length(xmax)!=0){
       for(i in 1:length(xmax)){
         figure2_compare <- figure2_compare + annotate("rect",xmin = xmin[i],xmax=xmax[i],ymin = -Inf,ymax = Inf,alpha = 0.2)
       }
     }

    ggsave(paste0("final_pictures/RealData/ReproductionWindow", window_size, "Filter", filter_size, "Area", Area, "Compare.svg"),plot = figure2_compare, width = 89,height = 60,units="mm",dpi=300,create.dir = TRUE)
    phi_plot <- mcmc_areas(daily_fit,"phi",transformations = function(x) 1/x) + scale_x_continuous("Overdispersion")
    ggsave(paste0("final_pictures/RealData/DailyPhiWindow", window_size, "Filter", filter_size,"Area", Area, ".svg"),plot = phi_plot, width = 89,height = 60,units="mm",dpi=300,create.dir = TRUE)
  }
}
}
