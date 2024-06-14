set.seed(0x1234)
source("functions.R")
s <- 0:14
g <- plnorm(s[-1],log(2.0),1.0) - plnorm(s[-length(s)],log(2.0),1.0)
g <- g/sum(g)

n_days <- 70
R_t <- rep(1.5,n_days)
R_t[40:n_days] <- 0.8
R_t <- 0.8*cos((1:n_days)*2*pi/n_days) + 1.2
simulated_data <- tibble(diagnosis_date = 1:n_days,
                         R_t = R_t, 
                         Total_case_count = generate_simulated_incidence(R_T,g,100)) 
Area = 50
alpha = 1e-3
beta = 1e-3
lambda = -log(Area/100)

for(window_size in list(1,2,4,7,"Random")){
  
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
  
  for(filter_size in c(1,2)){
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
    
    missing_data <- aggregated_data_full %>% 
      filter(!(row_number() %in% rows_to_keep))
    
    xmax <- missing_data$day
    xmin <- xmax - missing_data$window
    
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
    
    fit <- rstan::stan(file = "binomial_aggregate_known.stan",
                       data = stan_data, 
                       iter = 10000, 
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
      format_plot_data(.,min(data$diagnosis_date))
    
    I_plot_data <- summary_fit$I_T %>%
      format_plot_data(.,min(data$diagnosis_date))
    
    
    figure1 <- ggplot(daily_data, aes(x = diagnosis_date)) + 
      geom_ribbon(data = I_plot_data, aes(ymin = mid_lower,ymax = mid_upper),alpha = 0.8,fill = "#7570b3") +
      geom_ribbon(data = I_plot_data, aes(ymin = lower,ymax = upper),alpha = 0.5,fill = "#7570b3") + 
      geom_vline(xintercept = aggregated_data$week,linetype="dotted") + 
      geom_point(aes(y = Total_case_count),size = 0.5)  +
      scale_y_continuous("Daily Incidence")+
      scale_x_continuous("Day") +
      theme_classic() +
      theme(text = element_text(size=16)) 
    
    if(length(xmax)!=0){
    for(i in 1:length(xmax)){
      figure1 <- figure1 + annotate("rect",xmin = xmin[i],xmax=xmax[i],ymin = -Inf,ymax = Inf,alpha = 0.2)
    }
    }
    ggsave(paste0("final_pictures/SimulatedData/Cos/IncidenceWindow", window_size, "Filter",filter_size,"Area", Area, ".svg"),plot = figure1, width = 89,height = 60,units="mm",dpi=300,create.dir = TRUE)
    
    figure2 <- ggplot(R_plot_data,aes(x = diagnosis_date)) + 
      geom_ribbon( aes(ymin = mid_lower,ymax = mid_upper),alpha = 0.8,fill = "#7570b3") +
      geom_ribbon(data = R_plot_data, aes(ymin = lower,ymax = upper),alpha = 0.5,fill = "#7570b3") + 
      geom_vline(xintercept = aggregated_data$week,linetype="dotted") + 
      geom_point(data = daily_data,aes(y = R_t),size = 0.5)  +
      scale_y_continuous("R Effective",limits = c(0,NULL))+ 
      coord_cartesian(ylim = c(0,3))+
      scale_x_continuous("Day") +
      theme_classic() +
      theme(text = element_text(size=16)) 
    
    if(length(xmax)!=0){
      for(i in 1:length(xmax)){
        figure2 <- figure2 + annotate("rect",xmin = xmin[i],xmax=xmax[i],ymin = -Inf,ymax = Inf,alpha = 0.2)
      }
    }
    
    phi_plot <- mcmc_areas(fit,"phi",transformations = function(x) 1/x) + scale_x_continuous("Overdispersion")
    ggsave(paste0("final_pictures/SimulatedData/Cos/PhiWindow", window_size, "Filter", filter_size,"Area", Area, ".svg"),plot = phi_plot, width = 89,height = 60,units="mm",dpi=300,create.dir = TRUE)
    ggsave(paste0("final_pictures/SimulatedData/Cos/ReproductionWindow", window_size, "Filter", filter_size,"Area", Area, ".svg"),plot = figure2, width = 89,height = 60,units="mm",dpi=300,create.dir = TRUE)
  }
}
