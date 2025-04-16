# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)
# Set target options:
tar_option_set(
  packages = c(
    "tidyr",
    "tibble",
    "readr",
    "magrittr",
    "dplyr",
    "lubridate",
    "rstan",
    "bayesplot",
    "ggplot2",
    "cowplot",
    "StanHeaders"
  ) # Packages that your targets need for their tasks.
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()

# Parameters to run all simulations.
plot_quantiles = c(0.025, 0.25, 0.75, 0.975)
data_scenarios <- tibble::tibble(filename = c("data/victoria_data.csv"))
day_scenarios = tibble::tibble(days = c(7*15,7*30))
prior_scenarios = tibble::tibble(area = c(50),alpha = c(3,1e-3),beta = c(0.5,1e-3))
missing_scenarios = tibble::tibble(window = list(7,14,4,1,7,"Random",4), filter = c(1,1,1,2,2,1,2))
base_scenarios = tidyr::expand_grid(data_scenarios,day_scenarios,prior_scenarios)
missing_model_scenarios = tidyr::expand_grid(missing_scenarios)

save_real_data_figures <- list(
  tar_target(
    saved_reproduction_plot,
    ggsave(
      paste0(
        "final_pictures/RealData/ReproductionWindow",
        window,
        "Filter",
        filter,
        "Area",
        area,
        "Days",
        days,
        "Alpha",
        alpha,
        "Beta",
        beta,
        ".png"
      ),
      plot = reproduction_plot,
      width = 89,
      height = 60,
      units = "mm",
      dpi = 300,
      create.dir = TRUE
    ),
    format = "file"
  ),
  tar_target(
    saved_incidence_plot,
    ggsave(
      paste0(
        "final_pictures/RealData/IncidenceWindow",
        window,
        "Filter",
        filter,
        "Area",
        area,
        "Days",
        days,
        "Alpha",
        alpha,
        "Beta",
        beta,
        ".png"
      ),
      plot = incidence_plot,
      width = 89,
      height = 60,
      units = "mm",
      dpi = 300,
      create.dir = TRUE
    ),
    format = "file"
  )
)

set_generation_interval <- tar_target(
  covid_generation_interval,
  define_generation_interval(3.6, 3.1, 16)
)

load_data <- list(
  tar_target(data_file, filename, format = "file"),
  tar_target(data, load_and_filter_real_data(data_file,days))
)

base_scenario_pipeline <- list(
  tar_target(
    base_covid_fit,
    reduced_data_model(
      data,
      covid_generation_interval,
      area,
      1,
      1,
      alpha,
      beta
    )
  ),
  tar_target(base_crps_covid,
             calculate_crps(base_covid_fit,data)),
  tar_target(daily_covid_fit, daily_model(data, covid_generation_interval, alpha, beta, area)),
  tar_target(summ_daily_covid_fit, summarise_daily_model_fit(daily_covid_fit, plot_quantiles))
)

scenario_pipeline <- list(
  tar_target(
    fit,
    reduced_data_model(
      data,
      covid_generation_interval,
      area,
      window,
      filter,
      alpha,
      beta
    )),
  tar_target(summary_fit,summarise_model_fit(fit$model_fit, plot_quantiles)),
  tar_target(plotting_regions, calculate_plotting_regions(fit$model_data)),
  tar_target(incidence_plot, plot_incidence(summary_fit, data,plotting_regions)),
  tar_target(incidence_bayes_estimate_plot, plot_bayes_estimate(summary_fit, data)),
  tar_target(fit_skill, tibble::tibble(area = area, window = list(window), filter = filter, alpha = alpha, beta= beta, skill = calculate_mean_skill(base_crps_covid,calculate_crps(fit,data)))),
  tar_target(reproduction_plot, plot_effective_reproduction_number(summary_fit, summ_daily_covid_fit, data,plotting_regions)),
  save_real_data_figures
)

all_missing_scenarios <- tar_map(values = missing_model_scenarios,
                                 scenario_pipeline,
                                 unlist = FALSE)

# We want to demonstrate stability over time points - lets look at weekly.
incremental_data_scenarios <- tibble::tibble(filename = c("data/victoria_data.csv"), increment = "increment", area = 50, alpha = 3,beta = 0.5, days = seq(4,12,by = 1)*7,window = 7,filter = 1)
incremental_data_scenarios <- tibble::tibble(filename = c("data/victoria_data.csv"), increment = "increment", area = 50, alpha = 1e-3,beta = 1e-3, days = seq(4,12,by = 1)*7,window = 7,filter = 1)
incremental_scenario_pipeline <- list(
  tar_target(
    fit,
    reduced_data_model(
      data,
      covid_generation_interval,
      area,
      window,
      filter,
      alpha,
      beta
    )),
  tar_target(summary_fit,summarise_model_fit(fit$model_fit, plot_quantiles)),
  tar_target(tibble_summary_incidence, tibble::as_tibble(t(summary_fit$I_T)) |> dplyr::mutate(day = dplyr::row_number() + min(data$diagnosis_date), Day = as.factor(days))),
  tar_target(tibble_summary_R_T, tibble::as_tibble(t(summary_fit$R_T)) |> dplyr::mutate(day = dplyr::row_number()+ min(data$diagnosis_date), Day = as.factor(days)))
)

incremental_data_scenarios <- tar_map(values = incremental_data_scenarios,
                                      load_data,
                                      incremental_scenario_pipeline,
                                      unlist = FALSE)

epi_estim_comparison <- list(
  tar_target(epi_estim_solve, epiestim_estimate(fit_7_1_data.victoria_data.csv_210_50_0.001_0.001$model_data)),
  tar_target(epi_estim_incidence,get_epiestim_incidence(epi_estim_solve)),
  tar_target(epi_estim_rt,get_epiestim_rt(epi_estim_solve)),
  tar_target(epi_estim_incidence_plot, epiestim_plot_incidence(epi_estim_incidence,summary_fit_7_1_data.victoria_data.csv_210_50_0.001_0.001,data_data.victoria_data.csv_210_50_0.001_0.001)),
  tar_target(epi_estim_solve_short, epiestim_estimate(fit_7_1_data.victoria_data.csv_105_50_0.001_0.001$model_data)),
  tar_target(epi_estim_incidence_short,get_epiestim_incidence(epi_estim_solve_short)),
  tar_target(epi_estim_rt_short,get_epiestim_rt(epi_estim_solve_short)),
  tar_target(epi_estim_incidence_plot_short, epiestim_plot_incidence(epi_estim_incidence_short,summary_fit_7_1_data.victoria_data.csv_105_50_0.001_0.001,data_data.victoria_data.csv_105_50_0.001_0.001)),
  tar_target(epi_estim_rt_plot_short, epiestim_plot_rt(epi_estim_rt_short,summary_fit_7_1_data.victoria_data.csv_105_50_3_0.5,data_data.victoria_data.csv_105_50_0.001_0.001)),
  tar_target(
    epi_estim_saved_incidence_plot,
    ggsave(
      "final_pictures/RealData/EpiEstimIncidenceWindow7Filter1Days105.png",
      plot = epi_estim_incidence_plot_short,
      width = 89,
      height = 60,
      units = "mm",
      dpi = 300,
      create.dir = TRUE
    ),
    format = "file"
  ),
  tar_target(
    epi_estim_saved_rt_plot,
    ggsave(
      "final_pictures/RealData/EpiEstimReproductionWindow7Filter1Days105.png",
      plot = epi_estim_rt_plot_short,
      width = 89,
      height = 60,
      units = "mm",
      dpi = 300,
      create.dir = TRUE
    ),
    format = "file"
  )
)

r_t_shock <- function(){
  n_days <- 70
  R_t <- rep(1.5,n_days)
  R_t[40:n_days] <- 0.8
  return(R_t)
}

r_t_cos <- function(n){
  n_days <- 70
  0.8*cos((1:n_days)*n*pi/n_days) + 1.2
}

simulated_prior_scenarios = tibble::tibble(Area = c(50),alpha =1e-3,beta=1e-3)
simulated_reduced_scenarios = tidyr::expand_grid(tibble::tibble(window = list(1,2,4,7,"Random")),tibble::tibble(filter = c(1,2)))
all_simulated_scenarios = tidyr::expand_grid(simulated_prior_scenarios, simulated_reduced_scenarios)
all_simulated_cosine_scenarios = tidyr::expand_grid(all_simulated_scenarios, tibble::tibble(n = c(1,2,3)))



simulated_data_pipeline <- list(
  tar_target(simulated_generation_interval,define_simulated_generation_interval()),
  tar_target(R_t_shock, r_t_shock()),
  tar_map(values = all_simulated_scenarios,
          tar_target(simulated_data, generate_simulated_data(R_t_shock,simulated_generation_interval)),
          tar_target(simulated_fit, simulated_model_fit(simulated_data, simulated_generation_interval,Area, window,filter,alpha,beta)),
          tar_target(summ_simulated_fit, summarise_model_fit(simulated_fit$model_fit,plot_quantiles)),
          tar_target(simulated_plotting_regions, calculate_plotting_regions(simulated_fit$model_data)),
          tar_target(simulated_incidence_plot, plot_incidence(summ_simulated_fit, simulated_data |> dplyr::rename(cases = Total_case_count), simulated_plotting_regions)),
          tar_target(simulated_rt_plot, plot_simulated_effective_reproduction_number(summ_simulated_fit,simulated_data |> dplyr::rename(cases = Total_case_count),simulated_plotting_regions)),
          tar_target(
            saved_simulated_incidence_plot,
            ggsave(
              paste0(
                "final_pictures/SimulatedData/IncidenceWindow",
                window,
                "Filter",
                filter,
                "Area",
                Area,
                "Alpha",
                alpha,
                "Beta",
                beta,
                "Shock",
                ".png"
              ),
              plot = simulated_incidence_plot,
              width = 89,
              height = 60,
              units = "mm",
              dpi = 300,
              create.dir = TRUE
            ),
            format = "file"
          ),
          tar_target(
            saved_simulated_rt_plot,
            ggsave(
              paste0(
                "final_pictures/SimulatedData/ReproductionWindow",
                window,
                "Filter",
                filter,
                "Area",
                Area,
                "Alpha",
                alpha,
                "Beta",
                beta,
                "Shock",
                ".png"
              ),
              plot = simulated_rt_plot,
              width = 89,
              height = 60,
              units = "mm",
              dpi = 300,
              create.dir = TRUE
            ),
            format = "file"
          ),
          unlist = FALSE),
  tar_map(values = all_simulated_cosine_scenarios,
          tar_target(R_t_cos, r_t_cos(n)),
          tar_target(simulated_data, generate_simulated_data(R_t_cos,simulated_generation_interval)),
          tar_target(simulated_fit, simulated_model_fit(simulated_data, simulated_generation_interval,Area, window,filter,alpha,beta)),
          tar_target(summ_simulated_fit, summarise_model_fit(simulated_fit$model_fit,plot_quantiles)),
          tar_target(simulated_plotting_regions, calculate_plotting_regions(simulated_fit$model_data)),
          tar_target(simulated_incidence_plot, plot_incidence(summ_simulated_fit, simulated_data |> dplyr::rename(cases = Total_case_count), simulated_plotting_regions)),
          tar_target(zoomed_incidence_plot,plot_zoomed_incidence(simulated_incidence_plot)),
          tar_target(simulated_rt_plot, plot_simulated_effective_reproduction_number(summ_simulated_fit,simulated_data |> dplyr::rename(cases = Total_case_count),simulated_plotting_regions)),
          tar_target(
            saved_simulated_incidence_plot,
            ggsave(
              paste0(
                "final_pictures/SimulatedData/IncidenceWindow",
                window,
                "Filter",
                filter,
                "Area",
                Area,
                "Alpha",
                alpha,
                "Beta",
                beta,
                "Cos",
                n,
                ".png"
              ),
              plot = simulated_incidence_plot,
              width = 89,
              height = 60,
              units = "mm",
              dpi = 300,
              create.dir = TRUE
            ),
            format = "file"
          ),
          tar_target(
            saved_zoomed_simulated_incidence_plot,
            ggsave(
              paste0(
                "final_pictures/SimulatedData/ZoomIncidenceWindow",
                window,
                "Filter",
                filter,
                "Area",
                Area,
                "Alpha",
                alpha,
                "Beta",
                beta,
                "Cos",
                n,
                ".png"
              ),
              plot = zoomed_incidence_plot,
              width = 89,
              height = 60,
              units = "mm",
              dpi = 300,
              create.dir = TRUE
            ),
            format = "file"
          ),
          tar_target(
            saved_simulated_rt_plot,
            ggsave(
              paste0(
                "final_pictures/SimulatedData/ReproductionWindow",
                window,
                "Filter",
                filter,
                "Area",
                Area,
                "Alpha",
                alpha,
                "Beta",
                beta,
                "Cos",
                n,
                ".png"
              ),
              plot = simulated_rt_plot,
              width = 89,
              height = 60,
              units = "mm",
              dpi = 300,
              create.dir = TRUE
            ),
            format = "file"
          ),
          unlist = FALSE)
)

list(
  simulated_data_pipeline,
  set_generation_interval,
  tar_map(
    values = base_scenarios,
    list(load_data,
         base_scenario_pipeline,
         all_missing_scenarios,
         tar_combine(crps_table, all_missing_scenarios[["fit_skill"]],command  = dplyr::bind_rows(!!!.x))),
    unlist = FALSE),
  incremental_data_scenarios,
  tar_combine(incremental_plot_summ_incidence, incremental_data_scenarios[["tibble_summary_incidence"]],command = dplyr::bind_rows(!!!.x)),
  tar_combine(incremental_plot_summ_rt, incremental_data_scenarios[["tibble_summary_R_T"]],command = dplyr::bind_rows(!!!.x)),
  tar_target(incremental_plot_incidence, plot_consistency_incidence(incremental_plot_summ_incidence)),
  tar_target(incremental_plot_rt, plot_consistency_rt(incremental_plot_summ_rt)),
  tar_target(
    saved_incremental_plot_rt,
    ggsave(
      "final_pictures/RealData/IncrementalRt.png",
      plot = incremental_plot_rt,
      width = 89,
      height = 60,
      units = "mm",
      dpi = 300,
      create.dir = TRUE
    ),
    format = "file"
  ),
  tar_target(
    saved_incremental_plot_incidence,
    ggsave(
      "final_pictures/RealData/IncrementalIncidence.png",
      plot = incremental_plot_incidence,
      width = 89,
      height = 60,
      units = "mm",
      dpi = 300,
      create.dir = TRUE
    ),
    format = "file"
  ),
  epi_estim_comparison
)
