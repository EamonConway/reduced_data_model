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
    "StanHeaders"
  ) # Packages that your targets need for their tasks.
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()
alpha <- 3
beta <- 0.5
# area_scenarios = tibble(area = c(10,30,40,50,60,70,90))
area_scenarios = tibble::tibble(area = c(50))
# missing_scenarios = tibble::tibble
missing_scenarios = tibble::tibble(window = c(7,14,4,1,1), filter = c(1,1,1,1,2))
plot_quantiles = c(0.025, 0.25, 0.75, 0.975)


load_real_data <-   list(
  tar_target(data_file, "data/victoria_data.csv", format = "file"),
  tar_target(real_data, load_and_filter_real_data(data_file)),
  tar_target(
    covid_generation_interval,
    define_generation_interval(3.6, 3.1, 16)
  )
)

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

real_data_pipeline <- list(load_real_data,
                           tar_map(
                             values = area_scenarios,
                             tar_target(
                               daily_fit,
                               daily_model(real_data, covid_generation_interval, alpha, beta, area)
                             ),
                             tar_target(
                               summ_daily_fit,
                               summarise_daily_model_fit(daily_fit, plot_quantiles)
                             ),
                             tar_map(
                               values = missing_scenarios,
                               tar_target(
                                 covid_fit,
                                 reduced_data_model(
                                   real_data,
                                   covid_generation_interval,
                                   area,
                                   window,
                                   filter,
                                   alpha,
                                   beta
                                 )
                               ),
                               tar_target(crps_covid_fit,
                                          calculate_crps(covid_fit,real_data)),

                               tar_target(
                                 summ_covid_fit,
                                 summarise_model_fit(covid_fit$model_fit, plot_quantiles)
                               ),
                               tar_target(incidence_plot, plot_incidence(summ_covid_fit, real_data)),
                               tar_target(
                                 reproduction_plot,
                                 plot_effective_reproduction_number(summ_covid_fit, summ_daily_fit, real_data)
                               ),
                               save_real_data_figures
                             )
                           ))

list(real_data_pipeline)
