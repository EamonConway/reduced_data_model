# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)
# Set target options:
tar_option_set(
  packages = c("tidyr","tibble","readr","magrittr","dplyr","lubridate","rstan","bayesplot","ggplot2","StanHeaders") # Packages that your targets need for their tasks.
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()
scenarios  = tibble(area = 30,window = 7,filter = 1)

# List of targets for paper.
list(
  tar_target(data_file, "data/victoria_data.csv", format = "file"),
  tar_target(real_data, load_and_filter_real_data(data_file)),
  tar_target(covid_generation_interval,define_generation_interval(3.6,3.1,16)),
  # tar_target(daily_fit,daily_model(real_data,covid_generation_interval)),
  tar_map(values = scenarios,
  tar_target(covid_fit, reduced_data_model(real_data, covid_generation_interval, area,window,filter))
  )
)
