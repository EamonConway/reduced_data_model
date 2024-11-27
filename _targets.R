# Load packages required to define the pipeline:
library(targets)
library(readr)
library(magrittr)
library(dplyr)
library(lubridate)
# Set target options:
tar_option_set(
  packages = c("tidyr","tibble","readr","magrittr","dplyr","lubridate","rstan","bayesplot","ggplot2","StanHeaders") # Packages that your targets need for their tasks.
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()

# List of targets for paper.
list(
  tar_target(data_file, "data/victoria_data.csv", format = "file"),
  tar_target(covid_generation_interval,define_generation_interval(3.6,3.1,16)),
  tar_target(plot, reduced_data_model(covid_generation_interval, 30,7,1))
)
