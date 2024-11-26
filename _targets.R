# Load packages required to define the pipeline:
library(targets)

# Set target options:
tar_option_set(
  packages = c("tibble") # Packages that your targets need for their tasks.
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()

# List of targets for paper.
list(
  tar_target(data_file, "data/victoria_data.csv", format = "file"),
)
