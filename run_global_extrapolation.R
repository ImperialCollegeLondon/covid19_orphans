# This script runs the global extrapolation based on the data collected through the
# extract_study_data.R
library(readxl)

source("R/format_tfr.R")
source("R/project_world_primary_secondary.R")
source("R/project_world_primary.R")
source("R/project_world_parents.R")


### Plots ###################################################################################
source("R/plots_ratios.R")
source("R/plots_num_children.R")
source("R/plots_project_world.R")
source("R/time_series.R")