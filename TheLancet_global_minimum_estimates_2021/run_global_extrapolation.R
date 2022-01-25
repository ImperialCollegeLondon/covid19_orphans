# This script runs the global extrapolation based on the data collected through the
# extract_study_data.R
library(readxl)

source("TheLancet_global_minimum_estimates_2021/R/format_tfr.R")
source("TheLancet_global_minimum_estimates_2021/R/project_world_primary_secondary.R")
source("TheLancet_global_minimum_estimates_2021/R/project_world_primary.R")
source("TheLancet_global_minimum_estimates_2021/R/project_world_parents.R")


### Plots ###################################################################################
source("TheLancet_global_minimum_estimates_2021/R/plots_ratios.R")
source("TheLancet_global_minimum_estimates_2021/R/plots_num_children.R")
source("TheLancet_global_minimum_estimates_2021/R/plots_project_world.R")
source("TheLancet_global_minimum_estimates_2021/R/time_series.R")
