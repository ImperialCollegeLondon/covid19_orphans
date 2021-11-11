# This script runs the global extrapolation based on the data collected through the
# extract_study_data.R
library(readxl)

source("global_age_analysis_2021/R/format_tfr_europe.R")
format_tfr_europe(orphans_path = "global_age_analysis_2021/orphans.RDS", 
                  deaths_path = "global_age_analysis_2021/data/10-31-2021.csv")

source("global_age_analysis_2021/R/project_world_primary_secondary_europe.R")
source("global_age_analysis_2021/R/project_world_primary_europe.R")
source("global_age_analysis_2021/R/project_world_parents_europe.R")

source("global_age_analysis_2021/R/plots_project_world.R")
source("global_age_analysis_2021/R/combine_global_totals.R")
