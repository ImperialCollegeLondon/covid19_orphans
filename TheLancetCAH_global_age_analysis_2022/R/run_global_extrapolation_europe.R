# This script runs the global extrapolation based on the data collected through the
# extract_study_data.R
library(readxl)

source("TheLancetCAH_global_age_analysis_2022/R/format_tfr_europe.R")
format_tfr_europe(orphans_path = "TheLancetCAH_global_age_analysis_2022/orphans.RDS", 
                  deaths_path = "TheLancetCAH_global_age_analysis_2022/data/10-31-2021.csv")

source("TheLancetCAH_global_age_analysis_2022/R/project_world_primary_secondary_europe.R")
source("TheLancetCAH_global_age_analysis_2022/R/project_world_primary_europe.R")
source("TheLancetCAH_global_age_analysis_2022/R/project_world_parents_europe.R")

source("TheLancetCAH_global_age_analysis_2022/R/plots_project_world.R")
source("TheLancetCAH_global_age_analysis_2022/R/combine_global_totals.R")
