# Complete age analysis
data.table::setDTthreads(1)

# # Updates global numbers through 30th April with new india coefficient
source("TheLancet_global_minimum_estimates_2021/extract_study_data.R")
extract_study_data(updated_coef = TRUE)
source("TheLancetCAH_global_age_analysis_2022/R/run_global_extrapolation_europe_april.R")
source("TheLancetCAH_global_age_analysis_2022/R/format_update_results.R")
format_table("apr")

# Updates global numbers though 31st October
source("TheLancetCAH_global_age_analysis_2022/R/extract_study_data_oct.R")
source("TheLancetCAH_global_age_analysis_2022/R/run_global_extrapolation_europe.R")
source("TheLancetCAH_global_age_analysis_2022/R/format_update_results.R")
format_table("oct")
combine_table()

# # Runs age analysis
source("TheLancetCAH_global_age_analysis_2022/R/run_age_analysis_uncertainty.R")

# Run global age model
source("TheLancetCAH_global_age_analysis_2022/R/global_age_model.R")

# Makes time series plot
source("TheLancetCAH_global_age_analysis_2022/R/orphanhood_time_series.R")

# Makes figures and tables
source("TheLancetCAH_global_age_analysis_2022/R/age_plots.R")
