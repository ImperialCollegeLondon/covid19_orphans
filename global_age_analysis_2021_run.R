# Complete age analysis

# Updates global numbers through 30th April with new india coefficient
source("TheLancet_global_minimum_estimates_2021/extract_study_data.R")
extract_study_data(updated_coef = TRUE)
source("global_age_analysis_2021/R/run_global_extrapolation_europe_april.R")
source("global_age_analysis_2021/R/format_update_results.R")
format_table("apr")

# Updates global numbers though 31st October
source("global_age_analysis_2021/R/extract_study_data_oct.R")
source("global_age_analysis_2021/R/run_global_extrapolation_europe.R")
source("global_age_analysis_2021/R/format_update_results.R")
format_table("oct")
combine_table()

# Runs age analysis
source("global_age_analysis_2021/R/run_age_analysis_uncertainty.R")

# Run global age model
source("global_age_analysis_2021/R/global_age_model.R")

# Makes time series plot
source("global_age_analysis_2021/R/orphanhood_time_series.R")
 
# Makes figures and tables
source("global_age_analysis_2021/R/age_plots.R")