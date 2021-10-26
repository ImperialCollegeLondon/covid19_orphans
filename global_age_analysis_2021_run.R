# Complete age analysis

# Updates global numbers
source("global_age_analysis_2021/R/extract_study_data_oct.R")
source("global_age_analysis_2021/R/run_global_extrapolation_europe.R")

# Runs age analysis
# source("global_age_analysis_2021/R/run_age_analysis_uncertainty.R")

# # Run global age model
source("global_age_analysis_2021/R/global_age_model.R")
# 
# # Makes time series plot
source("global_age_analysis_2021/R/orphanhood_time_series.R")
# 
# # Makes figures and tables
source("global_age_analysis_2021/R/age_plots.R")