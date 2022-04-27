print("Runs WHO analysis")
source("excess_deaths_update_2022/R/time_series_who_adjusted.R")
source("excess_deaths_update_2022/R/who_analysis_new_uq_2.R")

print("Runs Economist analysis")
source("excess_deaths_update_2022/R/time_series_economist.R")
source("excess_deaths_update_2022/R/economist_analysis.R")

print("Runs IHME analysis")
source("excess_deaths_update_2022/R/time_series_ihme_adjusted.R")
source("excess_deaths_update_2022/R/ihme_analysis.R")

print("Makes tables and figures")
source("excess_deaths_update_2022/R/excess_deaths_table.R")
source("excess_deaths_update_2022/R/map_who.R")
source("excess_deaths_update_2022/R/orphanhood_timeseries_plot_who.R")
source("excess_deaths_update_2022/R/combine_plots.R")
source("excess_deaths_update_2022/R/numbers.R")

