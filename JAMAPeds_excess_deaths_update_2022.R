print("Runs WHO analysis")
source("JAMAPeds_excess_deaths_update_2022/R/time_series_who_adjusted.R")
source("JAMAPeds_excess_deaths_update_2022/R/who_analysis.R")

print("Runs Economist analysis")
source("JAMAPeds_excess_deaths_update_2022/R/economist_analysis.R")

print("Runs IHME analysis")
source("JAMAPeds_excess_deaths_update_2022/R/ihme_analysis.R")

print("Makes tables and figures")
source("JAMAPeds_excess_deaths_update_2022/R/excess_deaths_table.R")
source("JAMAPeds_excess_deaths_update_2022/R/map_who.R")
source("JAMAPeds_excess_deaths_update_2022/R/orphanhood_timeseries_plot_who.R")
source("JAMAPeds_excess_deaths_update_2022/R/combine_plots.R")
source("JAMAPeds_excess_deaths_update_2022/R/numbers.R")

