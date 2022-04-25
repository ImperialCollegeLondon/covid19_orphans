library(ggplot2)
library(cowplot)

timeseries = readRDS("excess_deaths_update_2022/figures/timeseries.RDS")
map = readRDS("excess_deaths_update_2022/figures/map_apr_2022.RDS")

p = plot_grid(timeseries, map, labels = "AUTO",  rel_widths = c(1, 1.5))
print(p)
ggsave(p,  file = "excess_deaths_update_2022/figures/fig1.pdf", width = 15, height = 5)
