library(tidyverse)
source("excess_deaths_update_2022/R/utils.R")

ihme = read.csv("excess_deaths_update_2022/output/ihme_uncertainty_global.csv")
ihme$source = "IHME"
who = read.csv("excess_deaths_update_2022/output/who_uncertainty_global.csv")
who$source = "WHO"
economist = read.csv("excess_deaths_update_2022/output/economist_uncertainty_global.csv")
economist$source = "Economist"
economist$date = who$date

comb = rbind(economist, ihme, who)
comb$primary_secondary_format = sprintf("%s [%s - %s]", 
                                        format(round(comb$primary_secondary, -2), big.mark = ",", trim = TRUE), 
                                        format(round.choose(comb$ps_lower, 100, 0), big.mark = ",", trim = TRUE), 
                                        format(round.choose(comb$ps_upper, 100, 1), big.mark = ",", trim = TRUE))
comb$primary_format = sprintf("%s [%s - %s]", 
                                        format(round(comb$primary, -2), big.mark = ",", trim = TRUE), 
                                        format(round.choose(comb$p_lower, 100, 0), big.mark = ",", trim = TRUE), 
                                        format(round.choose(comb$p_upper, 100, 1), big.mark = ",", trim = TRUE))
comb$orphanhood_format = sprintf("%s [%s - %s]", 
                                        format(round(comb$orphanhood, -2), big.mark = ",", trim = TRUE), 
                                        format(round.choose(comb$or_lower, 100, 0), big.mark = ",", trim = TRUE), 
                                        format(round.choose(comb$or_upper, 100, 1), big.mark = ",", trim = TRUE))

comb_sub = select(comb, source, date, orphanhood_format, primary_format, primary_secondary_format)
comb_sub_wide = pivot_wider(comb_sub, names_from = date, 
                            values_from = c(orphanhood_format, primary_format, primary_secondary_format))
comb_sub_wide = comb_sub_wide[,c(1, 2, 4, 6, 3, 5, 7)]
write.csv(comb_sub_wide, "excess_deaths_update_2022/output/excess_deaths_table.csv", row.names = FALSE)
