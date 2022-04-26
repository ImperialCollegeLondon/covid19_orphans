library(tidyverse)
source("excess_deaths_update_2022/R/utils.R")

d <- read.csv("excess_deaths_update_2022/output/who_uncertainty_all.csv")
d <- d[d$date == as.Date("2022-04-01"),]

d_countries = d[8:length(d$country),]
d_countries = d_countries[order(d_countries$primary_secondary, decreasing = TRUE),]
top5 = d_countries[1:10, c(1,4:6)]
top5$format = sprintf("%s [%s - %s]", 
                      format(round(top5$primary_secondary, -2), big.mark = ",", trim = TRUE), 
                      format(round.choose(top5$ps_lower, 100, 0), big.mark = ",", trim = TRUE), 
                      format(round.choose(top5$ps_upper, 100, 1), big.mark = ",", trim = TRUE))
top5 = select(top5, country, format)
print("Top 10")
print(top5)

d_africa =d_countries[d_countries$region=="African ",]
top5_africa = d_africa[1:5, c(1,4:6)]
top5_africa$format = sprintf("%s [%s - %s]", 
                      format(round(top5_africa$primary_secondary, -2), big.mark = ",", trim = TRUE), 
                      format(round.choose(top5_africa$ps_lower, 100, 0), big.mark = ",", trim = TRUE), 
                      format(round.choose(top5_africa$ps_upper, 100, 1), big.mark = ",", trim = TRUE))
top5_africa = select(top5_africa, country, format)
print("Top 5 AFRO")
print(top5_africa)


d_sea = d_countries[d_countries$region=="South-East Asia",]
top5_sea = d_sea[1:5, c(1,4:6)]
top5_sea$format = sprintf("%s [%s - %s]", 
                             format(round(top5_sea$primary_secondary, -2), big.mark = ",", trim = TRUE), 
                             format(round.choose(top5_sea$ps_lower, 100, 0), big.mark = ",", trim = TRUE), 
                             format(round.choose(top5_sea$ps_upper, 100, 1), big.mark = ",", trim = TRUE))
top5_sea = select(top5_sea, country, format)
print("Top 5 SEARO")
print(top5_sea)
                                                      