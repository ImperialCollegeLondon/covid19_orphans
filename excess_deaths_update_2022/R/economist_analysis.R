# This file produces a time series of orphans.
library(tidyverse)
library(countrycode)
source("excess_deaths_update_2022/R/orphanhood_functions.R")

#d <- read.csv("https://raw.githubusercontent.com/TheEconomist/covid-19-the-economist-global-excess-deaths-model/main/output-data/export_country_cumulative.csv")
d <- readRDS(file = "excess_deaths_update_2022/data/economist_excess_deaths.RDS")
d$country = countrycode(d$iso3c, "iso3c", "country.name")

# Remove countries removed in global analysis
d <- d[which(d$country != "Taiwan"),]
d <- d[which(d$country != "Vatican City"),]

d$country[which(d$country == "Iran")] <- "Iran (Islamic Republic of)"
d$country[which(d$country == "United States")] <- "United States of America"
d$country[which(d$country == "United Kingdom")] <- "England & Wales"
d$country[which(d$country == "Russia")] <- "Russian Federation"
d$country[which(d$country == "Antigua & Barbuda")] <- "Antigua and Barbuda"
d$country[which(d$country == "Bosnia & Herzegovina")] <- "Bosnia and Herzegovina"
d$country[which(d$country == "Bolivia")] <- "Bolivia (Plurinational State of)"
d$country[which(d$country == "Brunei")] <- "Brunei Darussalam"
d$country[which(d$country == "Côte d’Ivoire")] <- "Cote d'Ivoire"
d$country[which(d$country == "Congo - Kinshasa")] <- "Democratic Republic of the Congo"
d$country[which(d$country == "Congo - Brazzaville")] <- "Congo"
d$country[which(d$country == "Cape Verde")] <- "Cabo Verde"
d$country[which(d$country == "Czechia")] <- "Czech Republic"
d$country[which(d$country == "Gambia")] <- "Gambia (Republic of The)"
d$country[which(d$country == "Guinea-Bissau")] <- "Guinea Bissau"
d$country[which(d$country == "St. Kitts & Nevis")] <- "Saint Kitts and Nevis"
d$country[which(d$country == "Laos")] <- "Lao People's Democratic Republic"
d$country[which(d$country == "St. Lucia")] <- "Saint Lucia"
d$country[which(d$country == "Myanmar (Burma)")] <- "Myanmar"
d$country[which(d$country == "Palestinian Territories")] <- "Occupied Palestinian Territory"
d$country[which(d$country == "São Tomé & Príncipe")] <- "Sao Tome and Principe"
d$country[which(d$country == "Syria")] <- "Syrian Arab Republic"
d$country[which(d$country == "Trinidad & Tobago")] <- "Trinidad and Tobago"
d$country[which(d$country == "Tanzania")] <- "United Republic of Tanzania"
d$country[which(d$country == "Saint Vincent and the Grenadines")] <- "United Republic of Tanzania"
d$country[which(d$country == "Venezuela")] <- "Venezuela (Bolivarian Republic of)"
d$country[which(d$country == "Vietnam")] <- "Viet Nam"
d$country[which(d$country == "Hong Kong SAR China")] <- "China, Hong Kong SAR"
d$country[which(d$country == "Macao SAR China")] <- "China, Macao SAR"
d$country[which(d$country == "Moldova")] <- "Republic of Moldova"
d$country[which(d$country == "North Korea")] <- "Dem. People's Republic of Korea"
d$country[which(d$country == "South Korea")] <- "Republic of Korea"
d$country[which(d$country == "St. Vincent & Grenadines")] <- "Saint Vincent and the Grenadines"
d$country[which(d$country == "Wallis & Futuna")] <- "Wallis and Futuna"
d$country[which(d$country == "Turks & Caicos Islands")] <- "Turks and Caicos Islands"

deaths_country = select(d, country, date, cumulative_estimated_daily_excess_deaths,
                        cumulative_estimated_daily_excess_deaths_ci_95_bot, cumulative_estimated_daily_excess_deaths_ci_95_top)
names(deaths_country) = c( "country", "date", "total_deaths", "lower", "upper")   

# Remove data which is negative
deaths_country =  deaths_country[which(deaths_country$total_deaths > 0),]

# Select dates of interest
deaths_country = deaths_country[deaths_country$date %in% c("2021-12-27", "2022-03-28"),]

# Join JHU with tfr data
data = readRDS("excess_deaths_update_2022/data/tfr_econ.RDS")
data$country[which(data$country == "USA")] <- "United States of America"
data$country[which(data$country == "I.R. Iran")] <- "Iran (Islamic Republic of)"
data$sd = (data$tfr_u-data$tfr_l)/(2*1.96)
data$europe = ifelse(data$who_region == "European", 1, 0) 
data = select(data, "country", "tfr", "tfr_l",  "tfr_u", "sd", "who_region", "europe")
combined_data <- left_join(deaths_country, data, by = c("country"))
#tmp = unique(combined_data$country[is.na(combined_data$tfr)])
#print(length(tmp))
#tmp = unique(combined_data$country[is.na(combined_data$who_region)])
#print(length(tmp))

num_countries = print(sprintf("Number countries Economist: %s", length(unique(combined_data$country))))

# Calculate orphans
parents = NULL
primary = NULL
primary_secondary = NULL

dates = unique(combined_data$date)

for (i in 1:length(dates)){
  c_data <- combined_data[which(combined_data$date == dates[i]), c("country", "tfr", "tfr_l",  "tfr_u", "sd", "who_region", "europe", "total_deaths", "lower", "upper")]
  names(c_data) <- c("country", "tfr", "tfr_l", "tfr_u", "sd",  "who_region", "europe", "total_deaths", "lower", "upper")
  
  orphans <- calculate_all_orphans_time_series(c_data = c_data, date = dates[i], 
                                               uncertainty = TRUE, death_uncertainty = TRUE,
                                               num_samples = 5000, source = "economist")
  
  primary_secondary <- rbind(primary_secondary, orphans[[1]])
  primary <- rbind(primary, orphans[[2]])
  parents <- rbind(parents, orphans[[3]])
}

primary_secondary$country[primary_secondary$country == "England & Wales"] <- "United Kingdom"
names(primary_secondary) <- c("country", "region", "date", "primary_secondary", "ps_lower", "ps_upper")

primary$country[primary$country == "England & Wales"] <- "United Kingdom"
names(primary) <- c("country", "region", "date", "primary", "p_lower", "p_upper")

parents$country[parents$country == "England & Wales"] <- "United Kingdom"
names(parents) <- c("country", "region", "date", "orphanhood", "or_lower", "or_upper")

# Join (including uncertainty bounds) 
dat_uncertainty <- left_join(primary_secondary, select(primary, country, date, primary, 
                                                       p_lower, p_upper), by = c("country", "date"))

dat_uncertainty <- left_join(dat_uncertainty,  select(parents, country, date, orphanhood, 
                                                      or_lower, or_upper), by = c("country", "date"))

write.csv(dat_uncertainty, "excess_deaths_update_2022/output/economist_uncertainty_all.csv", row.names = FALSE)

print(dat_uncertainty[dat_uncertainty$country == "Global" & dat_uncertainty$date == "2021-12-27",])
print(dat_uncertainty[dat_uncertainty$country == "Global" & dat_uncertainty$date == "2022-03-28",])

write.csv(dat_uncertainty[dat_uncertainty$country == "Global",], "excess_deaths_update_2022/output/economist_uncertainty_global.csv", row.names = FALSE)

# Checking uncertainty intervals
#reg = dat_uncertainty[dat_uncertainty$country == dat_uncertainty$region,]
#print(sum(reg$primary_secondary < reg$ps_lower & reg$primary_secondary > reg$ps_upper))
#print(sum(reg$primary < reg$p_lower & reg$primary_secondary > reg$p_upper))
#print(sum(reg$orphanhood < reg$or_lower & reg$primary_secondary > reg$or_upper))
