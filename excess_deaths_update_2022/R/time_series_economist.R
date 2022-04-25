# This file produces a time series of orphans.
library(tidyverse)
library(countrycode)
source("excess_deaths_update_2022/R/shiny_table.R")

#d <- read.csv("https://raw.githubusercontent.com/TheEconomist/covid-19-the-economist-global-excess-deaths-model/main/output-data/export_country_cumulative.csv")
d = readRDS("excess_deaths_update_2022/data/economist_excess_deaths.RDS")
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

deaths_country = select(d, country, date, cumulative_estimated_daily_excess_deaths)

# Remove data which is negative
deaths_country =  deaths_country[which(deaths_country$cumulative_estimated_daily_excess_deaths > 0),]

# Temporarily just calculate last date
#deaths_country = deaths_country[deaths_country$date == max(deaths_country$date),]

# Join JHU with tfr data
data = readRDS("excess_deaths_update_2022/data/tfr_econ.RDS")
data$country[which(data$country == "USA")] <- "United States of America"
data$country[which(data$country == "I.R. Iran")] <- "Iran (Islamic Republic of)"
data$sd = (data$tfr_u-data$tfr_l)/(2*1.96)
data$europe = ifelse(data$who_region == "European", 1, 0) 
data = select(data, "country", "tfr", "tfr_l",  "tfr_u", "sd", "who_region", "europe")
combined_data <- left_join(deaths_country, data, by = c("country"))
tmp = unique(combined_data$country[is.na(combined_data$tfr)])
print(length(tmp))
tmp = unique(combined_data$country[is.na(combined_data$who_region)])
print(length(tmp))

# Calculate orphans
parents = NULL
primary = NULL
primary_secondary = NULL

dates = unique(combined_data$date)

# Could be re-written so just add new column on end of spreadsheet each day
for (i in 1:length(dates)){
  print(i)
  c_data <- combined_data[which(combined_data$date == dates[i]), c("country", "tfr", "tfr_l",  "tfr_u", "sd", "who_region", "europe", "cumulative_estimated_daily_excess_deaths")]
  names(c_data) <- c("country", "tfr", "tfr_l", "tfr_u", "sd",  "who_region", "europe", "total_deaths")
  orphans <- calculate_all_orphans_time_series(c_data, dates[i], uncertainty = FALSE, death_uncertainty = FALSE)
  
  primary_secondary <- rbind(primary_secondary, orphans[[1]])
  primary <- rbind(primary, orphans[[2]])
  parents <- rbind(parents, orphans[[3]])
}

g <- primary_secondary %>% group_by(date) %>% summarise(total = sum(central))

primary_secondary$country[primary_secondary$country == "England & Wales"] <- "United Kingdom"
primary$country[primary$country == "England & Wales"] <- "United Kingdom"
parents$country[parents$country == "England & Wales"] <- "United Kingdom"


# Join
ps <- select(primary_secondary, country, region, date,  central)
names(ps) <- c("country", "region", "date", "primary_secondary")
p <- select(primary, country, date,  central)
names(p) <- c("country", "date", "primary")
pa <- select(parents, country, date,  central)
names(pa) <- c("country", "date", "orphanhood")

dat <- left_join(ps, p, by = c("country", "date"))
dat <- left_join(dat, pa, by = c("country", "date"))

write.csv(dat, "excess_deaths_update_2022/output/orphanhood_timeseries_no_omit_economist.csv", row.names=FALSE)

omit = dat$country[dat$date == max(dat$date) & dat$orphanhood < 25]
dat_omit = dat[!dat$country %in% omit,]

dat_all = dat
dat_all <- dat_all[order(dat_all$date),]
print(dat_all[dat_all$country == "Global" & dat_all$date == "2021-12-27",])
print(dat_all[dat_all$country == "Global" & dat_all$date == "2022-03-28",])

write.csv(dat_all, "excess_deaths_update_2022/output/orphanhood_timeseries_economist.csv", row.names=FALSE)
