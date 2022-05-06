# This file produces a time series of orphans.
library(tidyverse)
library(countrycode)
source("excess_deaths_update_2022/R/orphanhood_functions.R")

#---------------------------------------------------------------------------------------
# Read in economist data
d <- read.csv("https://raw.githubusercontent.com/TheEconomist/covid-19-the-economist-global-excess-deaths-model/main/output-data/export_country_cumulative.csv")
#saveRDS(d, file = "excess_deaths_update_2022/data/economist_excess_deaths.RDS")
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
d$country[which(d$country == "St. Pierre & Miquelon")] <- "Saint Pierre and Miquelon"
d$country[which(d$country =="St. Helena")] <- "Saint Helena"
d$country[which(d$country == "U.S. Virgin Islands")] <- "United States Virgin Islands"

deaths_country = select(d, country, date, cumulative_estimated_daily_excess_deaths,
                        cumulative_estimated_daily_excess_deaths_ci_95_bot, cumulative_estimated_daily_excess_deaths_ci_95_top)
names(deaths_country) = c( "country", "date", "total_deaths", "lower", "upper")   

# Select dates of interest
deaths_country_econ = deaths_country[deaths_country$date %in% c("2021-12-27", "2022-04-25"),]
deaths_country_econ$date = as.Date(deaths_country_econ$date)

#---------------------------------------------------------------------------------------
# Read in jhu data
d = readRDS("excess_deaths_update_2022/data/downloaded_timeseries_jhu.RDS")

d2 =  select(d, -Province.State, -Lat, -Long)

deaths_country = d2 %>% 
  group_by(Country.Region) %>%
  summarise_all(sum) 

deaths_country = deaths_country[which(!deaths_country$Country.Region %in% c("Diamond Princess", "Holy See", "MS Zaandam")),]
deaths_country$Country.Region[which(deaths_country$Country.Region == "Taiwan*")] = "Taiwan"
deaths_country$Country.Region[which(deaths_country$Country.Region == "Iran")] = "I.R. Iran"
deaths_country$Country.Region[which(deaths_country$Country.Region == "Russia")] = "Russian Federation"
deaths_country$Country.Region[which(deaths_country$Country.Region == "Bolivia")] = "Bolivia (Plurinational State of)"
deaths_country$Country.Region[which(deaths_country$Country.Region == "Brunei")] = "Brunei Darussalam"
deaths_country$Country.Region[which(deaths_country$Country.Region == "Burma")] = "Myanmar"
deaths_country$Country.Region[which(deaths_country$Country.Region == "Congo (Brazzaville)")] = "Congo"
deaths_country$Country.Region[which(deaths_country$Country.Region == "Congo (Kinshasa)")] = "Democratic Republic of the Congo"
deaths_country$Country.Region[which(deaths_country$Country.Region == "Venezuela")] = "Venezuela (Bolivarian Republic of)"
deaths_country$Country.Region[which(deaths_country$Country.Region == "Vietnam")] = "Viet Nam"
deaths_country$Country.Region[which(deaths_country$Country.Region == "Syria")] = "Syrian Arab Republic"
deaths_country$Country.Region[which(deaths_country$Country.Region == "Moldova")] = "Republic of Moldova"
deaths_country$Country.Region[which(deaths_country$Country.Region == "Tanzania")] = "United Republic of Tanzania"
deaths_country$Country.Region[which(deaths_country$Country.Region == "Korea, South")] = "Republic of Korea"
deaths_country$Country.Region[which(deaths_country$Country.Region == "Laos")] = "Lao People's Democratic Republic"
deaths_country$Country.Region[which(deaths_country$Country.Region == "US")] <- "United States of America"
deaths_country$Country.Region[which(deaths_country$Country.Region == "I.R. Iran")] <- "Iran (Islamic Republic of)"
deaths_country$Country.Region[which(deaths_country$Country.Region  == "Gambia")] <- "Gambia (Republic of The)"
deaths_country$Country.Region[which(deaths_country$Country.Region == "Guinea-Bissau")] <- "Guinea Bissau"
deaths_country$Country.Region[which(deaths_country$Country.Region  == "Czechia")] <- "Czech Republic"
deaths_country$Country.Region[which(deaths_country$Country.Region  == "United Kingdom")] <- "England & Wales"
deaths_country$Country.Region[which(deaths_country$Country.Region  == "West Bank and Gaza")] <- "Occupied Palestinian Territory"
deaths_country$Country.Region[which(deaths_country$Country.Region  == "Micronesia")] <- "Micronesia (Federated States of)"
deaths_country$Country.Region[which(deaths_country$Country.Region  == "St. Pierre & Miquelon")] <- "Saint Pierre and Miquelon"
deaths_country$Country.Region[which(deaths_country$Country.Region  == "St. Helena")] <- "Saint Helena"
deaths_country$Country.Region[which(deaths_country$Country.Region  == "U.S. Virgin Islands")] <- "United States Virgin Islands"
deaths_country = select(deaths_country, Country.Region, X12.27.21, X4.25.22)

#---------------------------------------------------------------------------------------
# Adjust study countries in JHU data by multipliers
multipliers_study <- read.csv('excess_deaths_update_2022/data/multipliers.csv', header = FALSE)
names(multipliers_study) <- c("Country.Region", "mult_study")
remove_study = c("Brazil", "India", "Mexico", "Peru", "South Africa", "Iran (Islamic Republic of)", "Colombia", "Russian Federation")
multipliers_study = multipliers_study[!multipliers_study$Country %in% remove_study,]

deaths_country = left_join(deaths_country, multipliers_study, by = "Country.Region")
deaths_country$mult_study[is.na(deaths_country$mult_study)] = 1
deaths_country$X12.27.21 = deaths_country$X12.27.21 * deaths_country$mult_study
deaths_country$X4.25.22 = deaths_country$X4.25.22 * deaths_country$mult_study

deaths_country = gather(deaths_country, key = date, value = jhu_deaths, -Country.Region)
deaths_country$date = sub('.', '', deaths_country$date)
deaths_country$date <- as.Date(deaths_country$date, format = "%m.%d.%y")

#---------------------------------------------------------------------------------------
# Combine data and choose max
deaths_country_econ = left_join(deaths_country_econ, deaths_country, by = c("country" = "Country.Region", "date"))
deaths_country_econ$jhu_deaths[is.na(deaths_country_econ$jhu_deaths)] = 0
deaths_country_econ$comb_death = ifelse(deaths_country_econ$jhu_deaths > deaths_country_econ$total_deaths, 
                                        deaths_country_econ$jhu_deaths, deaths_country_econ$total_deaths)
deaths_country_econ$lower = ifelse(deaths_country_econ$comb_death == deaths_country_econ$total_deaths, 
                                   deaths_country_econ$lower, deaths_country_econ$jhu_deaths)
deaths_country_econ$upper = ifelse(deaths_country_econ$comb_death == deaths_country_econ$total_deaths, 
                                   deaths_country_econ$upper, deaths_country_econ$jhu_deaths)

deaths_country = select(deaths_country_econ, country, date, comb_death, lower, upper)

# Remove countries with no TFR
deaths_country = deaths_country[!deaths_country$country %in% c("Falkland Islands" , "Pitcairn Islands", "Western Sahara"),]

#---------------------------------------------------------------------------------------
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
  c_data <- combined_data[which(combined_data$date == dates[i]), c("country", "tfr", "tfr_l",  "tfr_u", "sd", "who_region", "europe", "comb_death", "lower", "upper")]
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
print(dat_uncertainty[dat_uncertainty$country == "Global" & dat_uncertainty$date == "2022-04-25",])

write.csv(dat_uncertainty[dat_uncertainty$country == "Global",], "excess_deaths_update_2022/output/economist_uncertainty_global.csv", row.names = FALSE)

# Checking uncertainty intervals
#reg = dat_uncertainty[dat_uncertainty$country == dat_uncertainty$region,]
#print(sum(reg$primary_secondary < reg$ps_lower & reg$primary_secondary > reg$ps_upper))
#print(sum(reg$primary < reg$p_lower & reg$primary_secondary > reg$p_upper))
#print(sum(reg$orphanhood < reg$or_lower & reg$primary_secondary > reg$or_upper))
