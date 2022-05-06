# This file produces a time series of orphans.
library(tidyverse)
source("excess_deaths_update_2022/R/orphanhood_functions.R")

#d <- read.csv("https://ghdx.healthdata.org/sites/default/files/record-attached-files/IHME_EM_COVID_19_2020_2021_DATA_Y2022M03D10.CSV")
d <- readRDS(file = "excess_deaths_update_2022/data/ihme_excess_deaths.RDS")

d_country = d %>% filter(level == 3 &
                           measure_name == "excess_deaths")
d_country = select(d_country, location_name, mean_value, lower_ci, upper_ci)
names(d_country) <- c("country", "mean_value", "lower_ci", "upper_ci")

# Remove countries removed in global analysis
d_country <- d_country[which(d_country$country != "Taiwan (province of China)"),]

d_country$country[which(d_country$country == "Iran")] <- "Iran (Islamic Republic of)"
d_country$country[which(d_country$country == "USA")] <- "United States of America"
d_country$country[which(d_country$country == "UK")] <- "England & Wales"
d_country$country[which(d_country$country == "Russia")] <- "Russian Federation"
d_country$country[which(d_country$country == "Bolivia")] <- "Bolivia (Plurinational State of)"
d_country$country[which(d_country$country == "Brunei")] <- "Brunei Darussalam"
d_country$country[which(d_country$country == "C\xf4te d'Ivoire")] <- "Cote d'Ivoire"
d_country$country[which(d_country$country == "DR Congo")] <- "Democratic Republic of the Congo"
d_country$country[which(d_country$country == "Congo (Brazzaville)")] <- "Congo"
d_country$country[which(d_country$country == "Cape Verde")] <- "Cabo Verde"
d_country$country[which(d_country$country == "Czechia")] <- "Czech Republic"
d_country$country[which(d_country$country == "The Gambia")] <- "Gambia (Republic of The)"
d_country$country[which(d_country$country == "Guinea-Bissau")] <- "Guinea Bissau"
d_country$country[which(d_country$country == "Laos")] <- "Lao People's Democratic Republic"
d_country$country[which(d_country$country == "Palestine")] <- "Occupied Palestinian Territory"
d_country$country[which(d_country$country == "S\xe3o Tom\xe9 and Pr\xedncipe")] <- "Sao Tome and Principe"
d_country$country[which(d_country$country == "Syria")] <- "Syrian Arab Republic"
d_country$country[which(d_country$country == "Tanzania")] <- "United Republic of Tanzania"
d_country$country[which(d_country$country == "Venezuela")] <- "Venezuela (Bolivarian Republic of)"
d_country$country[which(d_country$country == "Vietnam")] <- "Viet Nam"
d_country$country[which(d_country$country == "Moldova")] <- "Republic of Moldova"
d_country$country[which(d_country$country == "North Korea")] <- "Dem. People's Republic of Korea"
d_country$country[which(d_country$country == "South Korea")] <- "Republic of Korea"
d_country$country[which(d_country$country == "The Bahamas")] <- "Bahamas"
d_country$country[which(d_country$country == "Virgin Islands")] <- "United States Virgin Islands"
d_country$date = as.Date("2021/12/31")
deaths_country = d_country
deaths_country_ihme = d_country
names(deaths_country_ihme) = c("country","total","lower", "upper", "date")

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

#---------------------------------------------------------------------------------------
# Work out max excess covid for dec 2021
# Adjust JHU with study country multipliers
multipliers_study <- read.csv('excess_deaths_update_2022/data/multipliers.csv', header = FALSE)
names(multipliers_study) <- c("Country.Region", "mult_study")
remove_study = c("Brazil", "India", "Mexico", "Peru", "South Africa", "Iran (Islamic Republic of)", "Colombia", "Russian Federation")
multipliers_study = multipliers_study[!multipliers_study$Country %in% remove_study,]

deaths_country_dec =  select(deaths_country, Country.Region, X12.31.21)
deaths_country_dec = left_join(deaths_country_dec, multipliers_study, by = "Country.Region")
deaths_country_dec$X12.31.21 = ifelse(!is.na(deaths_country_dec$mult_study), 
                                      deaths_country_dec$X12.31.21 * deaths_country_dec$mult_study, 
                                      deaths_country_dec$X12.31.21)
deaths_country_dec$mult_study = NULL

#Combine 
deaths_country_dec = right_join(deaths_country_dec, deaths_country_ihme, by = c("Country.Region" = "country"))

#Work out max
deaths_country_dec$X12.31.21[is.na(deaths_country_dec$X12.31.21)] <- 0
deaths_country_dec$comb_deaths = ifelse(deaths_country_dec$X12.31.21 > deaths_country_dec$total, 
                                        deaths_country_dec$X12.31.21, deaths_country_dec$total)
deaths_country_dec$lower = ifelse(deaths_country_dec$comb_deaths == deaths_country_dec$total, 
                                  deaths_country_dec$lower, deaths_country_dec$X12.31.21)
deaths_country_dec$upper = ifelse(deaths_country_dec$comb_deaths == deaths_country_dec$total, 
                                  deaths_country_dec$upper, deaths_country_dec$X12.31.21)

# Select final columns 
deaths_country_dec = select(deaths_country_dec, Country.Region, comb_deaths, lower, upper)
deaths_country_dec$date = as.Date("2021-12-31")

#---------------------------------------------------------------------------------------
# Predict forward to April

# Work out timeseries multiplier
timeseries_multiplier =  select(deaths_country, Country.Region, X12.31.21, X5.1.22)
timeseries_multiplier$mult = timeseries_multiplier$X5.1.22 / timeseries_multiplier$X12.31.21

# Do adjusting
deaths_country_apr =  deaths_country_dec
deaths_country_apr$date = as.Date("2022-05-01")
deaths_country_apr = left_join(deaths_country_apr, timeseries_multiplier, by = "Country.Region")
deaths_country_apr$comb_deaths = deaths_country_apr$comb_deaths * deaths_country_apr$mult
deaths_country_apr$lower = deaths_country_apr$lower * deaths_country_apr$mult
deaths_country_apr$upper = deaths_country_apr$upper * deaths_country_apr$mult
deaths_country_apr = select(deaths_country_apr, -X12.31.21, -X5.1.22, -mult)
deaths_country_apr = deaths_country_apr[!is.na(deaths_country_apr$comb_deaths),]


#---------------------------------------------------------------------------------------
# Combine 2 dates
deaths_country = rbind(deaths_country_dec, deaths_country_apr)

# Join JHU with tfr data
data = readRDS("excess_deaths_update_2022/data/tfr_ihme.RDS")
data$country[which(data$country == "USA")] <- "United States of America"
data$country[which(data$country == "I.R. Iran")] <- "Iran (Islamic Republic of)"
data$sd = (data$tfr_u-data$tfr_l)/(2*1.96)
data$europe = ifelse(data$who_region == "European", 1, 0) 
data = select(data, "country", "tfr", "tfr_l",  "tfr_u", "sd", "who_region", "europe")
combined_data <- left_join(deaths_country, data, by = c("Country.Region" = "country"))
#tmp = unique(combined_data$Country.Region[is.na(combined_data$tfr)])
#print(length(tmp))
#tmp = unique(combined_data$Country.Region[is.na(combined_data$who_region)])
#print(length(tmp))

num_countries = print(sprintf("Number countries IHME: %s", length(unique(combined_data$Country.Region))))

# Calculate orphans
parents = NULL
primary = NULL
primary_secondary = NULL

dates = unique(combined_data$date)

for (i in 1:length(dates)){
  c_data <- combined_data[which(combined_data$date == dates[i]), c("Country.Region", "tfr", "tfr_l",  "tfr_u", "sd", "who_region", "europe", "comb_deaths", "lower", "upper")]
  names(c_data) <- c("country", "tfr", "tfr_l", "tfr_u", "sd",  "who_region", "europe", "total_deaths", "lower", "upper")
  orphans <- calculate_all_orphans_time_series(c_data, dates[i], uncertainty = TRUE, death_uncertainty = TRUE, 
                                               num_samples = 5000, source = "ihme")
  
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

write.csv(dat_uncertainty, "excess_deaths_update_2022/output/ihme_uncertainty_all.csv", row.names = FALSE)

print(dat_uncertainty[dat_uncertainty$country == "Global" & dat_uncertainty$date == "2021-12-31",])
print(dat_uncertainty[dat_uncertainty$country == "Global" & dat_uncertainty$date == "2022-05-01",])

write.csv(dat_uncertainty[dat_uncertainty$country == "Global",], "excess_deaths_update_2022/output/ihme_uncertainty_global.csv", row.names = FALSE)

# Checking uncertainty intervals
#reg = dat_uncertainty[dat_uncertainty$country == dat_uncertainty$region,]
#print(sum(reg$primary_secondary < reg$ps_lower & reg$primary_secondary > reg$ps_upper))
#print(sum(reg$primary < reg$p_lower & reg$primary_secondary > reg$p_upper))
#print(sum(reg$orphanhood < reg$or_lower & reg$primary_secondary > reg$or_upper))
