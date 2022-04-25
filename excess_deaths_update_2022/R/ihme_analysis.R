# This file produces a time series of orphans.
library(tidyverse)
source("excess_deaths_update_2022/R/shiny_table.R")

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
d2 <- select(d, Country.Region, X4.1.22)

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

multipliers = read.csv("excess_deaths_update_2022/output/ihme_jhu_death_multiplier.csv")
deaths_country = right_join(deaths_country, multipliers, by = "Country.Region")
names(deaths_country)[1] = "country"

deaths_country$total = deaths_country$X4.1.22 * deaths_country$mult
deaths_country$lower = deaths_country$X4.1.22 * deaths_country$mult_lower
deaths_country$upper = deaths_country$X4.1.22 * deaths_country$mult_upper
deaths_country$date = as.Date("2022/04/01")
deaths_country$region = NULL

deaths_country = select(deaths_country, -mult, -mult_lower, -mult_upper, -X4.1.22)
deaths_country = rbind(deaths_country_ihme, deaths_country)

# Countries with negative excess deaths
deaths_country_negative <- deaths_country[which(deaths_country$total < 0),]
write.csv(deaths_country_negative, "excess_deaths_update_2022/output/negative_excess_deaths_ihme.csv")

# Remove data which is negative
deaths_country =  deaths_country[which(deaths_country$total > 0),]

# Remove Turkmenistan
#deaths_country = deaths_country[which(!deaths_country$country %in% c("Turkmenistan", "Greenland", "Bermuda","Puerto Rico", 
#                                                                    "United States Virgin Islands", "Dem. People's Republic of Korea", "Guam",
#                                                                    "Northern Mariana Islands")),] #<---- ADD BACK IN


# Join JHU with tfr data
data = readRDS("excess_deaths_update_2022/data/tfr_ihme.RDS")
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

num_countries = print(sprintf("Number countries IHME: %s", length(unique(combined_data$country))))

# Calculate orphans
parents = NULL
primary = NULL
primary_secondary = NULL

dates = unique(combined_data$date)

# Could be re-written so just add new column on end of spreadsheet each day
for (i in 1:length(dates)){
  print(i)
  c_data <- combined_data[which(combined_data$date == dates[i]), c("country", "tfr", "tfr_l",  "tfr_u", "sd", "who_region", "europe", "total", "lower", "upper")]
  names(c_data) <- c("country", "tfr", "tfr_l", "tfr_u", "sd",  "who_region", "europe", "total_deaths", "lower", "upper")
  orphans <- calculate_all_orphans_time_series(c_data, dates[i], uncertainty = TRUE, death_uncertainty = TRUE, num_samples = 20000)
  
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
print(dat_uncertainty[dat_uncertainty$country == "Global" & dat_uncertainty$date == "2022-04-01",])

write.csv(dat_uncertainty[dat_uncertainty$country == "Global",], "excess_deaths_update_2022/output/ihme_uncertainty_global.csv", row.names = FALSE)

# Checking uncertainty intervals
reg = dat_uncertainty[dat_uncertainty$country == dat_uncertainty$region,]
print(sum(reg$primary_secondary < reg$ps_lower & reg$primary_secondary > reg$ps_upper))
print(sum(reg$primary < reg$p_lower & reg$primary_secondary > reg$p_upper))
print(sum(reg$orphanhood < reg$or_lower & reg$primary_secondary > reg$or_upper))
