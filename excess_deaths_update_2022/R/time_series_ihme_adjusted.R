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

d_ihme = select(d_country, country, mean_value, lower_ci, upper_ci)
names(d_ihme) = c("country", "total", "lower", "upper")

#-------------------------------------------------------------------------------------
# Reads in JHU data
#d <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
d = readRDS("excess_deaths_update_2022/data/downloaded_timeseries_jhu.RDS")

d2 <- select(d, -Long, -Lat, -Province.State)

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

#-------------------------------------------------------------------------------------
# Creates adjustment factor JHU/ WHO

jhu = select(deaths_country, Country.Region, X12.31.21)

multipliers = right_join(jhu, d_ihme, by = c("Country.Region"="country"))
multipliers$mult = multipliers$total/multipliers$X12.31.21
multipliers$mult_lower = multipliers$lower/multipliers$X12.31.21
multipliers$mult_upper = multipliers$upper/multipliers$X12.31.21
multipliers = multipliers[!is.na(multipliers$mult),] # Remove missing countries
multipliers = multipliers[!is.infinite(multipliers$mult),] # Remove inf countries
multipliers = select(multipliers, Country.Region, mult, mult_lower, mult_upper)
write.csv(multipliers, "excess_deaths_update_2022/output/ihme_jhu_death_multiplier.csv", row.names=FALSE)
multipliers = select(multipliers, Country.Region, mult)

# Remove negative excess deaths 
multipliers = multipliers[multipliers$mult > 0,]

# Adjust deaths data
deaths_country <- right_join(deaths_country, multipliers, by = c("Country.Region"))
deaths_country[, 2:(ncol(deaths_country)-1)] <- deaths_country[,2:(ncol(deaths_country)-1)] * t(deaths_country[,ncol(deaths_country)]) 

#-------------------------------------------------------------------------------------
# Join JHU with tfr data
data = readRDS("excess_deaths_update_2022/data/tfr.RDS")
data$country[which(data$country == "USA")] <- "United States of America"
data$country[which(data$country == "I.R. Iran")] <- "Iran (Islamic Republic of)"
data$sd = (data$tfr_u-data$tfr_l)/(2*1.96)
data$europe = ifelse(data$who_region == "European", 1, 0) 
data = select(data, "country", "tfr", "tfr_l",  "tfr_u", "sd", "who_region", "europe")
combined_data <- right_join(data, deaths_country, by = c("country" = "Country.Region"))

#-------------------------------------------------------------------------------------
# Calculate orphans
dates <- names(combined_data)
dates <- dates[9:(length(dates)-1)]

parents = NULL
primary = NULL
primary_secondary = NULL

print("Calculating IHME timeseries")
for (i in 1:length(dates)){
  c_data <- combined_data[,c("country", "tfr", "tfr_l",  "tfr_u", "sd", "who_region", "europe", dates[i])]
  c_data[,dates[i]][is.na(c_data[,dates[i]])] <- 0
  names(c_data) <- c("country", "tfr", "tfr_l", "tfr_u", "sd",  "who_region", "europe", "total_deaths")
  orphans <- calculate_all_orphans_time_series(c_data, dates[i], uncertainty = FALSE, death_uncertainty = FALSE,
                                               source = "ihme")
  
  primary_secondary <- rbind(primary_secondary, orphans[[1]])
  primary <- rbind(primary, orphans[[2]])
  parents <- rbind(parents, orphans[[3]])
}

primary_secondary$country[primary_secondary$country == "England & Wales"] <- "United Kingdom"
primary_secondary$date = sub('.', '', primary_secondary$date)
primary_secondary$date <- as.Date(primary_secondary$date, format = "%m.%d.%y")

primary$country[primary$country == "England & Wales"] <- "United Kingdom"
primary$date = sub('.', '', primary$date)
primary$date <- as.Date(primary$date, format = "%m.%d.%y")

parents$country[parents$country == "England & Wales"] <- "United Kingdom"
parents$date  <- sub('.', '', parents$date)
parents$date <- as.Date(parents$date, format = "%m.%d.%y")

# Join
ps <- select(primary_secondary, country, region, date,  central)
names(ps) <- c("country", "region", "date", "primary_secondary")
p <- select(primary, country, date,  central)
names(p) <- c("country", "date", "primary")
pa <- select(parents, country, date,  central)
names(pa) <- c("country", "date", "orphanhood")

dat <- left_join(ps, p, by = c("country", "date"))
dat <- left_join(dat, pa, by = c("country", "date"))
write.csv(dat, "excess_deaths_update_2022/output/orphanhood_timeseries_no_omit_ihme_adjusted.csv", row.names=FALSE)



#-------------------------------------------------------------------------------------
# Remove countries with fewer than 25 orphans 
omit = dat$country[dat$date == max(dat$date) & dat$orphanhood < 25]
dat = dat[!dat$country %in% omit,]
dat$region = NULL

dat_all = dat
dat_all <- dat_all[order(dat_all$date),]
#print(dat_all[dat_all$country == "Global" & dat_all$date == "2021-12-31",])
#print(dat_all[dat_all$country == "Global" & dat_all$date == "2022-04-01",])

dat_all$primary_secondary = ifelse(dat_all$primary_secondary  < dat_all$primary,  dat_all$primary, dat_all$primary_secondary)
tmp = dat_all[dat_all$primary_secondary  < dat_all$primary, ]
write.csv(dat_all, "excess_deaths_update_2022/output/orphanhood_timeseries_ihme_adjusted.csv", row.names=FALSE)




