# This file produces a time series of orphans.
library(tidyverse)
library(readxl)
source("excess_deaths_update_2022/R/orphanhood_functions.R")

# Read in WHO death data
d <- read_excel("excess_deaths_update_2022/data/EstimatesBySexAge_WHO.xlsx", sheet = 2)
names(d) <- as.character(d[2,])
d <- d[3:length(d$Country),]

##### Need to check which metric I should be using but doing this based on excess, Final 2021, Final 2022
d_country = d %>% filter(measure == "excess" & 
                           `source year` %in% c("Final 2020","Final 2021" )) %>% 
  group_by(Country,`WHO region`) %>%
  summarise(total = sum(as.numeric(mean)),
            lower = sum(as.numeric(lwr)))

non_country = c("AFRO", "AMRO", "EMRO", "EURO", "Global", "HIC", "LIC", "LMIC", "SEARO", "UMIC", "WPRO")
d_country = d_country[which(! d_country$Country %in% non_country),]
names(d_country) = c("country", "region", "total")

d_country$country[which(d_country$country == "The United Kingdom")] <- "England & Wales"
d_country$country[which(d_country$country == "Côte d'Ivoire")] <- "Cote d'Ivoire"
d_country$country[which(d_country$country == "Czechia")] <- "Czech Republic"
d_country$country[which(d_country$country == "Gambia")] <- "Gambia (Republic of The)"
d_country$country[which(d_country$country == "Guinea-Bissau")] <- "Guinea Bissau"
d_country$country[which(d_country$country == "Democratic People's Republic of Korea")] <- "Dem. People's Republic of Korea"

d_who = select(d_country, country, total)

print(sprintf("Total deaths WHO: %s", sum(d_who$total)))
#-------------------------------------------------------------------------------------
# Reads in JHU data
#d <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
#saveRDS(d, file = "excess_deaths_update_2022/data/downloaded_timeseries_jhu.RDS")
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

multipliers = right_join(jhu, d_who, by = c("Country.Region"="country"))
multipliers$mult = multipliers$total/multipliers$X12.31.21
multipliers = multipliers[!is.na(multipliers$mult),] # Remove missing countries
multipliers = multipliers[!is.infinite(multipliers$mult),] # Remove inf countries
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

print("Calculating who timeseries")
for (i in 1:length(dates)){
  c_data <- combined_data[,c("country", "tfr", "tfr_l",  "tfr_u", "sd", "who_region", "europe", dates[i])]
  c_data[,dates[i]][is.na(c_data[,dates[i]])] <- 0
  names(c_data) <- c("country", "tfr", "tfr_l", "tfr_u", "sd",  "who_region", "europe", "total_deaths")
  orphans <- calculate_all_orphans_time_series(c_data, dates[i], uncertainty = FALSE, death_uncertainty = FALSE, source = "who")
  
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
write.csv(dat, "excess_deaths_update_2022/output/orphanhood_timeseries_no_omit_who_adjusted.csv", row.names=FALSE)

#-------------------------------------------------------------------------------------
# Remove countries with fewer than 25 orphans 
omit = dat$country[dat$date == max(dat$date) & dat$orphanhood < 25]
dat = dat[!dat$country %in% omit,]
dat$region = NULL

dat_all = dat
dat_all <- dat_all[order(dat_all$date),]
print(dat_all[dat_all$country == "Global" & dat_all$date == "2021-12-31",])
print(dat_all[dat_all$country == "Global" & dat_all$date == "2022-04-01",])

dat_all$primary_secondary = ifelse(dat_all$primary_secondary  < dat_all$primary,  dat_all$primary, dat_all$primary_secondary)
write.csv(dat_all, "excess_deaths_update_2022/output/orphanhood_timeseries_who_adjusted.csv", row.names=FALSE)

