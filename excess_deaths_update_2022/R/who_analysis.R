# This file produces a time series of orphans.
library(tidyverse)
library(readxl)
source("excess_deaths_update_2022/R/orphanhood_functions.R")

# Read in WHO data
d <- read_excel("excess_deaths_update_2022/data/EstimatesBySexAge_WHO.xlsx", sheet = 2)
names(d) <- as.character(d[2,])
d <- d[3:length(d$Country),]

##### Need to check which metric I should be using but doing this based on excess, Final 2021, Final 2022
d_country = d %>% filter(measure == "excess" & 
                        `source year` %in% c("Final 2020","Final 2021" )) %>% 
  mutate(sd = (uppr - lwr)/(2*1.96)) %>%
  group_by(Country,`WHO region`) %>%
  summarise(total = sum(as.numeric(mean)),
            lower = sum(as.numeric(lwr)),
            upper = sum(as.numeric(uppr)))

non_country = c("AFRO", "AMRO", "EMRO", "EURO", "Global", "HIC", "LIC", "LMIC", "SEARO", "UMIC", "WPRO")
d_country = d_country[which(! d_country$Country %in% non_country),]
names(d_country) = c("country", "region", "total", "lower", "upper")

d_country$country[which(d_country$country == "The United Kingdom")] <- "England & Wales"
d_country$country[which(d_country$country == "Côte d'Ivoire")] <- "Cote d'Ivoire"
d_country$country[which(d_country$country == "Czechia")] <- "Czech Republic"
d_country$country[which(d_country$country == "Gambia")] <- "Gambia (Republic of The)"
d_country$country[which(d_country$country == "Guinea-Bissau")] <- "Guinea Bissau"
d_country$country[which(d_country$country == "Democratic People's Republic of Korea")] <- "Dem. People's Republic of Korea"

d_country$date = as.Date("2021/12/31")
deaths_country_who = d_country

#---------------------------------------------------------------------------------------
# Read in jhu data
d <- readRDS("excess_deaths_update_2022/data/downloaded_timeseries_jhu.RDS")
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

multipliers = read.csv("excess_deaths_update_2022/output/who_jhu_death_multiplier.csv")
deaths_country = right_join(deaths_country, multipliers, by = "Country.Region")
names(deaths_country)[1] = "country"

deaths_country$total = deaths_country$X4.1.22 * deaths_country$mult
deaths_country$lower = deaths_country$X4.1.22 * deaths_country$mult_lower
deaths_country$upper = deaths_country$X4.1.22 * deaths_country$mult_upper
deaths_country$date = as.Date("2022/04/01")
deaths_country$region = NULL

deaths_country = select(deaths_country, -mult, -mult_lower, -mult_upper, -X4.1.22)
deaths_country = rbind(deaths_country_who, deaths_country)

# Countries with negative excess deaths
deaths_country_negative <- deaths_country[which(deaths_country$total < 0),]
write.csv(deaths_country_negative, "excess_deaths_update_2022/output/negative_excess_deaths_who.csv")

# Remove data which is negative
deaths_country =  deaths_country[which(deaths_country$total > 0),]

# Remove Turkmenistan to match timeseries
#deaths_country = deaths_country[which(deaths_country$country != "Turkmenistan"),] 

# Join JHU with tfr data
data = readRDS("excess_deaths_update_2022/data/tfr_who.RDS")
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

# update the "europe" column
combined_data$europe = ifelse(combined_data$who_region == "European", 1, 0)

num_countries = print(sprintf("Number countries WHO: %s", length(unique(combined_data$country))))

# Calculate orphans
parents = NULL
primary = NULL
primary_secondary = NULL

dates = unique(combined_data$date)

for (i in 1:length(dates)){
  print(dates[i])
  c_data <- combined_data[which(combined_data$date == dates[i]), 
                          c("country", "tfr", "tfr_l",  "tfr_u", "sd", "who_region", "europe", "total", "lower", "upper")]
  names(c_data) <- c("country", "tfr", "tfr_l", "tfr_u", "sd",  "who_region", "europe", "total_deaths", "lower", "upper")
  orphans <- calculate_all_orphans_time_series(c_data = c_data, date = dates[i], 
                                               uncertainty = TRUE, death_uncertainty = TRUE,
                                               num_samples = 20000, source = "who")
  
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

write.csv(dat_uncertainty, "excess_deaths_update_2022/output/who_uncertainty_all.csv", row.names = FALSE)

print(dat_uncertainty[dat_uncertainty$country == "Global" & dat_uncertainty$date == "2021-12-31",])
print(dat_uncertainty[dat_uncertainty$country == "Global" & dat_uncertainty$date == "2022-04-01",])

write.csv(dat_uncertainty[dat_uncertainty$country == "Global",], "excess_deaths_update_2022/output/who_uncertainty_global.csv", row.names = FALSE)

# Checking uncertainty intervals
#reg = dat_uncertainty[dat_uncertainty$country == dat_uncertainty$region,]
#print(sum(reg$primary_secondary < reg$ps_lower & reg$primary_secondary > reg$ps_upper))
#print(sum(reg$primary < reg$p_lower & reg$primary_secondary > reg$p_upper))
#print(sum(reg$orphanhood < reg$or_lower & reg$primary_secondary > reg$or_upper))

